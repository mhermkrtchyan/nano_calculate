BeginPackage["Tool`Optics`"];
Begin["`Private`"]

(*
	Get constants
*)

Needs["Tool`Constants`"];

ClearAll[$$BoltzmannConstantSI, $$VacuumPremittivitySI, $$PlanckConstantSI, $$ElectronChargeSI, $$SpeedOfLightSI];
$$BoltzmannConstantSI      = Tool`Constants`Private`$$BoltzmannConstantSI;
$$VacuumPremittivitySI     = Tool`Constants`Private`$$VacuumPremittivitySI;
$$PlanckConstantSI         = Tool`Constants`Private`$$PlanckConstantSI;
$$ElectronChargeSI	       = Tool`Constants`Private`$$ElectronChargeSI;
$$SpeedOfLightSI           = Tool`Constants`Private`$$SpeedOfLightSI;

(*
	Get helpers
*)

Needs["Tool`Helpers`"];

ClearAll[$$FailureFunctionSignature, $$FailureQuantumNumber];
$$FailureFunctionSignature = Tool`Helpers`Private`$FailureFunctionSignature;
$$FailureQuantumNumber     = Tool`Helpers`Private`$FailureQuantumNumber; 

ClearAll[$JouleToEV, GaussDistribution];
$JouleToEV                  = Tool`Helpers`Private`$JouleToEV;
GaussDistribution          = Tool`Helpers`Private`$GaussDistribution;

(*
	Get semiconductor parameters
*)

Needs["Tool`Semiconductors`"];

ClearAll[$EffectiveMass, $BohrRadius, $RydbergEnergy, $DielectricConstant, $GapEnergy];
$EffectiveMass              = Tool`Semiconductors`Private`$EffectiveMass;
$BohrRadius        		    = Tool`Semiconductors`Private`$BohrRadius;
$RydbergEnergy              = Tool`Semiconductors`Private`$RydbergEnergy;
$DielectricConstant         = Tool`Semiconductors`Private`$DielectricConstant;
$GapEnergy                  = Tool`Semiconductors`Private`$GapEnergy;

(*
	Get eigensystems
*)

Needs["Tool`Eigensystems`"];

ClearAll[StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D];
StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D = Tool`Eigensystems`Private`StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D;

(*
	Interband Transitions
*)

ClearAll[InterbandAbsorptionCoefficient];
InterbandAbsorptionCoefficient[InitialState_, FinalState_, Hole_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI = QuantityMagnitude @ $$BoltzmannConstantSI,
			EffectiveMass 		= QuantityMagnitude @ $EffectiveMass[InitialState["Semiconductor"], #] &,
			GapEnergy 			= QuantityMagnitude @ $GapEnergy[InitialState["Semiconductor"], temperature],
			BohrRadius 			= QuantityMagnitude @ $BohrRadius[InitialState["Semiconductor"], #] &
			,
			ElectronModel, HoleModel, ElectronWaveFunction, HoleWaveFunction, ElectronEnergy, HoleEnergy,
			MatrixElement, DeltaEnergy, Chemicalpotential, FermiDirac, Linewidth, AbsorptionCoefficient
		},

		HoleModel = InitialState[Hole];
		HoleWaveFunction = ReplaceAll[
			HoleModel["Axial", "WaveFunction"] * HoleModel["Radial", "WaveFunction"],
			Global`r -> Max[InitialState["Geometry", "Radial"]] * BohrRadius[Hole]
		];
		HoleEnergy = HoleModel["Axial", "Energy"] + HoleModel["Radial", "Energy"];

		ElectronModel = FinalState["Electron"];
		ElectronWaveFunction = ReplaceAll[
			ElectronModel["Axial", "WaveFunction"] * ElectronModel["Radial", "WaveFunction"],
			Global`r -> Max[FinalState["Geometry", "Radial"]] * BohrRadius["Electron"]
		];
		ElectronEnergy = ElectronModel["Axial", "Energy"] + ElectronModel["Radial", "Energy"];

		MatrixElement = Abs @ Integrate[
			2*Pi * HoleWaveFunction * ElectronWaveFunction,
			Prepend[
				InitialState["Geometry", "Axial"],
				Global`z
			]
		];

		DeltaEnergy = (ElectronEnergy + HoleEnergy + GapEnergy)  * 10^3;
		
		Chemicalpotential = - (GapEnergy/2) + (3/4) * $JouleToEV[BoltzmannConstantSI * temperature] * Log[EffectiveMass["Heavy Hole"] / EffectiveMass["Electron"]];
		FermiDirac = 1 / (1 + Exp[(# - Chemicalpotential) / $JouleToEV[BoltzmannConstantSI * temperature]]) &;
		
		Linewidth = 0.1 + 0.0013487663304156054 * temperature + 0.00004994855667640969 * temperature^2 ;
		
		With[
			{
				LightEnergy = Global`LightEnergy
			},

			AbsorptionCoefficient = Times[
				FermiDirac[ElectronEnergy] * (1 - FermiDirac[HoleEnergy]),
				LightEnergy * Linewidth / ((LightEnergy - DeltaEnergy)^2 + Linewidth^2),
				MatrixElement^2
			]
		]
	];
InterbandAbsorptionCoefficient[___] := $$FailureFunctionSignature["Dependencies`Private`InterbandAbsorptionCoefficient"];

ClearAll[InterbandAbsorptionEdge];
InterbandAbsorptionEdge[InitialState_, FinalState_, Hole_, temperature_] :=
	Catch @ Block[
		{
			GapEnergy = QuantityMagnitude @ $GapEnergy[InitialState["Semiconductor"], temperature]
			,
			HoleModel, HoleEnergy, ElectronModel, ElectronEnergy
		},

		HoleModel = InitialState[Hole];
		HoleEnergy = HoleModel["Axial", "Energy"] + HoleModel["Radial", "Energy"];

		ElectronModel = FinalState["Electron"];
		ElectronEnergy = ElectronModel["Axial", "Energy"] + ElectronModel["Radial", "Energy"];
		
		ElectronEnergy + HoleEnergy + GapEnergy
	];
InterbandAbsorptionEdge[___] := $$FailureFunctionSignature["Dependencies`Private`InterbandAbsorptionEdge"];

ClearAll[PhotoluminescenceCoefficient];
PhotoluminescenceCoefficient[InitialState_, FinalState_, Hole_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI = QuantityMagnitude @ $$BoltzmannConstantSI,
			Mass 				= QuantityMagnitude @ EffectiveMass[InitialState["Semiconductor"], #] &,
			Gap 				= QuantityMagnitude @ $GapEnergy[InitialState["Semiconductor"], temperature]
		},

		absorption = 
			Simplify @ Plus @@ Map[
				InterbandAbsorptionCoefficient[InitialState, FinalState, Hole, temperature]
			];

		Times[
			LightEnergy * absorption,
			Exp[-(LightEnergy - 10^3 * Gap)/(10^3 * $JouleToEV[BoltzmannConstantSI * temperature])],
			Exp[(0.5 - Gap)/($JouleToEV[BoltzmannConstantSI * temperature])]
		]
	];
PhotoluminescenceCoefficient[___] := $$FailureFunctionSignature["Dependencies`Private`PhotoluminescenceCoefficient"];

(*
	Intraband Transitions
*)

ClearAll[IntrabandAbsorptionCoefficient];
IntrabandAbsorptionCoefficient[Model_, ElectricField_, MagneticField_, InitialState_, FinalState_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI 	= QuantityMagnitude @ $$BoltzmannConstantSI,
			PlanckConstantSI 		= QuantityMagnitude @ $$PlanckConstantSI,
			VacuumPremittivitySI 	= QuantityMagnitude @ $$VacuumPremittivitySI,
			ElectronChargeSI 		= QuantityMagnitude @ $$ElectronChargeSI,
			SpeedOfLightSI 			= QuantityMagnitude @ $$SpeedOfLightSI,
			Radius 					= QuantityMagnitude @ BohrRadius[Model["Parameters", "Semiconductor"], #] &,
			Mass 					= QuantityMagnitude @ EffectiveMass[Model["Parameters", "Semiconductor"], #] &,
			Gap 					= QuantityMagnitude @ $GapEnergy[Model["Parameters", "Semiconductor"]]
			,
			Intensity = 10^8, ElectronsPopulation = 3 * 10^22, InfraredRefractiveIndex = 3.51
			,
			Electron1Model, Electron2Model, Electron1WaveFunction, Electron2WaveFunction, Electron1Energy, Electron2Energy,
			MatrixElement12, MatrixElement11, MatrixElement22, DeltaEnergy, Chemicalpotential, FermiDirac, Linewidth,
			LinearConstant, NonLinearConstant, LinearAbsorptionCoefficient, NonLinearAbsorptionCoefficient
		},

		Electron1Model = Model[{ElectricField, MagneticField}][{MagneticFieldElectron1, RadialNumberElectron1, AxialNumberElectron1}]["Electron"];
		Electron1WaveFunction = Last@Electron1Model["Axial"] * Last@Electron1Model["Radial"] /. Eigensystems`Private`r -> 1;
		Electron1Energy = First@Electron1Model["Axial"] + First@Electron1Model["Radial"];

		Electron2Model = Model[{ElectricField, MagneticField}][{MagneticFieldElectron2, RadialNumberElectron2, AxialNumberElectron2}]["Electron"];
		Electron2WaveFunction = Last@Electron2Model["Axial"] * Last@Electron2Model["Radial"] /. Eigensystems`Private`r -> 1;
		Electron2Energy = First@Electron2Model["Axial"] + First@Electron2Model["Radial"];
		
		MatrixElement12 = Abs @ Integrate[
			Electron1WaveFunction * Eigensystems`Private`z * Electron2WaveFunction,
			{
				Eigensystems`Private`z,
				- Min @ Model["Parameters", "Sizes", "Axial"],
				  Max @ Model["Parameters", "Sizes", "Axial"]
			}
			,
			{
				Eigensystems`Private`angle,
				0,
				2 * Pi
			}
		];
		MatrixElement11 = Abs @ Integrate[
			Electron1WaveFunction * Eigensystems`Private`z * Electron1WaveFunction,
			{
				Eigensystems`Private`z,
				- Min @ Model["Parameters", "Sizes", "Axial"],
				  Max @ Model["Parameters", "Sizes", "Axial"]
			}
			,
			{
				Eigensystems`Private`angle,
				0,
				2 * Pi
			}
		];
		MatrixElement22 = Abs @ Integrate[
			Electron2WaveFunction * Eigensystems`Private`z * Electron2WaveFunction,
			{
				Eigensystems`Private`z,
				- Min @ Model["Parameters", "Sizes", "Axial"],
				  Max @ Model["Parameters", "Sizes", "Axial"]
			}
			,
			{
				Eigensystems`Private`angle,
				0,
				2 * Pi
			}
		];

		DeltaEnergy = Abs[Electron2Energy - Electron1Energy]  * 10^3;
		
		Chemicalpotential = - (Gap/2) + (3/4) * $JouleToEV[BoltzmannConstantSI * temperature] * Log[Mass["Heavy Hole"] / Mass["Electron"]];
		FermiDirac = 1 / (1 + Exp[(# - Chemicalpotential) / $JouleToEV[BoltzmannConstantSI * temperature]]) &;
		Linewidth = 0.1 + 0.0013487663304156054 * temperature + 0.00004994855667640969 * temperature^2 ;
		
		LinearConstant = Divide[
			Radius["Electron"]^2 * ElectronChargeSI^2 * ElectronsPopulation,
			PlanckConstantSI * VacuumPremittivitySI * DielectricConstant[Model["Parameters", "Semiconductor"]] * SpeedOfLightSI
		];

		LinearAbsorptionCoefficient = Times[
			LinearConstant * FermiDirac[Electron1Energy] * (1 - FermiDirac[Electron2Energy]),
			LightEnergy * Linewidth *  MatrixElement12^2 / ((DeltaEnergy - LightEnergy)^2 + Linewidth^2)
		];

		NonLinearConstant = 10^6 * Nest[
			$JouleToEV,
			Divide[
				- Radius["Electron"]^4 * ElectronChargeSI^4 * 2*ElectronsPopulation * Intensity,
				PlanckConstantSI * VacuumPremittivitySI^2 * DielectricConstant[Model["Parameters", "Semiconductor"]] * SpeedOfLightSI^2 * InfraredRefractiveIndex
			],
			2
		];

		NonLinearAbsorptionCoefficient = Times[
			NonLinearConstant * FermiDirac[Electron1Energy] * (1 - FermiDirac[Electron2Energy]),
			LightEnergy * Linewidth * MatrixElement12^4 / ((DeltaEnergy - LightEnergy)^2 + Linewidth^2)^2,
			1 - ((MatrixElement22 - MatrixElement11)^2/(4 Abs[MatrixElement12]^2))*((3 DeltaEnergy^2 - 4*LightEnergy*DeltaEnergy + LightEnergy^2 - Linewidth^2)/(DeltaEnergy^2 + Linewidth^2))
		];

		TotalAbsorptionCoefficient = Simplify[LinearAbsorptionCoefficient + NonLinearAbsorptionCoefficient];

		{LinearAbsorptionCoefficient, NonLinearAbsorptionCoefficient, TotalAbsorptionCoefficient}
	];
IntrabandAbsorptionCoefficient[___] := $$FailureFunctionSignature["Dependencies`Private`IntrabandAbsorptionCoefficient"];

ClearAll[SecondHarmonicGeneration];
SecondHarmonicGeneration[Model_, ElectricField_, MagneticField_, NumbersElectron1_, NumbersElectron2_, NumbersElectron3_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI = QuantityMagnitude @ $$BoltzmannConstantSI,
			VacuumPremittivitySI = QuantityMagnitude @ $$VacuumPremittivitySI, ElectronChargeSI = QuantityMagnitude @ $$ElectronChargeSI,
			ElectronsPopulation = 3 * 10^22,
			Radius = QuantityMagnitude @ BohrRadius[Model["Parameters", "Semiconductor"], #] &,
			Mass = QuantityMagnitude @ EffectiveMass[Model["Parameters", "Semiconductor"], #] &,
			Gap = QuantityMagnitude @ $GapEnergy[Model["Parameters", "Semiconductor"]],
			Electron1Model, Electron1WaveFunction, Electron1Energy,
			Electron2Model, Electron2WaveFunction, Electron2Energy,
			Electron3Model, Electron3WaveFunction, Electron3Energy,
			MatrixElement12, MatrixElement13, MatrixElement23, DeltaEnergy12, DeltaEnergy13,
			Chemicalpotential, FermiDirac, Linewidth, shg
		},

		{MagneticFieldElectron1, RadialNumberElectron1, AxialNumberElectron1} = NumbersElectron1;
		{MagneticFieldElectron2, RadialNumberElectron2, AxialNumberElectron2} = NumbersElectron2;
		{MagneticFieldElectron3, RadialNumberElectron3, AxialNumberElectron3} = NumbersElectron3;


		Electron1Model = Model[{ElectricField, MagneticField}][{MagneticFieldElectron1, RadialNumberElectron1, AxialNumberElectron1}]["Electron"];
		Electron1WaveFunction = Last@Electron1Model["Axial"] * Last@Electron1Model["Radial"] /. Eigensystems`Private`r -> 1;
		Electron1Energy = First@Electron1Model["Axial"] + First@Electron1Model["Radial"];

		Electron2Model = Model[{ElectricField, MagneticField}][{MagneticFieldElectron2, RadialNumberElectron2, AxialNumberElectron2}]["Electron"];
		Electron2WaveFunction = Last@Electron2Model["Axial"] * Last@Electron2Model["Radial"] /. Eigensystems`Private`r -> 1;
		Electron2Energy = First@Electron2Model["Axial"] + First@Electron2Model["Radial"];

		Electron3Model = Model[{ElectricField, MagneticField}][{MagneticFieldElectron3, RadialNumberElectron3, AxialNumberElectron3}]["Electron"];
		Electron3WaveFunction = Last@Electron3Model["Axial"] * Last@Electron3Model["Radial"] /. Eigensystems`Private`r -> 1;
		Electron3Energy = First@Electron3Model["Axial"] + First@Electron3Model["Radial"];
		
		MatrixElement12 = Abs @ Integrate[
			Electron1WaveFunction * Eigensystems`Private`z * Electron2WaveFunction,
			{
				Eigensystems`Private`z,
				- Min @ Model["Parameters", "Sizes", "Axial"],
				  Max @ Model["Parameters", "Sizes", "Axial"]
			}
			,
			{
				Eigensystems`Private`angle,
				0,
				2 * Pi
			}
		];
		MatrixElement13 = Abs @ Integrate[
			Electron1WaveFunction * Eigensystems`Private`z * Electron3WaveFunction,
			{
				Eigensystems`Private`z,
				- Min @ Model["Parameters", "Sizes", "Axial"],
				  Max @ Model["Parameters", "Sizes", "Axial"]
			}
			,
			{
				Eigensystems`Private`angle,
				0,
				2 * Pi
			}
		];
		MatrixElement23 = Abs @ Integrate[
			Electron2WaveFunction * Eigensystems`Private`z * Electron3WaveFunction,
			{
				Eigensystems`Private`z,
				- Min @ Model["Parameters", "Sizes", "Axial"],
				  Max @ Model["Parameters", "Sizes", "Axial"]
			}
			,
			{
				Eigensystems`Private`angle,
				0,
				2 * Pi
			}
		];

		DeltaEnergy12 = Abs[Electron2Energy - Electron1Energy]  * 10^3;
		DeltaEnergy13 = Abs[Electron3Energy - Electron1Energy]  * 10^3;
		
		Chemicalpotential = - (Gap/2) + (3/4) * $JouleToEV[BoltzmannConstantSI * temperature] * Log[Mass["Heavy Hole"] / Mass["Electron"]];
		FermiDirac = 1 / (1 + Exp[(# - Chemicalpotential) / $JouleToEV[BoltzmannConstantSI * temperature]]) &;
		Linewidth = 0.1 + 0.0013487663304156054 * temperature + 0.00004994855667640969 * temperature^2 ;
		
		LinearConstant = 10^6 * Nest[$JouleToEV, Radius["Electron"]^3 * ElectronChargeSI^3 * ElectronsPopulation / VacuumPremittivitySI, 2];

		shg = Times[
			LinearConstant * FermiDirac[Electron1Energy] * (1 - FermiDirac[Electron2Energy]) * (1 - FermiDirac[Electron3Energy]),
			Abs[MatrixElement12 * MatrixElement13 *  MatrixElement23 / ((LightEnergy - DeltaEnergy12 - I*Linewidth) * (2*LightEnergy - DeltaEnergy13 - I*Linewidth))]
		]
	];
SecondHarmonicGeneration[___] := $$FailureFunctionSignature["Dependencies`Private`SecondHarmonicGeneration"];

ClearAll[ThirdHarmonicGeneration];
ThirdHarmonicGeneration[Model_, ElectricField_, MagneticField_, NumbersElectron1_, NumbersElectron2_, NumbersElectron3_, NumbersElectron4_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI = QuantityMagnitude @ $$BoltzmannConstantSI,
			VacuumPremittivitySI = QuantityMagnitude @ $$VacuumPremittivitySI, ElectronChargeSI = QuantityMagnitude @ $$ElectronChargeSI,
			ElectronsPopulation = 3 * 10^22,
			Radius = QuantityMagnitude @ BohrRadius[Model["Parameters", "Semiconductor"], #] &,
			Mass = QuantityMagnitude @ EffectiveMass[Model["Parameters", "Semiconductor"], #] &,
			Gap = QuantityMagnitude @ $GapEnergy[Model["Parameters", "Semiconductor"]],
			Electron1Model, Electron1WaveFunction, Electron1Energy,
			Electron2Model, Electron2WaveFunction, Electron2Energy,
			Electron3Model, Electron3WaveFunction, Electron3Energy,
			Electron4Model, Electron4WaveFunction, Electron4Energy,
			MatrixElement12, MatrixElement33, MatrixElement34, MatrixElement14, DeltaEnergy12, DeltaEnergy13, DeltaEnergy14,
			Chemicalpotential, FermiDirac, Linewidth, thg
		},

		{MagneticFieldElectron1, RadialNumberElectron1, AxialNumberElectron1} = NumbersElectron1;
		{MagneticFieldElectron2, RadialNumberElectron2, AxialNumberElectron2} = NumbersElectron2;
		{MagneticFieldElectron3, RadialNumberElectron3, AxialNumberElectron3} = NumbersElectron3;
		{MagneticFieldElectron4, RadialNumberElectron4, AxialNumberElectron4} = NumbersElectron4;


		Electron1Model = Model[{ElectricField, MagneticField}][{MagneticFieldElectron1, RadialNumberElectron1, AxialNumberElectron1}]["Electron"];
		Electron1WaveFunction = Last@Electron1Model["Axial"] * Last@Electron1Model["Radial"] /. Eigensystems`Private`r -> 1;
		Electron1Energy = First@Electron1Model["Axial"] + First@Electron1Model["Radial"];

		Electron2Model = Model[{ElectricField, MagneticField}][{MagneticFieldElectron2, RadialNumberElectron2, AxialNumberElectron2}]["Electron"];
		Electron2WaveFunction = Last@Electron2Model["Axial"] * Last@Electron2Model["Radial"] /. Eigensystems`Private`r -> 1;
		Electron2Energy = First@Electron2Model["Axial"] + First@Electron2Model["Radial"];

		Electron3Model = Model[{ElectricField, MagneticField}][{MagneticFieldElectron3, RadialNumberElectron3, AxialNumberElectron3}]["Electron"];
		Electron3WaveFunction = Last@Electron3Model["Axial"] * Last@Electron3Model["Radial"] /. Eigensystems`Private`r -> 1;
		Electron3Energy = First@Electron3Model["Axial"] + First@Electron3Model["Radial"];

		Electron4Model = Model[{ElectricField, MagneticField}][{MagneticFieldElectron4, RadialNumberElectron4, AxialNumberElectron4}]["Electron"];
		Electron4WaveFunction = Last@Electron4Model["Axial"] * Last@Electron4Model["Radial"] /. Eigensystems`Private`r -> 1;
		Electron4Energy = First@Electron4Model["Axial"] + First@Electron4Model["Radial"];
		
		MatrixElement12 = Abs @ Integrate[
			Electron1WaveFunction * Eigensystems`Private`z * Electron2WaveFunction,
			{
				Eigensystems`Private`z,
				- Min @ Model["Parameters", "Sizes", "Axial"],
				  Max @ Model["Parameters", "Sizes", "Axial"]
			}
			,
			{
				Eigensystems`Private`angle,
				0,
				2 * Pi
			}
		];
		MatrixElement23 = Abs @ Integrate[
			Electron2WaveFunction * Eigensystems`Private`z * Electron3WaveFunction,
			{
				Eigensystems`Private`z,
				- Min @ Model["Parameters", "Sizes", "Axial"],
				  Max @ Model["Parameters", "Sizes", "Axial"]
			}
			,
			{
				Eigensystems`Private`angle,
				0,
				2 * Pi
			}
		];
		MatrixElement34 = Abs @ Integrate[
			Electron3WaveFunction * Eigensystems`Private`z * Electron4WaveFunction,
			{
				Eigensystems`Private`z,
				- Min @ Model["Parameters", "Sizes", "Axial"],
				  Max @ Model["Parameters", "Sizes", "Axial"]
			}
			,
			{
				Eigensystems`Private`angle,
				0,
				2 * Pi
			}
		];
		MatrixElement14 = Abs @ Integrate[
			Electron1WaveFunction * Eigensystems`Private`z * Electron4WaveFunction,
			{
				Eigensystems`Private`z,
				- Min @ Model["Parameters", "Sizes", "Axial"],
				  Max @ Model["Parameters", "Sizes", "Axial"]
			}
			,
			{
				Eigensystems`Private`angle,
				0,
				2 * Pi
			}
		];

		DeltaEnergy12 = Abs[Electron2Energy - Electron1Energy]  * 10^3;
		DeltaEnergy13 = Abs[Electron3Energy - Electron1Energy]  * 10^3;
		DeltaEnergy14 = Abs[Electron4Energy - Electron1Energy]  * 10^3;

				
		Chemicalpotential = - (Gap/2) + (3/4) * $JouleToEV[BoltzmannConstantSI * temperature] * Log[Mass["Heavy Hole"] / Mass["Electron"]];
		FermiDirac = 1 / (1 + Exp[(# - Chemicalpotential) / $JouleToEV[BoltzmannConstantSI * temperature]]) &;
		Linewidth = 0.1 + 0.0013487663304156054 * temperature + 0.00004994855667640969 * temperature^2 ;
		
		LinearConstant = 10^9 * Nest[$JouleToEV, Radius["Electron"]^4 * ElectronChargeSI^4 * ElectronsPopulation / VacuumPremittivitySI, 3];
		
		thg = Times[
			LinearConstant * FermiDirac[Electron1Energy] * (1 - FermiDirac[Electron2Energy]) * (1 - FermiDirac[Electron3Energy]) * (1 - FermiDirac[Electron4Energy]),
			Abs @ Divide[
				MatrixElement12 * MatrixElement23 *  MatrixElement34 * MatrixElement14,
				(LightEnergy - DeltaEnergy12 - I*Linewidth) * (2*LightEnergy - DeltaEnergy13 - I*Linewidth) * (3*LightEnergy - DeltaEnergy14 - I*Linewidth)
			]
		]
	];
ThirdHarmonicGeneration[___] := $$FailureFunctionSignature["Dependencies`Private`ThirdHarmonicGeneration"];

(*
	Ensemble Effects
*)

ClearAll[GetSPCQDRaduisDependency];
GetSPCQDRaduisDependency[pMaterial_, pHole_, eField_, {meQN_, reQN_, aeQN_, mhQN_, rhQN_, ahQN_}] :=
	Block[
		{
			getAxialDependency, axialDependency, getRadialDependency, radialDependency, fullEnergyDependency,
			numberHole 
		},

		numberHole =If[pHole == "Light Hole",
			2,
			3
		];

		getAxialDependency =
			{
				#,
				Plus[
					QuantityMagnitude @ StronglyProlateConicalQuantumDotEigensystem[
						pMaterial,
						#,
						10 * #,
						eField,
						meQN,
						reQN,
						aeQN
					][[1, 1, 1]]
					,
					QuantityMagnitude @ StronglyProlateConicalQuantumDotEigensystem[
						pMaterial,
						#,
						10 * #,
						eField,
						mhQN,
						rhQN,
						ahQN
					][[1, numberHole, 1]]
				]
			} & /@ Range[10];

		axialDependency =
			Normal[
				LinearModelFit[
					getAxialDependency,
					1/R^2,
					R
				]
			];
		
		getRadialDependency = 
			{
				#
				,
				Plus[
					QuantityMagnitude @ StronglyProlateConicalQuantumDotEigensystem[
						pMaterial,
						#,
						10 * #,
						eField,
						mhQN,
						rhQN,
						ahQN
					][[2, 1, 1]]
					,
					QuantityMagnitude @ StronglyProlateConicalQuantumDotEigensystem[
						pMaterial,
						#,
						10 * #,
						eField,
						mhQN,
						rhQN,
						ahQN
					][[2, numberHole, 1]]	
				]
			}& /@ Range[10];

		radialDependency =
			Normal[
				LinearModelFit[
					getRadialDependency,
					1/R^2,
					R
				]
			];

		fullEnergyDependency = 
			Simplify[
				axialDependency + radialDependency
			]

	];
GetSPCQDRaduisDependency[___] := $$FailureFunctionSignature["Dependencies`Private`GetSPCQDRaduisDependency"];

ClearAll[GetSOCQDRaduisDependency];
GetSOCQDRaduisDependency[pMaterial_, pHole_, eField_, {meQN_, reQN_, aeQN_, mhQN_, rhQN_, ahQN_}]:=
	Block[
		{
			getRadialDependency, radialDependency, getAxialDependency, axialDependency, fullEnergyDependency,
			numberHole
		},

		numberHole =If[pHole == "Light Hole",
			2,
			3
		];
		
		getRadialDependency =
			{
				#,
				Plus[
					QuantityMagnitude @ StronglyOblateConicalQuantumDotEigensystem[
						pMaterial,
						10 * #,
						#,
						eField,
						meQN,
						reQN,
						aeQN
					][[1, 1, 1]]
					,
					QuantityMagnitude @ StronglyOblateConicalQuantumDotEigensystem[
						pMaterial,
						10 * #,
						#,
						eField,
						mhQN,
						rhQN,
						ahQN
					][[1, numberHole, 1]]
				]
			} & /@ Range[10];

		radialDependency =
			Normal[
				LinearModelFit[
					getRadialDependency,
					1/R^2,
					R
				]
			];
		
		getAxialDependency = 
			{
				#
				,
				Plus[
					QuantityMagnitude @ StronglyOblateConicalQuantumDotEigensystem[
						pMaterial,
						10 * #,
						#,
						eField,
						mhQN,
						rhQN,
						ahQN
					][[2, 1, 1]]
					,
					QuantityMagnitude @ StronglyOblateConicalQuantumDotEigensystem[
						pMaterial,
						10 * #,
						#,
						eField,
						mhQN,
						rhQN,
						ahQN
					][[2, numberHole, 1]]	
				]
			}  & /@ Range[10];
		
		axialDependency =
			Normal[
				LinearModelFit[
					getAxialDependency,
					1/R^2,
					R
				]
			];

		fullEnergyDependency = 
			Simplify[
				radialDependency + axialDependency
			]

	];
GetSOCQDRaduisDependency[___] := $$FailureFunctionSignature["Dependencies`Private`GetSOCQDRaduisDependency"];

ClearAll[EnsembleEffectForOneTransition];
EnsembleEffectForOneTransition[pMaterial_, Model_, pHole_, eField_, LightFrequencyGap_, {meQN_, reQN_, aeQN_, mhQN_, rhQN_, ahQN_}] :=
	Block[
		{
			EnergyRadiusDependency,
			EnergyRadiusDependencyCoefficient1, EnergyRadiusDependencyCoefficient2, EnergyRadiusDependencyCoefficient,
			gauss, shift, intergrate, absorption, numberHole
		},

		numberHole = If[pHole == "Light Hole",
			2,
			3
		];

		EnergyRadiusDependency = 
			If[Model == "SPCQD",
				GetSPCQDRaduisDependency[pMaterial, pHole, eField, {meQN, reQN, aeQN, mhQN, rhQN, ahQN}],
				GetSOCQDRaduisDependency[pMaterial, pHole, eField, {meQN, reQN, aeQN, mhQN, rhQN, ahQN}]
			];
	
		EnergyRadiusDependencyCoefficient1 = First @ EnergyRadiusDependency;
		EnergyRadiusDependencyCoefficient2 = Numerator @ Last @ EnergyRadiusDependency;
		EnergyRadiusDependencyCoefficient  = (LightFrequencyGap - EnergyRadiusDependencyCoefficient1) / EnergyRadiusDependencyCoefficient2;
	
		gauss = 
			GaussDistribution[
				EnergyRadiusDependencyCoefficient^-(1/2)
			];

		shift = EnergyRadiusDependencyCoefficient^(-3/2);
	
		intergrate = 
			NIntegrate[
				Times[
					StronglyProlateConicalQuantumDotEigensystem[
						pMaterial,
						5,
						0.5,
						eField,
						meQN,
						reQN,
						aeQN
					][[1, 1, 2]],
					StronglyProlateConicalQuantumDotEigensystem[
						pMaterial,
						5,
						0.5,
						eField,
						meQN,
						reQN,
						aeQN
					][[1, numberHole, 2]]
				],
				{Tool`Eigensystems`Private`z, 0, 0.5},
				{Tool`Eigensystems`Private`r, 0, 0.5}
			];
	
		absorption = intergrate * gauss * shift / (2 * EnergyRadiusDependencyCoefficient2)
	]
EnsembleEffectForOneTransition[___] := $$FailureFunctionSignature["Dependencies`Private`EnsembleEffectForOneTransition"];

ClearAll[EnsembleEffectForAllTransitions];
EnsembleEffectForAllTransitions[pMaterial_, Model_, pHole_, eField_, LightFrequencyGap_] :=
	Block[
		{
			oneTransition = EnsembleEffectForOneTransition[pMaterial, Model, pHole, eField, LightFrequencyGap, #] &,
			PossibleQNValues = Tuples[{{-1, 0, 1}, {1, 2}, {0, 1, 2}}], allQNValues
		},
		allQNValues = 
			Map[
				Flatten[
					Append[
						PossibleQNValues[[#]],
						PossibleQNValues[[#]]
					]
				] &,
				Range[Length @ PossibleQNValues]
			];
		
		Simplify[
			Plus @@ (oneTransition /@ allQNValues)
		]	
	];	
EnsembleEffectForAllTransitions[___] := $$FailureFunctionSignature["Dependencies`Private`EnsembleEffectForAllTransitions"];

End[];
EndPackage[];