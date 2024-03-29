BeginPackage["Tool`Optics`"];

Block[{$ContextPath}, Needs["GeneralUtilities`"];];

ClearAll[InterbandAbsorptionCoefficient];
GeneralUtilities`SetUsage[InterbandAbsorptionCoefficient,
"InterbandAbsorptionCoefficient[InitialState$, FinalState$, Hole$, temperature$]
    This function gives programmatically acses to interband transition absorption coefficient light energy dependency."
];

ClearAll[PhotoluminescenceCoefficient];
GeneralUtilities`SetUsage[PhotoluminescenceCoefficient,
"PhotoluminescenceCoefficient[InitialState$, FinalState$, Hole$, temperature$]
    This function gives programmatically acses to interband transition photoluminescence."
];

ClearAll[IntrabandAbsorptionCoefficient];
GeneralUtilities`SetUsage[IntrabandAbsorptionCoefficient,
"IntrabandAbsorptionCoefficient[InitialState$, FinalState$, Particle$, temperature$]
    This function gives programmatically acses to intraband transition absorption coefficient light energy dependency."
];

ClearAll[ReflectiveIndexChange];
GeneralUtilities`SetUsage[ReflectiveIndexChange,
"ReflectiveIndexChange[InitialState$, FinalState$, Particle$, temperature$]
    This function gives programmatically acses to reflective index change coefficient light energy dependency."
];

ClearAll[SecondHarmonicGeneration];
GeneralUtilities`SetUsage[SecondHarmonicGeneration,
"SecondHarmonicGeneration[State1$, State2$, State3$, Particle$, temperature$]
    This function gives programmatically acses to three-level system second harmonic generation coefficient light energy dependency."
];

ClearAll[ThirdHarmonicGeneration];
GeneralUtilities`SetUsage[ThirdHarmonicGeneration,
"ThirdHarmonicGeneration[State1$, State2$, State3$, State4$, Particle$, temperature$]
    This function gives programmatically acses to four-level system third harmonic generation coefficient light energy dependency."
];

ClearAll[PhotoionisationCrossSection];
GeneralUtilities`SetUsage[PhotoionisationCrossSection,
"PhotoionisationCrossSection[InitialState$, FinalState$, Hole$, temperature$]
    This function gives programmatically acses to photoionisation cross section from light energy dependency."
];

Begin["`Private`"]

(* Get Constants *)
Needs["Tool`Constants`"];
ClearAll[$$BoltzmannConstantSI, $$VacuumPremittivitySI, $$PlanckConstantSI, $$ElectronChargeSI, $$SpeedOfLightSI];
$$BoltzmannConstantSI      = Tool`Constants`Private`$$BoltzmannConstantSI;
$$VacuumPremittivitySI     = Tool`Constants`Private`$$VacuumPremittivitySI;
$$PlanckConstantSI         = Tool`Constants`Private`$$PlanckConstantSI;
$$ElectronChargeSI	       = Tool`Constants`Private`$$ElectronChargeSI;
$$SpeedOfLightSI           = Tool`Constants`Private`$$SpeedOfLightSI;

(* Get Helpers *)
Needs["Tool`Helpers`"];
ClearAll[$$FailureFunctionSignature, $JouleToEV];
$$FailureFunctionSignature = Tool`Helpers`Private`$FailureFunctionSignature;
$JouleToEV                 = Tool`Helpers`Private`$JouleToEV;

(* Get Semiconductor Parameters *)
Needs["Tool`Semiconductors`"];
ClearAll[$EffectiveMass, $BohrRadius, $RydbergEnergy, $DielectricConstant, $GapEnergy, $Linewidth];
$EffectiveMass              = Tool`Semiconductors`Private`$EffectiveMass;
$BohrRadius        		    = Tool`Semiconductors`Private`$BohrRadius;
$DielectricConstant         = Tool`Semiconductors`Private`$DielectricConstant;
$GapEnergy                  = Tool`Semiconductors`Private`$GapEnergy;
$Linewidth					= Tool`Semiconductors`Private`$Linewidth;

(* Interband absorption coefficient *)
InterbandAbsorptionCoefficient[InitialState_, FinalState_, Hole_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI = QuantityMagnitude @ $$BoltzmannConstantSI,
			EffectiveMass 		= QuantityMagnitude @ $EffectiveMass[InitialState["Semiconductor"], #] &,
			GapEnergy 			= QuantityMagnitude @ $GapEnergy[InitialState["Semiconductor"], temperature],
			BohrRadius 			= QuantityMagnitude @ $BohrRadius[InitialState["Semiconductor"], #] &,
			Linewidth			= $Linewidth[InitialState["Semiconductor"], temperature]
			,
			ElectronModel, HoleModel, ElectronWaveFunction, HoleWaveFunction, ElectronEnergy, HoleEnergy,
			MatrixElement, DeltaEnergy, Chemicalpotential, FermiDirac
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
		
		With[
			{
				LightEnergy = Global`LightEnergy
			},

			Times[
				FermiDirac[ElectronEnergy] * (1 - FermiDirac[HoleEnergy]),
				LightEnergy * Linewidth / ((LightEnergy - DeltaEnergy)^2 + Linewidth^2),
				MatrixElement^2
			]
		]
	];
InterbandAbsorptionCoefficient[___] := $$FailureFunctionSignature["Dependencies`Private`InterbandAbsorptionCoefficient"];

(* Photoluminescence coefficient *)
PhotoluminescenceCoefficient[InitialState_, FinalState_, Hole_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI = QuantityMagnitude @ $$BoltzmannConstantSI,
			Mass 				= QuantityMagnitude @ EffectiveMass[InitialState["Semiconductor"], #] &,
			Gap 				= QuantityMagnitude @ $GapEnergy[InitialState["Semiconductor"], temperature]
		},

		absorption = Simplify @ Plus @@ Map[
			InterbandAbsorptionCoefficient[InitialState, FinalState, Hole, temperature]
		];

		With[
			{
				LightEnergy = Global`LightEnergy
			},

			Times[
				LightEnergy * absorption,
				Exp[-(LightEnergy - 10^3 * Gap)/(10^3 * $JouleToEV[BoltzmannConstantSI * temperature])],
				Exp[(0.5 - Gap)/($JouleToEV[BoltzmannConstantSI * temperature])]
			]
		]
	];
PhotoluminescenceCoefficient[___] := $$FailureFunctionSignature["Dependencies`Private`PhotoluminescenceCoefficient"];

(* Intraband absorption coefficient *)
IntrabandAbsorptionCoefficient[InitialState_, FinalState_, Particle_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI 	= QuantityMagnitude @ $$BoltzmannConstantSI,
			PlanckConstantSI 		= QuantityMagnitude @ $$PlanckConstantSI,
			VacuumPremittivitySI 	= QuantityMagnitude @ $$VacuumPremittivitySI,
			ElectronChargeSI 		= QuantityMagnitude @ $$ElectronChargeSI,
			SpeedOfLightSI 			= QuantityMagnitude @ $$SpeedOfLightSI,
			Radius 					= QuantityMagnitude @ $BohrRadius[InitialState["Semiconductor"], #] &,
			Mass 					= QuantityMagnitude @ $EffectiveMass[InitialState["Semiconductor"], #] &,
			Gap 					= QuantityMagnitude @ $GapEnergy[InitialState["Semiconductor"], temperature],
			Linewidth				= $Linewidth[InitialState["Semiconductor"], temperature]
			,
			Intensity = 10^8, ElectronsPopulation = 3*10^22, InfraredRefractiveIndex = 3.51
			,
			Particle1Model, Particle2Model, Particle1WaveFunction, Particle2WaveFunction, Particle1Energy, Particle2Energy,
			MatrixElement12, MatrixElement11, MatrixElement22, DeltaEnergy, Chemicalpotential, FermiDirac,
			LinearConstant, NonLinearConstant, LinearAbsorptionCoefficient, NonLinearAbsorptionCoefficient
		},

		Particle1Model = InitialState[Particle];
		Particle1WaveFunction = ReplaceAll[
			Particle1Model["Axial", "WaveFunction"] * Particle1Model["Radial", "WaveFunction"],
			Global`r -> Max[InitialState["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle1Energy = Particle1Model["Axial", "Energy"] + Particle1Model["Radial", "Energy"];

		Particle2Model = FinalState[Particle];
		Particle2WaveFunction = ReplaceAll[
			Particle2Model["Axial", "WaveFunction"] * Particle2Model["Radial", "WaveFunction"],
			Global`r -> Max[FinalState["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle2Energy = Particle2Model["Axial", "Energy"] + Particle2Model["Radial", "Energy"];

		MatrixElement12 = Abs @ Integrate[
			2*Pi * Particle1WaveFunction * Global`z * Particle2WaveFunction
			,
			Prepend[
				InitialState["Geometry", "Axial"],
				Global`z
			]
		];
		MatrixElement11 = Abs @ Integrate[
			2*Pi * Particle1WaveFunction * Global`z * Particle1WaveFunction
			,
			Prepend[
				InitialState["Geometry", "Axial"],
				Global`z
			]
		];
		MatrixElement22 = Abs @ Integrate[
			2*Pi * Particle2WaveFunction * Global`z * Particle2WaveFunction
			,
			Prepend[
				InitialState["Geometry", "Axial"],
				Global`z
			]
		];

		DeltaEnergy = (Particle2Energy - Particle1Energy)  * 10^3;
		
		Chemicalpotential = - (Gap/2) + (3/4) * $JouleToEV[BoltzmannConstantSI * temperature] * Log[Mass["Heavy Hole"] / Mass["Electron"]];
		FermiDirac = 1 / (1 + Exp[(# - Chemicalpotential) / $JouleToEV[BoltzmannConstantSI * temperature]]) &;
		
		LinearConstant = Divide[
			Radius[Particle]^2 * ElectronChargeSI^2 * ElectronsPopulation,
			PlanckConstantSI * VacuumPremittivitySI * $DielectricConstant[InitialState["Semiconductor"]] * SpeedOfLightSI
		];

		NonLinearConstant = 10^6 * Nest[
			$JouleToEV,
			Divide[
				- Radius[Particle]^4 * ElectronChargeSI^4 * 2*ElectronsPopulation * Intensity,
				PlanckConstantSI * VacuumPremittivitySI^2 * $DielectricConstant[InitialState["Semiconductor"]] * SpeedOfLightSI^2 * InfraredRefractiveIndex
			],
			2
		];

		With[
			{
				LightEnergy = Global`LightEnergy
			},

			LinearAbsorptionCoefficient = Times[
				LinearConstant * FermiDirac[Particle1Energy] * (1 - FermiDirac[Particle2Energy]),
				LightEnergy * Linewidth *  MatrixElement12^2 / ((DeltaEnergy - LightEnergy)^2 + Linewidth^2)
			];
			LinearAbsorptionCoefficient = ToExpression[
				StringReplace[ToString[LinearAbsorptionCoefficient, InputForm], "LightEnergy" -> "#"] <> "&"
			];

			NonLinearAbsorptionCoefficient = Times[
				NonLinearConstant * FermiDirac[Particle1Energy] * (1 - FermiDirac[Particle2Energy]),
				LightEnergy * Linewidth * MatrixElement12^4 / ((DeltaEnergy - LightEnergy)^2 + Linewidth^2)^2,
				1 - ((MatrixElement22 - MatrixElement11)^2/(4 Abs[MatrixElement12]^2))*((3 DeltaEnergy^2 - 4*LightEnergy*DeltaEnergy + LightEnergy^2 - Linewidth^2)/(DeltaEnergy^2 + Linewidth^2))
			];
			NonLinearAbsorptionCoefficient = ToExpression[
				StringReplace[ToString[NonLinearAbsorptionCoefficient, InputForm], "LightEnergy" -> "#"] <> "&"
			];

			AssociationThread[
				{"Linear", "Nonlinear"}
				,
				{LinearAbsorptionCoefficient, NonLinearAbsorptionCoefficient}
			]
		]
	];
IntrabandAbsorptionCoefficient[___] := $$FailureFunctionSignature["Dependencies`Private`IntrabandAbsorptionCoefficient"];

(* Reflective index change *)
ReflectiveIndexChange[InitialState_, FinalState_, Particle_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI 	= QuantityMagnitude @ $$BoltzmannConstantSI,
			PlanckConstantSI 		= QuantityMagnitude @ $$PlanckConstantSI,
			VacuumPremittivitySI 	= QuantityMagnitude @ $$VacuumPremittivitySI,
			ElectronChargeSI 		= QuantityMagnitude @ $$ElectronChargeSI,
			SpeedOfLightSI 			= QuantityMagnitude @ $$SpeedOfLightSI,
			Radius 					= QuantityMagnitude @ $BohrRadius[InitialState["Semiconductor"], #] &,
			Mass 					= QuantityMagnitude @ $EffectiveMass[InitialState["Semiconductor"], #] &,
			Gap 					= QuantityMagnitude @ $GapEnergy[InitialState["Semiconductor"], temperature],
			Linewidth				= $Linewidth[InitialState["Semiconductor"], temperature]
			,
			Intensity = 10^8, ElectronsPopulation = 3*10^22, InfraredRefractiveIndex = 3.51
			,
			Particle1Model, Particle2Model, Particle1WaveFunction, Particle2WaveFunction, Particle1Energy, Particle2Energy,
			MatrixElement12, MatrixElement11, MatrixElement22, DeltaEnergy, Chemicalpotential, FermiDirac,
			LinearConstant, NonLinearConstant, LinearAbsorptionCoefficient, NonLinearAbsorptionCoefficient
		},

		Particle1Model = InitialState[Particle];
		Particle1WaveFunction = ReplaceAll[
			Particle1Model["Axial", "WaveFunction"] * Particle1Model["Radial", "WaveFunction"],
			Global`r -> Max[InitialState["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle1Energy = Particle1Model["Axial", "Energy"] + Particle1Model["Radial", "Energy"];

		Particle2Model = FinalState[Particle];
		Particle2WaveFunction = ReplaceAll[
			Particle2Model["Axial", "WaveFunction"] * Particle2Model["Radial", "WaveFunction"],
			Global`r -> Max[FinalState["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle2Energy = Particle2Model["Axial", "Energy"] + Particle2Model["Radial", "Energy"];

		MatrixElement12 = Abs @ Integrate[
			2*Pi * Particle1WaveFunction * Global`z * Particle2WaveFunction
			,
			Prepend[
				InitialState["Geometry", "Axial"],
				Global`z
			]
		];
		MatrixElement11 = Abs @ Integrate[
			2*Pi * Particle1WaveFunction * Global`z * Particle1WaveFunction
			,
			Prepend[
				InitialState["Geometry", "Axial"],
				Global`z
			]
		];
		MatrixElement22 = Abs @ Integrate[
			2*Pi * Particle2WaveFunction * Global`z * Particle2WaveFunction
			,
			Prepend[
				InitialState["Geometry", "Axial"],
				Global`z
			]
		];

		DeltaEnergy = Abs[Particle2Energy - Particle1Energy]  * 10^3;
		
		Chemicalpotential = - (Gap/2) + (3/4) * $JouleToEV[BoltzmannConstantSI * temperature] * Log[Mass["Heavy Hole"] / Mass["Electron"]];
		FermiDirac = 1 / (1 + Exp[(# - Chemicalpotential) / $JouleToEV[BoltzmannConstantSI * temperature]]) &;
		
		LinearConstant = $JouleToEV[
			Divide[
				10^3 * Radius[Particle]^2 * ElectronChargeSI^2 * ElectronsPopulation,
				VacuumPremittivitySI * 2*InfraredRefractiveIndex^2
			]
		];

		NonLinearConstant = Nest[
			$JouleToEV,
			Divide[
				- 10^9 * Radius[Particle]^4 * ElectronChargeSI^4 * ElectronsPopulation * Intensity,
				VacuumPremittivitySI^2 * 4*InfraredRefractiveIndex^3 * SpeedOfLightSI * $DielectricConstant[InitialState["Semiconductor"]]
			],
			3
		];

		With[
			{
				LightEnergy = Global`LightEnergy
			},

			LinearAbsorptionCoefficient = Times[
				LinearConstant * FermiDirac[Particle1Energy] * (1 - FermiDirac[Particle2Energy]) * MatrixElement12^2,
				(DeltaEnergy - LightEnergy) / ((DeltaEnergy - LightEnergy)^2 + Linewidth^2)
			];
			LinearAbsorptionCoefficient = ToExpression[
				StringReplace[ToString[LinearAbsorptionCoefficient, InputForm], "LightEnergy" -> "#"] <> "&"
			];

			NonLinearAbsorptionCoefficient = Times[
				NonLinearConstant * FermiDirac[Particle1Energy] * (1 - FermiDirac[Particle2Energy]),
				MatrixElement12^2/((DeltaEnergy - LightEnergy)^2 + Linewidth^2)^2 (4 MatrixElement12^2 (DeltaEnergy-LightEnergy)-(MatrixElement22 - MatrixElement11)^2/(DeltaEnergy^2 + Linewidth^2) ((DeltaEnergy - LightEnergy) (DeltaEnergy (DeltaEnergy - LightEnergy) - Linewidth^2) - Linewidth^2 (2 DeltaEnergy - LightEnergy)))
			];
			NonLinearAbsorptionCoefficient = ToExpression[
				StringReplace[ToString[NonLinearAbsorptionCoefficient, InputForm], "LightEnergy" -> "#"] <> "&"
			];

			AssociationThread[
				{"Linear", "Nonlinear"}
				,
				{LinearAbsorptionCoefficient, NonLinearAbsorptionCoefficient}
			]
		]
	];
ReflectiveIndexChange[___] := $$FailureFunctionSignature["Dependencies`Private`ReflectiveIndexChange"];

(* Second Harmonic Generation *)
SecondHarmonicGeneration[State1_, State2_, State3_, Particle_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI 	= QuantityMagnitude @ $$BoltzmannConstantSI,
			VacuumPremittivitySI 	= QuantityMagnitude @ $$VacuumPremittivitySI,
			ElectronChargeSI 		= QuantityMagnitude @ $$ElectronChargeSI,
			Radius 					= QuantityMagnitude @ $BohrRadius[State1["Semiconductor"], #] &,
			Mass 					= QuantityMagnitude @ $EffectiveMass[State1["Semiconductor"], #] &,
			Gap 					= QuantityMagnitude @ $GapEnergy[State1["Semiconductor"], temperature],
			Linewidth				= $Linewidth[State1["Semiconductor"], temperature]
			,
			ElectronsPopulation = 3 * 10^22
			,
			Particle1Model, Particle1WaveFunction, Particle1Energy,
			Particle2Model, Particle2WaveFunction, Particle2Energy,
			Particle3Model, Particle3WaveFunction, Particle3Energy,
			MatrixElement12, MatrixElement13, MatrixElement23,
			DeltaEnergy12, DeltaEnergy13, Chemicalpotential, FermiDirac,
			LinearAbsorptionCoefficient
		},

		Particle1Model = State1[Particle];
		Particle1WaveFunction = ReplaceAll[
			Particle1Model["Axial", "WaveFunction"] * Particle1Model["Radial", "WaveFunction"],
			Global`r -> Max[State1["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle1Energy = Particle1Model["Axial", "Energy"] + Particle1Model["Radial", "Energy"];

		Particle2Model = State2[Particle];
		Particle2WaveFunction = ReplaceAll[
			Particle2Model["Axial", "WaveFunction"] * Particle2Model["Radial", "WaveFunction"],
			Global`r -> Max[State2["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle2Energy = Particle2Model["Axial", "Energy"] + Particle2Model["Radial", "Energy"];

		Particle3Model = State3[Particle];
		Particle3WaveFunction = ReplaceAll[
			Particle3Model["Axial", "WaveFunction"] * Particle3Model["Radial", "WaveFunction"],
			Global`r -> Max[State3["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle3Energy = Particle3Model["Axial", "Energy"] + Particle3Model["Radial", "Energy"];
		
		MatrixElement12 = Abs @ Integrate[
			2*Pi * Particle1WaveFunction * Global`z * Particle2WaveFunction
			,
			Prepend[
				State1["Geometry", "Axial"],
				Global`z
			]
		];

		MatrixElement13 = Abs @ Integrate[
			2*Pi * Particle1WaveFunction * Global`z * Particle3WaveFunction
			,
			Prepend[
				State1["Geometry", "Axial"],
				Global`z
			]
		];

		MatrixElement23 = Abs @ Integrate[
			2*Pi * Particle2WaveFunction * Global`z * Particle3WaveFunction
			,
			Prepend[
				State1["Geometry", "Axial"],
				Global`z
			]
		];

		DeltaEnergy12 = Abs[Particle2Energy - Particle1Energy]  * 10^3;
		DeltaEnergy13 = Abs[Particle3Energy - Particle1Energy]  * 10^3;
		
		Chemicalpotential = - (Gap/2) + (3/4) * $JouleToEV[BoltzmannConstantSI * temperature] * Log[Mass["Heavy Hole"] / Mass["Electron"]];
		FermiDirac = 1 / (1 + Exp[(# - Chemicalpotential) / $JouleToEV[BoltzmannConstantSI * temperature]]) &;
		
		LinearConstant = 10^6 * Nest[$JouleToEV, Radius[Particle]^3 * ElectronChargeSI^3 * ElectronsPopulation / VacuumPremittivitySI, 2];

		With[
			{
				LightEnergy = Global`LightEnergy
			},

			LinearAbsorptionCoefficient = Times[
				LinearConstant * FermiDirac[Particle1Energy] * (1 - FermiDirac[Particle2Energy]) * (1 - FermiDirac[Particle3Energy]),
				Abs[MatrixElement12 * MatrixElement13 *  MatrixElement23 / ((LightEnergy - DeltaEnergy12 - I*Linewidth) * (2*LightEnergy - DeltaEnergy13 - I*Linewidth))]
			];
			LinearAbsorptionCoefficient = ToExpression[
				StringReplace[ToString[LinearAbsorptionCoefficient, InputForm], "LightEnergy" -> "#"] <> "&"
			]
		]
	];
SecondHarmonicGeneration[___] := $$FailureFunctionSignature["Dependencies`Private`SecondHarmonicGeneration"];

(* Third Harmonic Generation *)
ThirdHarmonicGeneration[State1_, State2_, State3_, State4_, Particle_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI 	= QuantityMagnitude @ $$BoltzmannConstantSI,
			VacuumPremittivitySI 	= QuantityMagnitude @ $$VacuumPremittivitySI,
			ElectronChargeSI 		= QuantityMagnitude @ $$ElectronChargeSI,
			Radius 					= QuantityMagnitude @ $BohrRadius[State1["Semiconductor"], #] &,
			Mass 					= QuantityMagnitude @ $EffectiveMass[State1["Semiconductor"], #] &,
			Gap 					= QuantityMagnitude @ $GapEnergy[State1["Semiconductor"], temperature],
			Linewidth				= $Linewidth[State1["Semiconductor"], temperature]

			,
			ElectronsPopulation = 3 * 10^22
			,
			Particle1Model, Particle1WaveFunction, Particle1Energy,
			Particle2Model, Particle2WaveFunction, Particle2Energy,
			Particle3Model, Particle3WaveFunction, Particle3Energy,
			Particle4Model, Particle4WaveFunction, Particle4Energy,
			MatrixElement12, MatrixElement33, MatrixElement34, MatrixElement14,
			DeltaEnergy12, DeltaEnergy13, DeltaEnergy14, Chemicalpotential, FermiDirac,
			LinearAbsorptionCoefficient
		},

		Particle1Model = State1[Particle];
		Particle1WaveFunction = ReplaceAll[
			Particle1Model["Axial", "WaveFunction"] * Particle1Model["Radial", "WaveFunction"],
			Global`r -> Max[State1["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle1Energy = Particle1Model["Axial", "Energy"] + Particle1Model["Radial", "Energy"];

		Particle2Model = State2[Particle];
		Particle2WaveFunction = ReplaceAll[
			Particle2Model["Axial", "WaveFunction"] * Particle2Model["Radial", "WaveFunction"],
			Global`r -> Max[State2["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle2Energy = Particle2Model["Axial", "Energy"] + Particle2Model["Radial", "Energy"];

		Particle3Model = State3[Particle];
		Particle3WaveFunction = ReplaceAll[
			Particle3Model["Axial", "WaveFunction"] * Particle3Model["Radial", "WaveFunction"],
			Global`r -> Max[State3["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle3Energy = Particle3Model["Axial", "Energy"] + Particle3Model["Radial", "Energy"];

		Particle4Model = State4[Particle];
		Particle4WaveFunction = ReplaceAll[
			Particle4Model["Axial", "WaveFunction"] * Particle4Model["Radial", "WaveFunction"],
			Global`r -> Max[State4["Geometry", "Radial"]] * Radius[Particle]
		];
		Particle4Energy = Particle4Model["Axial", "Energy"] + Particle4Model["Radial", "Energy"];

		MatrixElement12 = Abs @ Integrate[
			2*Pi * Particle1WaveFunction * Global`z * Particle2WaveFunction
			,
			Prepend[
				State1["Geometry", "Axial"],
				Global`z
			]
		];

		MatrixElement23 = Abs @ Integrate[
			2*Pi * Particle2WaveFunction * Global`z * Particle3WaveFunction
			,
			Prepend[
				State1["Geometry", "Axial"],
				Global`z
			]
		];

		MatrixElement34 = Abs @ Integrate[
			2*Pi * Particle3WaveFunction * Global`z * Particle4WaveFunction
			,
			Prepend[
				State1["Geometry", "Axial"],
				Global`z
			]
		];

		MatrixElement14 = Abs @ Integrate[
			2*Pi * Particle1WaveFunction * Global`z * Particle4WaveFunction
			,
			Prepend[
				State1["Geometry", "Axial"],
				Global`z
			]
		];

		DeltaEnergy12 = Abs[Particle2Energy - Particle1Energy]  * 10^3;
		DeltaEnergy13 = Abs[Particle3Energy - Particle1Energy]  * 10^3;
		DeltaEnergy14 = Abs[Particle4Energy - Particle1Energy]  * 10^3;

				
		Chemicalpotential = - (Gap/2) + (3/4) * $JouleToEV[BoltzmannConstantSI * temperature] * Log[Mass["Heavy Hole"] / Mass["Electron"]];
		FermiDirac = 1 / (1 + Exp[(# - Chemicalpotential) / $JouleToEV[BoltzmannConstantSI * temperature]]) &;
		
		LinearConstant = 10^9 * Nest[$JouleToEV, Radius[Particle]^4 * ElectronChargeSI^4 * ElectronsPopulation / VacuumPremittivitySI, 3];
		
		With[
			{
				LightEnergy = Global`LightEnergy
			},

			LinearAbsorptionCoefficient = Times[
				LinearConstant * FermiDirac[Particle1Energy] * (1 - FermiDirac[Particle2Energy]) * (1 - FermiDirac[Particle3Energy]) * (1 - FermiDirac[Particle4Energy]),
				Abs @ Divide[
					MatrixElement12 * MatrixElement23 *  MatrixElement34 * MatrixElement14,
					(LightEnergy - DeltaEnergy12 - I*Linewidth) * (2*LightEnergy - DeltaEnergy13 - I*Linewidth) * (3*LightEnergy - DeltaEnergy14 - I*Linewidth)
				]
			];
			LinearAbsorptionCoefficient = ToExpression[
				StringReplace[ToString[LinearAbsorptionCoefficient, InputForm], "LightEnergy" -> "#"] <> "&"
			]
		]
	];
ThirdHarmonicGeneration[___] := $$FailureFunctionSignature["Dependencies`Private`ThirdHarmonicGeneration"];

(* Photoionisation cross section *)
PhotoionisationCrossSection[InitialState_, FinalState_, Hole_, temperature_] :=
	Catch @ Block[
		{
			BoltzmannConstantSI = QuantityMagnitude @ $$BoltzmannConstantSI,
			EffectiveMass 		= QuantityMagnitude @ $EffectiveMass[InitialState["Semiconductor"], #] &,
			GapEnergy 			= QuantityMagnitude @ $GapEnergy[InitialState["Semiconductor"], temperature],
			BohrRadius 			= QuantityMagnitude @ $BohrRadius[InitialState["Semiconductor"], #] &,
			Linewidth			= $Linewidth[InitialState["Semiconductor"], temperature],
			ElectronChargeSI	= QuantityMagnitude @ $$ElectronChargeSI,
			SpeedOfLightSI    	= QuantityMagnitude @ $$SpeedOfLightSI,
			PlanckConstantSI	= QuantityMagnitude @ $$PlanckConstantSI,
			DielectricConstant 	= QuantityMagnitude @ $DielectricConstant[InitialState["Semiconductor"]],
			InfraredRefractiveIndex = 3.51
			,
			ElectronModel, HoleModel, ElectronWaveFunction, HoleWaveFunction, ElectronEnergy, HoleEnergy,
			MatrixElement, DeltaEnergy, Chemicalpotential, FermiDirac, finiteStructureConst
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

		finiteStructureConst = ElectronChargeSI^2 / (PlanckConstantSI * SpeedOfLightSI);

		DeltaEnergy = (ElectronEnergy + HoleEnergy + GapEnergy)  * 10^3;
		
		Chemicalpotential = - (GapEnergy/2) + (3/4) * $JouleToEV[BoltzmannConstantSI * temperature] * Log[EffectiveMass["Heavy Hole"] / EffectiveMass["Electron"]];
		FermiDirac = 1 / (1 + Exp[(# - Chemicalpotential) / $JouleToEV[BoltzmannConstantSI * temperature]]) &;
		
		With[
			{
				LightEnergy = Global`LightEnergy
			},
			
			Times[
				FermiDirac[ElectronEnergy] * (1 - FermiDirac[HoleEnergy]),
				InfraredRefractiveIndex/DielectricConstant,
				finiteStructureConst * 4 * Pi^3 / 3,
				LightEnergy * Linewidth / ((LightEnergy - DeltaEnergy)^2 + Linewidth^2),
				MatrixElement^2
			]
		]
	];
PhotoionisationCrossSection[___] := $$FailureFunctionSignature["Dependencies`Private`PhotoionisationCrossSection"];

End[];
EndPackage[];