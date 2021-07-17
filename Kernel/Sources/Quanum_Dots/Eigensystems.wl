BeginPackage["Tool`Eigensystems`"];
Begin["`Private`"];

(*	
	██████╗ ███████╗███████╗██╗███╗   ██╗██╗████████╗██╗ ██████╗ ███╗   ██╗███████╗
	██╔══██╗██╔════╝██╔════╝██║████╗  ██║██║╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
	██║  ██║█████╗  █████╗  ██║██╔██╗ ██║██║   ██║   ██║██║   ██║██╔██╗ ██║███████╗
	██║  ██║██╔══╝  ██╔══╝  ██║██║╚██╗██║██║   ██║   ██║██║   ██║██║╚██╗██║╚════██║
	██████╔╝███████╗██║     ██║██║ ╚████║██║   ██║   ██║╚██████╔╝██║ ╚████║███████║
	╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝
*)

Needs["Tool`Constants`"];

ClearAll[$$PlanckConstantSI, $$ElectronChargeSI, $$ElectronChargeCGS, $$SpeedOfLightSI, $$ElectronChargeSI, $$VacuumPremittivitySI];
$$PlanckConstantSI         	= Tool`Constants`Private`$$PlanckConstantSI;
$$ElectronChargeSI         	= Tool`Constants`Private`$$ElectronChargeSI;
$$ElectronChargeCGS        	= Tool`Constants`Private`$$ElectronChargeCGS;
$$SpeedOfLightSI           	= Tool`Constants`Private`$$SpeedOfLightSI;
$$VacuumPremittivitySI     	= Tool`Constants`Private`$$VacuumPremittivitySI;

Needs["Tool`Semiconductors`"];

ClearAll[$EffectiveMass, $BohrRadius, $RydbergEnergy, $DielectricConstant];
$EffectiveMass              = Tool`Semiconductors`Private`$EffectiveMass;
$BohrRadius        		    = Tool`Semiconductors`Private`$BohrRadius;
$RydbergEnergy              = Tool`Semiconductors`Private`$RydbergEnergy;
$DielectricConstant         = Tool`Semiconductors`Private`$DielectricConstant;


Needs["Tool`Helpers`"];

ClearAll[$FailureFunctionSignature, $FailureQuantumNumber];
$$FailureFunctionSignature 	= Tool`Helpers`Private`$FailureFunctionSignature;
$$FailureQuantumNumber     	= Tool`Helpers`Private`$FailureQuantumNumber;

ClearAll[$$StorageDir];
$$EigensystemDir			= Tool`Helpers`Private`$$EigensystemDir;

ClearAll[JouleToEV];
JouleToEV                  	= Tool`Helpers`Private`$JouleToEV;

Needs["Tool`Potentials`"];

ClearAll[DoubleMorseConfinement, MoshinskyConfiniment];
DoubleMorseConfinement     	= Tool`Potentials`Private`DoubleMorseConfinement;
MoshinskyConfiniment       	= Tool`Potentials`Private`MoshinskyConfiniment;

(*	
	███╗   ███╗ ██████╗ ██████╗ ███████╗██╗     ███████╗
	████╗ ████║██╔═══██╗██╔══██╗██╔════╝██║     ██╔════╝
	██╔████╔██║██║   ██║██║  ██║█████╗  ██║     ███████╗
	██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██║     ╚════██║
	██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗███████╗███████║
	╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚══════╝╚══════╝
*)

(*
	Conical Quantum Dot
*)

ClearAll[tempStronglyProlateConicalQuantumDot, StronglyProlateConicalQuantumDot];
tempStronglyProlateConicalQuantumDot[Semiconductor_, BaseRadius_, Height_, {ElectricField_, MagneticField_}, {MagneticNumber_, RadialNumber_, AxialNumber_}] :=
	Catch @ Block[
		{
			PlanckConstantSI 	= QuantityMagnitude @ $$PlanckConstantSI,
			ElectronChargeSI 	= QuantityMagnitude @ $$ElectronChargeSI,

			EffectiveMass 		= QuantityMagnitude @ $EffectiveMass[Semiconductor, #] &,
			BohrRadius 			= QuantityMagnitude @ $BohrRadius[Semiconductor, #] &,
			RydbergEnergy 		= QuantityMagnitude @ $RydbergEnergy[Semiconductor, #] &,
			
			effectivePotential, axialEigensystem, radialEigensystem, totalEigensystem
		},

		With[
			{
				z = Global`z, r = Global`r
			},

			effectivePotential = Times[
				BesselJZero[MagneticNumber, RadialNumber]^2 / BaseRadius^2,
				Subtract[
					1 / (1 - 0.1 * BaseRadius / Height)^2 - (2 * 0.1 * BaseRadius / Height) / (1 - 0.1 * BaseRadius / Height)^3,
					2 * z / (Height * (1 - 0.1 * BaseRadius / Height)^3)
				] 	
			];

			axialEigensystem = Times[
				RydbergEnergy[#],
				NDEigensystem[
					{
						Plus[
							-Laplacian[waveFunction[z], {z}],
							effectivePotential * waveFunction[z],
							-ElectronChargeSI * ElectricField * (2 * EffectiveMass[#] * BohrRadius[#]^3) / PlanckConstantSI^2 * z * waveFunction[z]
						]
						,
						DirichletCondition[waveFunction[z] == 0, True]
					},
					waveFunction[z],
					{z, 0, Height},
					10	
				][[All, AxialNumber + 1]]
			] &;

			radialEigensystem = {
				JouleToEV[
					PlanckConstantSI^2 * BesselJZero[MagneticNumber, RadialNumber]^2 / (2 * EffectiveMass[#] * BohrRadius[#]^2 * BaseRadius^2)
				]
				,
				Sqrt[2]/r * BesselJ[MagneticNumber, N @ BesselJZero[MagneticNumber, RadialNumber] * r] / BesselJ[MagneticNumber + 1, N @ BesselJZero[MagneticNumber, RadialNumber]]
			} &;

			totalEigensystem = AssociationMap[
				Association[
					"Axial" -> AssociationThread[
						{"Energy", "WaveFunction"} -> axialEigensystem[#]
					]
					,
					"Radial" -> AssociationThread[
						{"Energy", "WaveFunction"} -> radialEigensystem[#]
					]
				] &
				,
				{"Electron", "Light Hole", "Heavy Hole"}
			]
		]
	];
StronglyProlateConicalQuantumDot[Semiconductor_?StringQ, BaseRadius_?NumberQ, Height_?NumberQ] :=
	DynamicModule[
		{
			QuantumNumbersValues = Tuples[{Range[-1, 1], Range[1, 5], Range[0, 4]}],
			FieldsValues = Tuples[{Range[0, 5], {0}}], 
			createNB, length, createPanel = "", state, counter = 0,
			electric, magnetic, dir, eigensystem
		},

		createNB = CreatePalette[Dynamic[createPanel], WindowMargins -> Automatic, WindowFrame -> "Frameless"];

		length = Length[FieldsValues] * Length[QuantumNumbersValues];

		Progress`EvaluateWithProgress[
			eigensystem = AssociationMap[
				Function[fields
					,
					electric = First @ fields;
					magnetic = Last @ fields;
					AssociationMap[
						Function[numbers
							,
							state = numbers;
							counter ++;
							tempStronglyProlateConicalQuantumDot[Semiconductor, BaseRadius, Height, fields, numbers]
						]
						,
						QuantumNumbersValues
					]
				]
				,
				FieldsValues
			];

			eigensystem = <|
				"Eigensystem" -> eigensystem,
				"Semiconductor" -> Semiconductor,
				"Geometry" -> <|
					"Radial" -> {-BaseRadius, BaseRadius},
					"Axial" -> {0, Height}
				|>
			|>;

			dir = Export[$$EigensystemDir["Strongly_Prolate_CQD_" <> ToString[Semiconductor]], eigensystem];
			,
			{
				"Semiconductor name" 				-> Semiconductor,
				"QD Base Radius (BohrRadius)" 		-> BaseRadius,
				"QD Height (BohrRadius)" 			-> Height,
				"Electric Field strength (kV/cm)" 	-> electric,
				"Magnetic Field strength (T)" 		-> magnetic,
				"State |Magnetic, Radial, Axial>" 	:> StringReplace[ToString[state], {"{" -> "|", "}" -> ">"}],
				"ItemCurrent" 						:> ToString[counter] <> "/" <> ToString[length],
				"Percentage" 						:> counter / length,
				"Progress" 							:> counter / length,
				"ElapsedTime" 						-> Automatic,
				"RemainingTime" 					-> Automatic
			}
			,
			"Delay" 								-> 0,
			"Buttons" 								-> <|"Stop" :> (FrontEndTokenExecute["EvaluatorAbort"] && NotebookClose[createNB])|>,
			"Title" 								-> "Strongly Prolate CQD Eigensystem",
			"Container" 							:> createPanel
		];

		NotebookClose[createNB];

		dir
	];
StronglyProlateConicalQuantumDot[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`StronglyProlateConicalQuantumDot"];

ClearAll[tempStronglyOblateConicalQuantumDot, StronglyOblateConicalQuantumDot];
tempStronglyOblateConicalQuantumDot[Semiconductor_, BaseRadius_, Height_, {ElectricField_, MagneticField_}, {MagneticNumber_, RadialNumber_, AxialNumber_}] :=
	Catch @ Block[
		{
			PlanckConstantSI 	= QuantityMagnitude @ $$PlanckConstantSI,
			ElectronChargeSI 	= QuantityMagnitude @ $$ElectronChargeSI,

			EffectiveMass 		= QuantityMagnitude @ $EffectiveMass[Semiconductor, #] &,
			BohrRadius 			= QuantityMagnitude @ $BohrRadius[Semiconductor, #] &,

			radialIntegral, firstOrder, axialEigensystem, radialEigensystem, totalEigensystem
		},

		radialIntegral = Plus[
 			0.01 * BesselJZero[MagneticNumber, RadialNumber]^(MagneticNumber+3)/4^MagneticNumber,
			Hypergeometric2F1[MagneticNumber + 1/2, MagneticNumber + 5/2, 2*MagneticNumber+1, -BesselJZero[MagneticNumber, RadialNumber]^2] / ((2*MagneticNumber+3) * Gamma[RadialNumber+1]^2)
		];
		
		firstOrder = Sqrt[2] * Divide[
			BesselJ[MagneticNumber, N @ BesselJZero[MagneticNumber, RadialNumber] * 0.1],
			BaseRadius * BesselJ[MagneticNumber + 1, N @ BesselJZero[MagneticNumber, RadialNumber]]
		];
		With[
			{
				z = Global`z, r = Global`r
			},

			axialEigensystem = {
				JouleToEV[Pi^2 * PlanckConstantSI^2 * AxialNumber / (2 * EffectiveMass[#] * BohrRadius[#]^2 * Height^2)]
				,
				Sqrt[2/(Height * (1 - 0.1))] * Sin[Pi * AxialNumber * z /(Height * (1 - 0.1))]
			} &;

			radialEigensystem = {
				JouleToEV[
					Plus[
						PlanckConstantSI^2 * N @ BesselJZero[MagneticNumber, RadialNumber]^2 / (2 * EffectiveMass[#] * BohrRadius[#]^2 * BaseRadius^2),
						Times[
							Pi^2 * PlanckConstantSI^2 / (2 * EffectiveMass[#] * BohrRadius[#]^2 * Height^2),
							4 * radialIntegral / (BesselJ[MagneticNumber + 1, BesselJZero[MagneticNumber, RadialNumber]] * BesselJZero[MagneticNumber, RadialNumber]^3)
						],
						Times[
							1 / (2 * Pi),
							NIntegrate[
								Times[
									firstOrder^2
									,
									Subtract[
										Pi^2 * PlanckConstantSI^2 / (EffectiveMass[#] * BohrRadius[#]^3 * Height^2 * BaseRadius),
										ElectronChargeSI * ElectricField * Cos[2 * Pi]
									] 
									,
									BohrRadius[#] * r
								]
								,
								{r, 0, BaseRadius}
							]
						]
					]
				]
				,
				Subtract[
					Sqrt[2] / BaseRadius * BesselJ[MagneticNumber, N @ BesselJZero[MagneticNumber, RadialNumber] * r] / BesselJ[MagneticNumber + 1, N @ BesselJZero[MagneticNumber, RadialNumber]] * Sqrt[2 * Pi],
					ElectricField * BaseRadius * ElectronChargeSI * 1/BesselJ[MagneticNumber + 1, N @ BesselJZero[MagneticNumber, RadialNumber]]
				]
			} &;

			totalEigensystem = AssociationMap[
				Association[
					"Axial" -> AssociationThread[
						{"Energy", "WaveFunction"} -> axialEigensystem[#]
					]
					,
					"Radial" -> AssociationThread[
						{"Energy", "WaveFunction"} -> radialEigensystem[#]
					]
				] &
				,
				{"Electron", "Light Hole", "Heavy Hole"}
			]
		]
	]
StronglyOblateConicalQuantumDot[Semiconductor_?StringQ, BaseRadius_?NumberQ, Height_?NumberQ] :=
	DynamicModule[
		{
			QuantumNumbersValues = Tuples[{Range[-1, 1], Range[1, 5], Range[0, 4]}],
			FieldsValues = Tuples[{Range[0, 5], {0}}], 
			createNB, length, createPanel = "", state, counter = 0,
			electric, magnetic, dir, eigensystem
		},

		createNB = CreatePalette[Dynamic[createPanel], WindowMargins -> Automatic, WindowFrame -> "Frameless"];

		length = Length[FieldsValues] * Length[QuantumNumbersValues];

		Progress`EvaluateWithProgress[
			eigensystem = AssociationMap[
				Function[fields
					,
					electric = First @ fields;
					magnetic = Last @ fields;
					AssociationMap[
						Function[numbers
							,
							state = numbers;
							counter ++;
							tempStronglyOblateConicalQuantumDot[Semiconductor, BaseRadius, Height, fields, numbers]
						]
						,
						QuantumNumbersValues
					]
				]
				,
				FieldsValues
			];

			eigensystem = <|
				"Eigensystem" -> eigensystem,
				"Semiconductor" -> Semiconductor,
				"Geometry" -> <|
					"Radial" -> {-BaseRadius, BaseRadius},
					"Axial" -> {0, Height}
				|>
			|>;

			dir = Export[$$EigensystemDir["Strongly_Oblate_CQD_" <> ToString[Semiconductor]], eigensystem];
			,
			{
				"Semiconductor name" 				-> Semiconductor,
				"QD Base Radius (BohrRadius)" 		-> BaseRadius,
				"QD Height (BohrRadius)" 			-> Height,
				"Electric Field strength (kV/cm)" 	-> electric,
				"Magnetic Field strength (T)" 		-> magnetic,
				"State |Magnetic, Radial, Axial>" 	:> StringReplace[ToString[state], {"{" -> "|", "}" -> ">"}],
				"ItemCurrent" 						:> ToString[counter] <> "/" <> ToString[length],
				"Percentage" 						:> counter / length,
				"Progress" 							:> counter / length,
				"ElapsedTime" 						-> Automatic,
				"RemainingTime" 					-> Automatic
			}
			,
			"Delay" 								-> 0,
			"Buttons" 								-> <|"Stop" :> (FrontEndTokenExecute["EvaluatorAbort"] && NotebookClose[createNB])|>,
			"Title" 								-> "Strongly Oblate CQD Eigensystem",
			"Container" 							:> createPanel
		];

		NotebookClose[createNB];

		dir
	];
StronglyOblateConicalQuantumDot[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`StronglyOblateConicalQuantumDot"];

(*
	Ellipsoidal Quantum Dot: 1D Moshinsky Impurity
*)

ClearAll[tempStronglyProlateEllipsoidalQuantumDotWithMoshinsky1D, StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D];
tempStronglyProlateEllipsoidalQuantumDotWithMoshinsky1D[Semiconductor_, SemiAxes_, Interaction_, ParticlesNumber_, {ElectricField_, MagneticField_}, {COMNumber_, RelNumber_}] :=
	Catch @ Block[
		{
			PlanckConstantSI = QuantityMagnitude @ $$PlanckConstantSI,
			
			BohrRadius       = QuantityMagnitude @ $BohrRadius[Semiconductor, #] &,
			EffectiveMass    = QuantityMagnitude @ $EffectiveMass[Semiconductor, #] &,
			
			Moshinsky        = MoshinskyConfiniment[Semiconductor, #, Interaction, SemiAxes, ParticlesNumber] &,
			
			groundEnergy, centerOfMassEnergy, relativeEnergy, waveFunction, totalEigensystem
		},

		With[
			{
				z = Global`z
			},
		
			groundEnergy =
				JouleToEV @ Divide[
					ParticlesNumber * PlanckConstantSI^2,
					4 * EffectiveMass[#] * BohrRadius[#]^2 * Min @ SemiAxes^2
				] &;
			
			centerOfMassEnergy = JouleToEV[PlanckConstantSI * First @ Moshinsky[#] * (COMNumber + 1/2)] &;
		
			relativeEnergy = JouleToEV[PlanckConstantSI * First @ Moshinsky[#] * Min @ Last @ Moshinsky[#] * (RelNumber + (ParticlesNumber - 1) / 2)] &;

			waveFunction = Times[
				1/Sqrt[2^COMNumber * COMNumber!] * (1/Pi) ^ 0.25 * Exp[-Sqrt[ParticlesNumber]^2 * z^2 / 2] * HermiteH[COMNumber, Sqrt[ParticlesNumber] * z]
				,
				Times[
					1/Sqrt[2^RelNumber * RelNumber!] * (Min @ Last[Moshinsky[#]]/Pi) ^ 0.25,
					Exp[-(Sqrt[(ParticlesNumber - 1)/ParticlesNumber] * z - 1/(ParticlesNumber - 1)*(ParticlesNumber - 1) * z)^2 / 2],
					HermiteH[RelNumber, Sqrt[Last[Moshinsky[#]]] * (Sqrt[(ParticlesNumber - 1)/ParticlesNumber] * z - 1/(ParticlesNumber - 1) * (ParticlesNumber - 1) * z)]
				] ^ (ParticlesNumber - 1)
			] &;

			totalEigensystem = AssociationMap[
				Association[
					"Ground" -> groundEnergy[#]
					,
					"CenterOfMass" -> centerOfMassEnergy[#]
					,
					"Relative" -> relativeEnergy[#]
					,
					"WaveFunction" -> waveFunction[#]
				] &
				,
				{"Electron", "Light Hole", "Heavy Hole"}
			]
		]
	];
StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D[Semiconductor_?StringQ] :=
	DynamicModule[
		{
			QuantumNumbersValues = Tuples[{Range[0, 4], Range[0, 4]}],
			FieldsValues = Tuples[{{0}, {0}}],
			ParticlesValues = Range[2, 10],
			InteractionValues = {0, 0.4*10^-3},
			SemiAxesValues = Table[{m, 5 * m}, {m, 0.1, 1, 0.1}],
			createNB, length, createPanel = "", state, counter = 0,
			electric, magnetic, particlesNum, currentSemiAxes, inter, dir, eigensystem
		},

		createNB = CreatePalette[Dynamic[createPanel], WindowMargins -> Automatic, WindowFrame -> "Frameless"];

		length = Length[ParticlesValues] * Length[FieldsValues] * Length[QuantumNumbersValues] * Length[InteractionValues] * Length[SemiAxesValues];
		

		Progress`EvaluateWithProgress[
			eigensystem = AssociationMap[
				Function[semiAxes
					,
					currentSemiAxes = semiAxes;
					AssociationMap[	
						Function[pairInt
							,
							inter = pairInt;
							AssociationMap[
								Function[particles
									,
									particlesNum = particles;
									AssociationMap[
										Function[fields
											,
											electric = First @ fields;
											magnetic = Last @ fields;
											AssociationMap[
												Function[numbers
													,
													state = numbers;
													counter ++;
													tempStronglyProlateEllipsoidalQuantumDotWithMoshinsky1D[Semiconductor, semiAxes, pairInt, particles, fields, numbers]
												]
												,
												QuantumNumbersValues
											]
										]
										,
										FieldsValues
									]
								]
								,
								ParticlesValues
							]
						]
						,
						InteractionValues
					]
				]
				,
				SemiAxesValues
			];

			dir = Export[$$EigensystemDir["Strongly_Prolate_EQD_Moshinsky1D_" <> ToString[Semiconductor]], BinarySerialize @ eigensystem];
			,
			{
				"Semiconductor name" 				-> Semiconductor,
				"QD Semiaxes (BohrRadius)" 			-> currentSemiAxes,
				"Interaction (eV / BohrRadius^2)"	-> inter,
				"Particles Number"					-> particlesNum,
				"Electric Field strength (kV/cm)" 	-> electric,
				"Magnetic Field strength (T)" 		-> magnetic,
				"State |Center of Mass, Relative>" 	:> StringReplace[ToString[state], {"{" -> "|", "}" -> ">"}],
				"ItemCurrent" 						:> ToString[counter] <> "/" <> ToString[length],
				"Percentage" 						:> counter / length,
				"Progress" 							:> counter / length,
				"ElapsedTime" 						-> Automatic,
				"RemainingTime" 					-> Automatic
			}
			,
			"Delay" 								-> 0,
			"Buttons" 								-> <|"Stop" :> (FrontEndTokenExecute["EvaluatorAbort"] && NotebookClose[createNB])|>,
			"Title" 								-> "Strongly Prolate EQD Eigensystem",
			"Container" 							:> createPanel
		];

		NotebookClose[createNB];

		dir
	];
StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D"];

(*
	Lens-Shaped Quantum Dot
*)

ClearAll[tempBiconvexLensQuantumDot, BiconvexLensQuantumDot];
tempBiconvexLensQuantumDot[Semiconductor_, Radiuses_, Heights_, {ElectricField_, MagneticField_}, {MagneticNumber_, RadialNumber_, AxialNumber_}] :=
	Block[
		{
			PlanckConstantSI 	= QuantityMagnitude @ $$PlanckConstantSI,
			ElectronChargeCGS 	= QuantityMagnitude @ $$ElectronChargeCGS,
			SpeedOfLightSI 		= QuantityMagnitude @ $$SpeedOfLightSI,
			
			EffectiveMass 		= QuantityMagnitude @ $EffectiveMass[Semiconductor, #] &,
			BohrRadius 			= QuantityMagnitude @ $BohrRadius[Semiconductor, #] &,
			
			hB = Max @ Heights, hS = Min @ Heights, rB = Max @ Radiuses, rS = Min @ Radiuses,
			
			heightBig, heightSmall, radiusBig, radiusSmall,
			cyclotronFrequency, generalFrequency, problemFrequency, wellDepth,
			axialEigensystem, radialEigensystem, totalEigensystem
		},

		With[
			{
				z = Global`z, r = Global`r
			},

			heightBig 	= BohrRadius[#] * hB &;
			heightSmall = BohrRadius[#] * hS &;
			radiusBig 	= BohrRadius[#] * rB &;
			radiusSmall = BohrRadius[#] * rS &;
			dimR 		= BohrRadius[#] * r &;
			dimZ 		= BohrRadius[#] * z &;

			wellDepth = Sqrt[radiusBig[#]^2 - dimR[#]^2] + Sqrt[radiusSmall[#]^2 - dimR[#]^2] + heightBig[#] + heightSmall[#] - radiusBig[#] - radiusSmall[#] &;
		
			cyclotronFrequency = ElectronChargeCGS * MagneticField * 10^4 / (EffectiveMass[#] * 10^3 * SpeedOfLightSI * 10^2) &;

			generalFrequency = Pi^2 * PlanckConstantSI^2 * AxialNumber^2 * (radiusBig[#] + radiusSmall[#]) / (EffectiveMass[#]^2 * (heightBig[#] + heightSmall[#])^3 * radiusBig[#] * radiusSmall[#]) &;
			
			problemFrequency = Sqrt[cyclotronFrequency[#]^2 / 4 + generalFrequency[#]] &;
			
			MagneticLength = Sqrt[PlanckConstantSI/(EffectiveMass[#] * problemFrequency[#])] &;

			axialEigensystem = {
				JouleToEV[Pi^2 * PlanckConstantSI^2 * AxialNumber^2 / (2 * EffectiveMass[#] * (heightBig[#] + heightSmall[#])^2)],
				Sqrt[2 * BohrRadius[#] / wellDepth[#]] * Sin[Pi * AxialNumber / wellDepth[#] * (dimZ[#] + Sqrt[radiusSmall[#]^2 - dimR[#]^2] + heightSmall[#] - radiusSmall[#])]
			} &;

			radialEigensystem = {
				JouleToEV[PlanckConstantSI * problemFrequency[#] * (2*RadialNumber+Abs[MagneticNumber]+1) + PlanckConstantSI*cyclotronFrequency[#] * MagneticNumber/2],
				Times[
					1/Sqrt[2*Pi] * Exp[I*MagneticNumber*angle],
					Sqrt[(Abs @ MagneticNumber + RadialNumber)!/(2^Abs[MagneticNumber] * RadialNumber! * Abs[MagneticNumber]!)],
					BohrRadius[#],
					1/(MagneticLength[#]^(Abs[MagneticNumber] + 1)),
					dimR[#]^Abs[MagneticNumber] * Exp[- dimR[#]^2 / (4 * MagneticLength[#]^2)],
					Hypergeometric1F1[- RadialNumber, Abs @ MagneticNumber + 1, dimR[#]^2 / (2 * MagneticLength[#]^2)]
				]
			} &;

			totalEigensystem = AssociationMap[
				Association[
					"Axial" -> AssociationThread[
						{"Energy", "WaveFunction"} -> axialEigensystem[#]
					]
					,
					"Radial" -> AssociationThread[
						{"Energy", "WaveFunction"} -> radialEigensystem[#]
					]
				] &
				,
				{"Electron", "Light Hole", "Heavy Hole"}
			]
		]
	];
BiconvexLensQuantumDot[Semiconductor_?StringQ, Radiuses_?ListQ, Heights_?ListQ] :=
	DynamicModule[
		{
			QuantumNumbersValues = Tuples[{Range[-1, 1], Range[0, 4], Range[1, 5]}],
			FieldsValues = Tuples[{{0}, Range[0, 5]}], 
			createNB, length, createPanel = "", state, counter = 0,
			electric, magnetic, dir, eigensystem
		},

		createNB = CreatePalette[Dynamic[createPanel], WindowMargins -> Automatic, WindowFrame -> "Frameless"];

		length = Length[FieldsValues] * Length[QuantumNumbersValues];

		Progress`EvaluateWithProgress[
			eigensystem = AssociationMap[
				Function[fields
					,
					electric = First @ fields;
					magnetic = Last @ fields;
					AssociationMap[
						Function[numbers
							,
							state = numbers;
							counter ++;
							tempBiconvexLensQuantumDot[Semiconductor, Radiuses, Heights, fields, numbers]
						]
						,
						QuantumNumbersValues
					]
				]
				,
				FieldsValues
			];

			eigensystem  <|
				"Eigensystem" -> eigensystem,
				"Semiconductor" -> Semiconductor,
				"Geometry" -> <|
					"Radial" -> Radiuses,
					"Axial" -> Heights
				|>
			|>;

			dir = Export[$$EigensystemDir["Biconvex_LQD_" <> ToString[Semiconductor]], eigensystem];
			,
			{
				"Semiconductor name" 				-> Semiconductor,
				"QD Radiuses (BohrRadius)" 			-> Radiuses,
				"QD Heights (BohrRadius)" 			-> Heights,
				"Electric Field strength (kV/cm)" 	-> electric,
				"Magnetic Field strength (T)" 		-> magnetic,
				"State |Magnetic, Radial, Axial>" 	:> StringReplace[ToString[state], {"{" -> "|", "}" -> ">"}],
				"ItemCurrent" 						:> ToString[counter] <> "/" <> ToString[length],
				"Percentage" 						:> counter / length,
				"Progress" 							:> counter / length,
				"ElapsedTime" 						-> Automatic,
				"RemainingTime" 					-> Automatic
			}
			,
			"Delay" 								-> 0,
			"Buttons" 								-> <|"Stop" :> (FrontEndTokenExecute["EvaluatorAbort"] && NotebookClose[createNB])|>,
			"Title" 								-> "Biconvex LQD Eigensystem",
			"Container" 							:> createPanel
		];

		NotebookClose[createNB];

		dir
	];
BiconvexLensQuantumDot[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`BiconvexLensQuantumDot"];

(*
	Vertical Coupled Quantum Dot
*)

ClearAll[tempCylindricalQDsWithDoubleMorse, CylindricalQDsWithDoubleMorse];
tempCylindricalQDsWithDoubleMorse[Semiconductor_, Depths_, HalfWidths_, WellDistance_, {ElectricField_, MagneticField_}, {MagneticNumber_, RadialNumber_, AxialNumber_}] :=
	Catch @ Block[
		{
			PlanckConstantSI 		= QuantityMagnitude @ $$PlanckConstantSI,			
			EffectiveMass    		= QuantityMagnitude @ $EffectiveMass[Semiconductor, #] &,
			BohrRadius           	= QuantityMagnitude @ $BohrRadius[Semiconductor, #] &,
			RydbergEnergy          	= QuantityMagnitude @ $RydbergEnergy[Semiconductor, #] &,

			confinementPotential 	= DoubleMorseConfinement[Semiconductor, "Axial", Depths, HalfWidths, WellDistance],
			cylindricalQDDiameter = 2 WellDistance,
			
			axialEigensystem, radialEigensystem, totalEigensystem
		},

		With[
			{
				r = Global`r, z = Global`z
			},

			axialEigensystem =
				Times[
					RydbergEnergy[#],
					NDEigensystem[
						{
							-Laplacian[waveFunction[z], {z}] + confinementPotential * waveFunction[z],
							DirichletCondition[waveFunction[z] == 0, True]
						},
						waveFunction[z],
						{z, -2 WellDistance , 2 WellDistance},
						5
					][[All, AxialNumber + 1]]
				] &;
			
			radialEigensystem =
				{
					JouleToEV[Pi^2 * PlanckConstantSI^2 * RadialNumber^2 / (2 * EffectiveMass[#] * BohrRadius[#]^2 * cylindricalQDDiameter^2)],
					Sqrt[2 / cylindricalQDDiameter] * Sin[Pi * RadialNumber * r / cylindricalQDDiameter]
				} &;

			totalEigensystem = AssociationMap[
				Association[
					"Axial" -> AssociationThread[
						{"Energy", "WaveFunction"} -> axialEigensystem[#]
					]
					,
					"Radial" -> AssociationThread[
						{"Energy", "WaveFunction"} -> radialEigensystem[#]
					]
				] &
				,
				{"Electron", "Light Hole", "Heavy Hole"}
			]
		]

	];
CylindricalQDsWithDoubleMorse[Semiconductor_?StringQ, Depths_?ListQ, HalfWidths_?ListQ, WellDistance_?NumberQ] :=
	DynamicModule[
		{
			QuantumNumbersValues = Tuples[{Range[-1, 1], Range[1, 5], Range[0, 4]}],
			FieldsValues = Tuples[{{0}, {0}}], 
			createNB, length, createPanel = "", state, counter = 0,
			electric, magnetic, dir, eigensystem
		},

		createNB = CreatePalette[Dynamic[createPanel], WindowMargins -> Automatic, WindowFrame -> "Frameless"];

		length = Length[FieldsValues] * Length[QuantumNumbersValues];

		Progress`EvaluateWithProgress[
			eigensystem = AssociationMap[
				Function[fields
					,
					electric = First @ fields;
					magnetic = Last @ fields;
					AssociationMap[
						Function[numbers
							,
							state = numbers;
							counter ++;
							tempCylindricalQDsWithDoubleMorse[Semiconductor, Depths, HalfWidths, WellDistance, fields, numbers]
						]
						,
						QuantumNumbersValues
					]
				]
				,
				FieldsValues
			];

			dir = Export[$$EigensystemDir["Vertically_Coupled_CQD_Double_Morse_" <> ToString[Semiconductor]], eigensystem];
			,
			{
				"Semiconductor name" 						-> Semiconductor,
				"Morse Potentials Depths (BohrRadius)"		-> Depths,
				"Morse Potentials Half Widths (BohrRadius)"	-> HalfWidths,
				"Wells Distance (BohrRadius)" 				-> WellDistance,
				"Electric Field strength (kV/cm)" 			-> electric,
				"Magnetic Field strength (T)" 				-> magnetic,
				"State |Magnetic, Radial, Axial>" 			:> StringReplace[ToString[state], {"{" -> "|", "}" -> ">"}],
				"ItemCurrent" 								:> ToString[counter] <> "/" <> ToString[length],
				"Percentage" 								:> counter / length,
				"Progress" 									:> counter / length,
				"ElapsedTime" 								-> Automatic,
				"RemainingTime" 							-> Automatic
			}
			,
			"Delay" 										-> 0,
			"Buttons" 										-> <|"Stop" :> (FrontEndTokenExecute["EvaluatorAbort"] && NotebookClose[createNB])|>,
			"Title" 										-> "Vertically Coupled CQD Eigensystem",
			"Container" 									:> createPanel
		];

		NotebookClose[createNB];

		dir
	];
CylindricalQDsWithDoubleMorse[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`CylindricalQDsWithDoubleMorse"];

ClearAll[CylindricalQDsWithDoubleMorseImpurityVariation];
CylindricalQDsWithDoubleMorseImpurityVariation[semiconductor_, depths_, halfWidths_, distance_][{RadialNumber_, AxialNumber_}] :=
	Block[
		{
			confinementPotential   = DoubleMorseConfinement[semiconductor, "Axial", depths, halfWidths, distance],
			eigensystem = CylindricalQDsWithDoubleMorse[semiconductor, depths, halfWidths, distance][{RadialNumber, AxialNumber}],
			
			ElectronChargeSI     = QuantityMagnitude @ $$ElectronChargeSI,
			VacuumPremittivitySI = QuantityMagnitude @ $$VacuumPremittivitySI,
			Rydberg              = QuantityMagnitude @ $RydbergEnergy[semiconductor, "Electron"],
			Radius               = QuantityMagnitude @ $BohrRadius[semiconductor, "Electron"],
			$DielectricConstant   = QuantityMagnitude @ $DielectricConstant[semiconductor],
			
			cylindricalQDDiameter = 2 distance,
			nonImpurityWaveFunction, nonimpurityHamiltonian, nonImpurityEnergy,
			impurityCoordinates, impurityHamiltonian, totalHamiltonian,
			normalizationCoefficient, trialWaveFunction, trialEnergy, impurityEnergy
		},
		
		With[
			{
				r = Global`r, z = Global`z, angle = Global`angle,
				rImpurity = 0, zImpurity = 0
			},
	
			nonImpurityWaveFunction = Last@eigensystem["Electron", "Axial"] * Last@eigensystem["Electron", "Radial"];	
			nonImpurityEnergy = First@eigensystem["Electron", "Axial"] + First@eigensystem["Electron", "Radial"];
			
			nonimpurityHamiltonian = -Laplacian[#, {r}] - 1/r * D[#, r] - Laplacian[#, {angle}]/r^2 - Laplacian[#, {z}] + confinementPotential * # &;
	
			impurityCoordinates = Sqrt[r^2 + rImpurity^2 - r * rImpurity * Cos[angle] + (z - zImpurity)^2];
			impurityHamiltonian = - 1/Rydberg * JouleToEV[ElectronChargeSI^2 / (4 * Pi * $DielectricConstant * VacuumPremittivitySI)] / impurityCoordinates * # &;

			totalHamiltonian = nonimpurityHamiltonian[#] + impurityHamiltonian[#] &;

			normalizationCoefficient = Function[
				variation,
				Power[
					Quiet @ NIntegrate[
						Exp[-2 * variation * impurityCoordinates] * Abs[nonImpurityWaveFunction]^2 * r,
						{r, 0, 0.5 cylindricalQDDiameter}, {angle, 0, 2 Pi}, {z, -2 distance, 2 distance}
					],
					- 1/2
				]
			];

			trialWaveFunction = Function[
				variation,
				normalizationCoefficient[variation] * nonImpurityWaveFunction * Exp[-variation * impurityCoordinates]
			];

			trialEnergy = Function[
				variation,
				Quiet @ NIntegrate[
					Conjugate @ trialWaveFunction[variation] * totalHamiltonian[trialWaveFunction[variation]] * r,
					{r, 0, 0.5 cylindricalQDDiameter}, {angle, 0, 2 Pi}, {z, -2 distance, 2 distance}
				]
			];

			impurityEnergy = Min @ Map[
				trialEnergy[#] &
				,
				Range[0, 1, 0.1]
			] * Rydberg;

				<|
					"Without impurity energy" -> nonImpurityEnergy,
					"With impurity energy"    -> impurityEnergy,
					"Binding energy"          -> nonImpurityEnergy - impurityEnergy
				|>
		]
	];
CylindricalQDsWithDoubleMorseImpurityVariation[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`CylindricalQDsWithDoubleMorseImpurityVariation"];

ClearAll[CylindricalQDsWithDoubleMorseImpurityHeisenberg];
CylindricalQDsWithDoubleMorseImpurityHeisenberg[semiconductor_, depths_, halfWidths_, distance_][{RadialNumber_, AxialNumber_}] :=
	Block[
		{
			eigensystem = CylindricalQDsWithDoubleMorse[semiconductor, depths, halfWidths, distance][{RadialNumber, AxialNumber}],			
			
			ElectronChargeSI     = QuantityMagnitude @ $$ElectronChargeSI,
			PlanckConstantSI     = QuantityMagnitude @ $$PlanckConstantSI,
			VacuumPremittivitySI = QuantityMagnitude @ $$VacuumPremittivitySI,
			EffMass              = QuantityMagnitude @ $EffectiveMass[semiconductor, #] &,
			Rydberg              = QuantityMagnitude @ $RydbergEnergy[semiconductor, #] &,
			Radius               = QuantityMagnitude @ $BohrRadius[semiconductor, #] &,
			$DielectricConstant   = QuantityMagnitude @ $DielectricConstant[semiconductor],

			cylindricalQDDiameter = 2 distance, cylindricalQDLength = 10 distance,
			realtiveMass, nonImpurityEnergy, impurityEnergy, axial, radial
		},
		
		realtiveMass = 2 EffMass["Electron"]*EffMass["Heavy Hole"] / (EffMass["Electron"] + EffMass["Heavy Hole"]);

		nonImpurityEnergy = First@eigensystem["Electron", "Axial"] + First@eigensystem["Electron", "Radial"];

		impurityEnergy = - JouleToEV[ElectronChargeSI^2 / (4 * Pi * $DielectricConstant * VacuumPremittivitySI * Radius["Electron"])];

		axial = JouleToEV[PlanckConstantSI^2/(realtiveMass * Radius["Electron"]^2 * (0.5 cylindricalQDDiameter)^2)];

		radial = JouleToEV[PlanckConstantSI^2/(realtiveMass * Radius["Electron"]^2 * cylindricalQDLength^2)];

		impurityEnergy = nonImpurityEnergy + axial + radial + impurityEnergy;

		<|
			"Without impurity energy" -> nonImpurityEnergy,
			"With impurity energy"    -> impurityEnergy,
			"Binding energy"          -> nonImpurityEnergy - impurityEnergy
		|>	
	];
CylindricalQDsWithDoubleMorseImpurityHeisenberg[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`CylindricalQDsWithDoubleMorseImpurityHeisenberg"];

End[];
EndPackage[];