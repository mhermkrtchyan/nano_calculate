BeginPackage["Tool`Eigensystems`"];
Begin["`Private`"];

(* Get Constants *)
Needs["Tool`Constants`"];
ClearAll[$$PlanckConstantSI, $$ElectronChargeSI, $$ElectronChargeCGS, $$SpeedOfLightSI];
$$PlanckConstantSI		= Tool`Constants`Private`$$PlanckConstantSI;
$$ElectronChargeSI		= Tool`Constants`Private`$$ElectronChargeSI;
$$ElectronChargeCGS		= Tool`Constants`Private`$$ElectronChargeCGS;
$$SpeedOfLightSI		= Tool`Constants`Private`$$SpeedOfLightSI;

(* Get Helpers *)
Needs["Tool`Helpers`"];
ClearAll[$FailureFunctionSignature, $JouleToEV];
$$FailureFunctionSignature	= Tool`Helpers`Private`$FailureFunctionSignature;
$JouleToEV					= Tool`Helpers`Private`$JouleToEV;

(* Get Semiconductor Parameters *)
Needs["Tool`Semiconductors`"];
ClearAll[$EffectiveMass, $BohrRadius, $RydbergEnergy, $GapEnergy];
$EffectiveMass			= Tool`Semiconductors`Private`$EffectiveMass;
$BohrRadius				= Tool`Semiconductors`Private`$BohrRadius;
$RydbergEnergy			= Tool`Semiconductors`Private`$RydbergEnergy;
$GapEnergy              = Tool`Semiconductors`Private`$GapEnergy;

(* Get Potentials *)
Needs["Tool`Potentials`"];
ClearAll[MoshinskyConfiniment, MoshinskyConfiniment2];
MoshinskyConfiniment       	= Tool`Potentials`Private`MoshinskyConfiniment;
MoshinskyConfiniment2		= Tool`Potentials`Private`MoshinskyConfiniment2;

(* Conical Quantum Dot *)
ClearAll[StronglyProlateConicalQuantumDot, StronglyOblateConicalQuantumDot];
StronglyProlateConicalQuantumDot[Semiconductor_, BaseRadius_, Height_, {ElectricField_, MagneticField_}, {MagneticNumber_, RadialNumber_, AxialNumber_}] :=
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
				$JouleToEV[
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
StronglyProlateConicalQuantumDot[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`StronglyProlateConicalQuantumDot"];

StronglyOblateConicalQuantumDot[Semiconductor_, BaseRadius_, Height_, {ElectricField_, MagneticField_}, {MagneticNumber_, RadialNumber_, AxialNumber_}] :=
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
				$JouleToEV[Pi^2 * PlanckConstantSI^2 * AxialNumber / (2 * EffectiveMass[#] * BohrRadius[#]^2 * Height^2)]
				,
				Sqrt[2/(Height * (1 - 0.1))] * Sin[Pi * AxialNumber * z /(Height * (1 - 0.1))]
			} &;

			radialEigensystem = {
				$JouleToEV[
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
StronglyOblateConicalQuantumDot[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`StronglyOblateConicalQuantumDot"];

(* Moshinsky 1D/2D in Quantum Dot *)
ClearAll[StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D, BiconvexLensQuantumDotWithMoshinsky2D];
StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D[Semiconductor_, Gas_, SemiAxes_, Interaction_, ParticlesNumber_, {ElectricField_, MagneticField_}, {COMNumber_, RelNumber_}] :=
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
				$JouleToEV @ Divide[
					ParticlesNumber * PlanckConstantSI^2,
					4 * EffectiveMass[#] * BohrRadius[#]^2 * Min @ SemiAxes^2
				] &;
			
			centerOfMassEnergy = $JouleToEV[PlanckConstantSI * First @ Moshinsky[#] * (COMNumber + 1/2)] &;
		
			relativeEnergy = $JouleToEV[PlanckConstantSI * First @ Moshinsky[#] * Min @ Last @ Moshinsky[#] * ((ParticlesNumber-1)*RelNumber+(ParticlesNumber-1)/2)] &;

			waveFunction = Times[
				1/Sqrt[2^COMNumber * COMNumber!] * (1/Pi) ^ 0.25 * Exp[-Sqrt[ParticlesNumber]^2 * z^2 / 2] * HermiteH[COMNumber, Sqrt[ParticlesNumber] * z]
				,
				Times[
					1/Sqrt[2^RelNumber * RelNumber!] * (Min @ Last[Moshinsky[#]]/Pi) ^ 0.25,
					Exp[-(Sqrt[(ParticlesNumber - 1)/ParticlesNumber] * z - 1/(ParticlesNumber - 1)*(ParticlesNumber - 1) * z)^2 / 2],
					HermiteH[RelNumber, Sqrt[Last[Moshinsky[#]]] * (Sqrt[(ParticlesNumber - 1)/ParticlesNumber] * z - 1/(ParticlesNumber - 1) * (ParticlesNumber - 1) * z)]
				] ^ (ParticlesNumber - 1)
			] &;

			totalEigensystem = Association[
				"Ground" -> groundEnergy[#]
				,
				"CenterOfMass" -> centerOfMassEnergy[#]
				,
				"Relative" -> relativeEnergy[#]
				,
				"WaveFunction" -> waveFunction[#]
				,
				"Property" -> <|
					"Semiconductor" -> Semiconductor,
					"Gas" -> Gas,
					"Geometry" -> SemiAxes,
					"Interaction" -> Interaction,
					"Particles" -> ParticlesNumber,
					"ElectricField" -> ElectricField,
					"MagneticField" -> MagneticField
				|>
			] & [Gas]
		]
	];
StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`StronglyProlateEllipsoidalQuantumDotWithMoshinsky1D"];

BiconvexLensQuantumDotWithMoshinsky2D[Semiconductor_, Gas_, radii_, heights_, Interaction_, ParticlesNumber_, {ElectricField_, MagneticField_}, {COMNumber_, RelNumber_}] :=
	Catch @ Block[
		{
			PlanckConstantSI = QuantityMagnitude @ $$PlanckConstantSI,
			
			BohrRadius       = QuantityMagnitude @ $BohrRadius[Semiconductor, #] &,
			EffectiveMass    = QuantityMagnitude @ $EffectiveMass[Semiconductor, #] &,
			
			Moshinsky        = MoshinskyConfiniment2[Semiconductor, #, Interaction, radii, heights, ParticlesNumber] &,
			
			groundEnergy, centerOfMassEnergy, relativeEnergy, waveFunction, totalEigensystem
		},

		With[
			{
				z = Global`z
			},

			groundEnergy =
				$JouleToEV @ Divide[
					2 * ParticlesNumber * PlanckConstantSI^2,
					4 * EffectiveMass[#] * BohrRadius[#]^2 * Min @ heights^2
				] &;
			
		
			centerOfMassEnergy = $JouleToEV[PlanckConstantSI * First @ Moshinsky[#] * (2*COMNumber + 1)] &;
		
			relativeEnergy = $JouleToEV[PlanckConstantSI * First @ Moshinsky[#] * Min @ Last @ Moshinsky[#] * (2*(ParticlesNumber-1)*RelNumber+ParticlesNumber-1)] &;

			waveFunction = Times[
				1/Sqrt[2^COMNumber * COMNumber!] * (1/Pi) ^ 0.25 * Exp[-Sqrt[ParticlesNumber]^2 * z^2 / 2] * HermiteH[COMNumber, Sqrt[ParticlesNumber] * z]
				,
				Times[
					1/Sqrt[2^RelNumber * RelNumber!] * (Min @ Last[Moshinsky[#]]/Pi) ^ 0.25,
					Exp[-(Sqrt[(ParticlesNumber - 1)/ParticlesNumber] * z - 1/(ParticlesNumber - 1)*(ParticlesNumber - 1) * z)^2 / 2],
					HermiteH[RelNumber, Sqrt[Last[Moshinsky[#]]] * (Sqrt[(ParticlesNumber - 1)/ParticlesNumber] * z - 1/(ParticlesNumber - 1) * (ParticlesNumber - 1) * z)]
				] ^ (ParticlesNumber - 1)
			] &;

			totalEigensystem = Association[
				"Ground" -> groundEnergy[#]
				,
				"CenterOfMass" -> centerOfMassEnergy[#]
				,
				"Relative" -> relativeEnergy[#]
				,
				"WaveFunction" -> waveFunction[#] * waveFunction[#]
				,
				"Property" -> <|
					"Semiconductor" -> Semiconductor,
					"Gas" -> Gas,
					"Geometry" -> <|"Radii" -> radii, "Heights" -> heights|>,
					"Interaction" -> Interaction,
					"Particles" -> ParticlesNumber,
					"ElectricField" -> ElectricField,
					"MagneticField" -> MagneticField
				|>
			] & [Gas]
		]
	];
BiconvexLensQuantumDotWithMoshinsky2D[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`BiconvexLensQuantumDotWithMoshinsky2D"];

(* Lens-Shaped Quantum Dot *)
ClearAll[BiconvexLensQuantumDot];
BiconvexLensQuantumDot[Semiconductor_, Radiuses_, Heights_, {ElectricField_, MagneticField_}, {MagneticNumber_, RadialNumber_, AxialNumber_}] :=
	Block[
		{
			PlanckConstantSI 	= QuantityMagnitude @ $$PlanckConstantSI,
			ElectronChargeCGS 	= QuantityMagnitude @ $$ElectronChargeCGS,
			SpeedOfLightSI 		= QuantityMagnitude @ $$SpeedOfLightSI,
			EffectiveMass 		= QuantityMagnitude @ $EffectiveMass[Semiconductor, #] &,
			BohrRadius 			= QuantityMagnitude @ $BohrRadius[Semiconductor, #] &,
			GapEnergy 			= QuantityMagnitude @ $GapEnergy[InitialState["Semiconductor"], temperature],
			,
			hB = Max @ Heights, hS = Min @ Heights, rB = Max @ Radiuses, rS = Min @ Radiuses
			,
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
				$JouleToEV[Pi^2 * PlanckConstantSI^2 * AxialNumber^2 / (2 * EffectiveMass[#] * (heightBig[#] + heightSmall[#])^2)],
				Sqrt[2 * BohrRadius[#] / wellDepth[#]] * Sin[Pi * AxialNumber / wellDepth[#] * (dimZ[#] + Sqrt[radiusSmall[#]^2 - dimR[#]^2] + heightSmall[#] - radiusSmall[#])]
			} &;

			radialEigensystem = {
				$JouleToEV[PlanckConstantSI * problemFrequency[#] * (2*RadialNumber+Abs[MagneticNumber]+1) + PlanckConstantSI*cyclotronFrequency[#] * MagneticNumber/2],
				Times[
					1/Sqrt[2*Pi] * Exp[I*MagneticNumber*2*Pi],
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
		];

		Append[
			totalEigensystem,
			{
				"Geometry" -> <|
					"Axial" -> Heights,
					"Radial" -> Radiuses
				|>
				,
				"Semiconductor" -> Semiconductor
			}
		]
	];
BiconvexLensQuantumDot[___] := $$FailureFunctionSignature["Tool`Eigensystems`Private`BiconvexLensQuantumDot"];

End[];
EndPackage[];