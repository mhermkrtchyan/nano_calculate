BeginPackage["Tool`Potentials`"];
Begin["`Private`"];

(*
	Get constants
*)

Needs["Tool`Constants`"];

ClearAll[$$PlanckConstantSI, $$ElectronChargeSI, $$VacuumPremittivitySI];
$$PlanckConstantSI		= Tool`Constants`Private`$$PlanckConstantSI;
$$ElectronChargeSI 		= Tool`Constants`Private`$$ElectronChargeSI;
$$VacuumPremittivitySI	= Tool`Constants`Private`$$VacuumPremittivitySI;

(*
	Get helpers
*)

Needs["Tool`Helpers`"];

ClearAll[$$FailureFunctionSignature];
$FailureFunctionSignature	= Tool`Helpers`Private`$FailureFunctionSignature;

(*
	Get semiconductor parameters
*)

Needs["Tool`Semiconductors`"];

ClearAll[$EffectiveMass, $BohrRadius];
$EffectiveMass	= Tool`Semiconductors`Private`$EffectiveMass;
$BohrRadius 	= Tool`Semiconductors`Private`$BohrRadius;

(*
	Confiniments
*)

ClearAll[ParabolicConfinement];
ParabolicConfinement[semiconductor_?StringQ, direction_?StringQ, frequency_?NumberQ] :=
	Block[
		{
			EffectiveMass = QuantityMagnitude @ $EffectiveMass[semiconductor, "Electron"]
		},

		With[
			{
				z = Global`z,
				r = Global`r
			},

			Switch[direction
				,
				"Axial",
				EffectiveMass * frequency^2 * z^2 / 2
				,
				"Radial",
				EffectiveMass * frequency^2 * r^2 / 2
			]
		]
	];
ParabolicConfinement[___] := $FailureFunctionSignature["Tool`Potentials`Private`ParabolicConfinement"];

ClearAll[MPTConfinement];
MPTConfinement[semiconductor_?StringQ, direction_?StringQ, wellDepth_?NumberQ, wellHalfWidth_?NumberQ] :=
	With[
		{
			z = Global`z,
			r = Global`r
		},
		
		Switch[direction
			,
			"Axial",
			wellDepth - wellDepth / Cosh[z / wellHalfWidth]^2
			,
			"Radial",
			wellDepth - wellDepth / Cosh[r / wellHalfWidth]^2
		]
	];
MPTConfinement[___] := $FailureFunctionSignature["Tool`Potentials`Private`MPTConfinement"];

ClearAll[DoubleMPTConfinement];
DoubleMPTConfinement[semiconductor_?StringQ, direction_?StringQ, wellsDepth_?ListQ, wellsHalfWidth_?ListQ, wellsDistance_?NumberQ] :=
	With[
		{
			z = Global`z,
			r = Global`r
		},

		Switch[direction
			,
			"Axial",
			Plus[
				First@wellsDepth - First@wellsDepth / Cosh[(z + wellsDistance) / First@wellsHalfWidth]^2,
				Last@wellsDepth - Last@wellsDepth / Cosh[(z - wellsDistance) / Last@wellsHalfWidth]^2
			]
			,
			"Radial",
			Plus[
				First@wellsDepth - First@wellsDepth / Cosh[(r + wellsDistance) / First@wellsHalfWidth]^2,
				Last@wellsDepth - Last@wellsDepth / Cosh[(r - wellsDistance) / Last@wellsHalfWidth]^2
			]
		]
	];
DoubleMPTConfinement[___] := $FailureFunctionSignature["Tool`Potentials`Private`DoubleMPTConfinement"];

ClearAll[MorseConfinement];
MorseConfinement[semiconductor_?StringQ, direction_?StringQ, wellDepth_?NumberQ, wellHalfWidth_?NumberQ] :=
	With[
		{
			z = Global`z,
			r = Global`r
		},

		Switch[direction
			,
			"Axial",
			wellDepth + wellDepth * E^(-2 * z / wellHalfWidth) -2 * E^(- z / wellHalfWidth)
			,
			"Radial",
			wellDepth + wellDepth * E^(-2 * r / wellHalfWidth) -2 * E^(- r / wellHalfWidth)
		]
	];
MorseConfinement[___] := $FailureFunctionSignature["Tool`Potentials`Private`MorseConfinement"];

ClearAll[DoubleMorseConfinement];
DoubleMorseConfinement[semiconductor_?StringQ, direction_?StringQ, wellsDepth_?ListQ, wellsHalfWidth_?ListQ, wellsDistance_?NumberQ] :=
	With[
		{
			z = Global`z,
			r = Global`r
		},

		Switch[direction
			,
			"Axial",
			Plus[
				First@wellsDepth + First@wellsDepth * (Exp[-2*(z + wellsDistance)/First@wellsHalfWidth] -2*Exp[-(z + wellsDistance)/First@wellsHalfWidth]),
				Last@wellsDepth + Last@wellsDepth * (Exp[-2*(wellsDistance-z)/Last@wellsHalfWidth] -2*Exp[-(wellsDistance - z)/Last@wellsHalfWidth])	
			]
			,
			"Radial",
			Plus[
				First@wellsDepth + First@wellsDepth * (Exp[-2*(r + wellsDistance)/First@wellsHalfWidth] -2*Exp[-(r + wellsDistance)/First@wellsHalfWidth]),
				Last@wellsDepth + Last@wellsDepth * (Exp[-2*(wellsDistance-r)/Last@wellsHalfWidth] -2*Exp[-(wellsDistance - r)/Last@wellsHalfWidth])	
			]
		]
	];
DoubleMorseConfinement[___] := $FailureFunctionSignature["Tool`Potentials`Private`DoubleMorseConfinement"];

ClearAll[KratzerConfinement];
KratzerConfinement[semiconductor_?StringQ, direction_?StringQ, wellDepth_?NumberQ, wellHalfWidth_?NumberQ] :=
	With[
		{
			z = Global`z,
			r = Global`r
		},

		Switch[direction
			,
			"Axial",
			wellHalfWidth / (2 * z^2) - wellHalfWidth / z + wellDepth
			,
			"Radial",
			wellHalfWidth / (2 * r^2) - wellHalfWidth / r + wellDepth
		]
	];
KratzerConfinement[___] := $FailureFunctionSignature["Tool`Potentials`Private`KratzerConfinement"];

(*
	Interaction potentials 
*)

ClearAll[MoshinskyConfiniment];
MoshinskyConfiniment[semiconductor_?StringQ, particle_?StringQ, interactionParameter_?NumberQ, semiAxes_?ListQ, particlesNumber_?NumberQ] :=
	Block[
		{
			PlanckConstantSI 	= QuantityMagnitude @ $$PlanckConstantSI,
			BohrRadius 			= QuantityMagnitude @ $BohrRadius[semiconductor, particle],
			EffectiveMass   	= QuantityMagnitude @ $EffectiveMass[semiconductor, particle]
			,
			frequency, parameter
		},

		frequency = 0.7 * PlanckConstantSI * N @ BesselJZero[0, 1] / (BohrRadius^2 * EffectiveMass * Max @ semiAxes * Min @ semiAxes);

		parameter =
			Sqrt[
				1 + 2 * interactionParameter / (EffectiveMass * frequency^2) * particlesNumber
			];

		{frequency, parameter}
	];	
MoshinskyConfiniment[___] := $FailureFunctionSignature["Tool`Potentials`Private`MoshinskyConfiniment"];

ClearAll[MoshinskyConfiniment2];
MoshinskyConfiniment2[semiconductor_?StringQ, particle_?StringQ, interactionParameter_?NumberQ, radii_?ListQ, heights_?ListQ, particlesNumber_?NumberQ] :=
	Block[
		{
			PlanckConstantSI 	= QuantityMagnitude @ $$PlanckConstantSI,
			BohrRadius 			= QuantityMagnitude @ $BohrRadius[semiconductor, particle],
			EffectiveMass   	= QuantityMagnitude @ $EffectiveMass[semiconductor, particle]
			,
			frequency, parameter
		},

		frequency = Sqrt @ Divide[
			Pi * PlanckConstantSI * BohrRadius * (First @ radii + Last @ radii),
			BohrRadius^5 * EffectiveMass * First @ radii * Last @ radii * (First @ heights + Last @ heights)^3
		];

		parameter =
			Sqrt[
				1 + 2 * interactionParameter / (EffectiveMass * frequency^2) * particlesNumber
			];

		{frequency, parameter}
	];	
MoshinskyConfiniment2[___] := $FailureFunctionSignature["Tool`Potentials`Private`MoshinskyConfiniment"];

ClearAll[MoshinskyInteraction];
MoshinskyInteraction[semiconductor_?StringQ, particle_?StringQ, interactionParameter_?NumberQ, particlesDistance_?NumberQ]:=
	Block[
		{
			BohrRadius = QuantityMagnitude @ $BohrRadius[semiconductor, particle]
		}
		,
		0.5 * interactionParameter * BohrRadius^2 * particlesDistance^2 
	];
MoshinskyInteraction[___] := $FailureFunctionSignature["Tool`Potentials`Private`MoshinskyInteraction"];

ClearAll[CoulombInteraction];
CoulombInteraction[semiconductor_?StringQ, particle_?StringQ, interactionParameter_?NumberQ, particlesDistance_?NumberQ]:=
	Block[
		{
			ElectronChargeSI 		= QuantityMagnitude @ $$ElectronChargeSI[semiconductor, particle],
			VacuumPremittivitySI 	= QuantityMagnitude @ $$VacuumPremittivitySI[semiconductor, particle],
			BohrRadius 				= QuantityMagnitude @ $BohrRadius[semiconductor, particle]
		}
		,
		ElectronChargeSI^2 / (2 * 4 * Pi * VacuumPremittivitySI * BohrRadius * particlesDistance)
	]		
CoulombInteraction[___] := $FailureFunctionSignature["Tool`Potentials`Private`CoulombInteraction"];

End[];
EndPackage[];