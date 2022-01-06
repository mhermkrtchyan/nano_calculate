BeginPackage["Tool`Semiconductors`"];
Begin["`Private`"];

(*
	Get constants
*)

Needs["Tool`Constants`"];

ClearAll[$$ElectronMassSI, $$PlanckConstantSI, $$PlanckConstantCGS, $$ElectronChargeCGS];
$$ElectronMassSI			= Tool`Constants`Private`$$ElectronMassSI;
$$PlanckConstantSI			= Tool`Constants`Private`$$PlanckConstantSI;
$$PlanckConstantCGS			= Tool`Constants`Private`$$PlanckConstantCGS;
$$ElectronChargeCGS			= Tool`Constants`Private`$$ElectronChargeCGS;

(*
	Get helpers
*)

Needs["Tool`Helpers`"];

ClearAll[$$FailureFunctionSignature];
$FailureFunctionSignature	= Tool`Helpers`Private`$FailureFunctionSignature;

ClearAll[$JouleToEV];
$JouleToEV					= Tool`Helpers`Private`$JouleToEV;

(*
	Semiconductor parameters
*)

ClearAll[GapEnergy];
$GapEnergy[semiconductor_?StringQ, temperature_?NumberQ] :=
	Switch[semiconductor,
		"InAs",
		Quantity[0.415 - 2.76*10^-4*temperature^2/(temperature+83)									, "Electronvolts"]
		,
		"GaAs",
		Quantity[1.519 - 5.405*10^-4*temperature^2/(temperature+204)								, "Electronvolts"]
		,
		"InP",
		Quantity[1.421 - 4.9*10^-4*temperature^2/(temperature+327)									, "Electronvolts"]
		,
		"InSb",
		Quantity[0.24 - 6*10^-4*temperature^2/(temperature+500)										, "Electronvolts"]
		,
		"Ge",
		Quantity[0.742- 4.8*10^-4*temperature^2/(temperature+235)									, "Electronvolts"]
		,
		"Si",
		Quantity[1.17 - 4.73*10^-4*temperature^2/(temperature+636)									, "Electronvolts"]
		,
		"GeSi",
		Quantity[1.912+temperature^2*(-(4.8*10^-4/(235+temperature))-4.73*10^-4/(636+temperature))	, "Electronvolts"]
	];
$GapEnergy[___] := $FailureFunctionSignature["Tool`Semiconductors`Private`$GapEnergy"];

ClearAll[DielectricConstant];
$DielectricConstant[semiconductor_?StringQ] :=
	Switch[semiconductor,
		"InAs",
		15.5
		,
		"GaAs",
		12.9
		,
		"InP",
		12.5
		,
		"InSb",
		16.8
		,
		"Ge",
		16.2
		,
		"Si",
		11.7
		,
		"GeSi",
		14.4
	];
$DielectricConstant[___] := $FailureFunctionSignature["Tool`Semiconductors`Private`$DielectricConstant"];

ClearAll[EffectiveMass];
$EffectiveMass[semiconductor_?StringQ, particle_?StringQ] :=
	Switch[{semiconductor, particle},
		{"InAs", "Electron"},
		0.023*$$ElectronMassSI
		,
		{"InAs", "Light Hole"},
		0.026*$$ElectronMassSI
		,
		{"InAs", "Heavy Hole"},
		0.41*$$ElectronMassSI
		,
		{"GaAs", "Electron"},
		0.063*$$ElectronMassSI
		,
		{"GaAs", "Light Hole"},
		0.082*$$ElectronMassSI
		,
		{"GaAs", "Heavy Hole"},
		0.61*$$ElectronMassSI
		,
		{"InP", "Electron"},
		0.08*$$ElectronMassSI
		,
		{"InP", "Light Hole"},
		0.089*$$ElectronMassSI
		,
		{"InP", "Heavy Hole"},
		0.6*$$ElectronMassSI
		,
		{"InSb", "Electron"},
		0.014*$$ElectronMassSI
		,
		{"InSb", "Light Hole"},
		0.015*$$ElectronMassSI
		,
		{"InSb", "Heavy Hole"},
		0.43*$$ElectronMassSI
		,
		{"Ge", "Electron"},
		0.08*$$ElectronMassSI
		,
		{"Ge", "Light Hole"},
		0.09*$$ElectronMassSI
		,
		{"Ge", "Heavy Hole"},
		0.33*$$ElectronMassSI
		,
		{"Si", "Electron"},
		0.18*$$ElectronMassSI
		,
		{"Si", "Light Hole"},
		0.19*$$ElectronMassSI
		,
		{"Si", "Heavy Hole"},
		0.49*$$ElectronMassSI
		,
		{"GeSi", "Electron"},
		0.12*$$ElectronMassSI
		,
		{"GeSi", "Light Hole"},
		0.13*$$ElectronMassSI
		,
		{"GeSi", "Heavy Hole"},
		0.394*$$ElectronMassSI
	];
$EffectiveMass[___] := $FailureFunctionSignature["Tool`Semiconductors`Private`$EffectiveMass"];

ClearAll[BohrRadius];
$BohrRadius[semiconductor_?StringQ, particle_?StringQ] :=
	Block[
		{
			PlanckConstantCGS 	= QuantityMagnitude @ $$PlanckConstantCGS,
			ElectronChargeCGS 	= QuantityMagnitude @ $$ElectronChargeCGS,

			EffectiveMass		= QuantityMagnitude @ $EffectiveMass[semiconductor, particle],
			DielectricConstant	= $DielectricConstant[semiconductor]
		},

		Quantity[
			DielectricConstant * PlanckConstantCGS^2 * 10^-2 / (EffectiveMass * ElectronChargeCGS^2 * 10^3)
			,
			"Meters"
		]
	];
$BohrRadius[___] := $FailureFunctionSignature["Tool`Semiconductors`Private`$BohrRadius"];

ClearAll[RydbergEnergy];
$RydbergEnergy[semiconductor_?StringQ, particle_?StringQ] :=
	Block[
		{
			PlanckConstantSI 	= QuantityMagnitude @ $$PlanckConstantSI,
			EffectiveMass 		= QuantityMagnitude @ $EffectiveMass[semiconductor, particle],
			
			BohrRadius     		= QuantityMagnitude @ $BohrRadius[semiconductor, particle]
		},
		Quantity[
			$JouleToEV[
				PlanckConstantSI^2 / (2 * EffectiveMass * BohrRadius^2)
			]
			,
			"Electronvolts"
		]
	];
$RydbergEnergy[___] := $FailureFunctionSignature["Tool`Semiconductors`Private`$RydbergEnergy"];

End[];
EndPackage[];