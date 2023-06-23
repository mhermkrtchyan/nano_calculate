BeginPackage["Tool`Thermodynamics`"];

Block[{$ContextPath}, Needs["GeneralUtilities`"];];

ClearAll[GetThermodynamicParameter];
GeneralUtilities`SetUsage[GetThermodynamicParameter,
"GetThermodynamicParameter[Model$, Characteristic$]
	This function gives programmatically acses to thermodynamic parameter temparature dependency.

Arguments:
| Model$   | Dataset with quantum dot eigensystem and other properties |
| Characteristic$   | \"PartitionFunction\", \"MeanEnergy\", \"FreeEnergy\", \"Entropy\" or \"HeatCapacity\" |
"
];

Begin["`Private`"]

Needs["Tool`Constants`"];

ClearAll[$$BoltzmannConstantSI];
$$BoltzmannConstantSI      = Tool`Constants`Private`$$BoltzmannConstantSI;

Needs["Tool`Helpers`"];

ClearAll[$$FailureFunctionSignature, $JouleToEV];
$$FailureFunctionSignature 	= Tool`Helpers`Private`$FailureFunctionSignature;
$JouleToEV                  = Tool`Helpers`Private`$JouleToEV;

(*
	Get thermodynamics parameters
*)

GetThermodynamicParameter[Model_, Characteristic_] := 
	Block[
		{
			BoltzmannConstantSI = QuantityMagnitude @ $$BoltzmannConstantSI,
			eigensystem, particlesNumber,
			temperature, inverseTemperature,
			partitionFunction, parameter,
			meanEnergy, heatCap
		},

		With[
			{
				temperature = Global`temperature,
				COMNumber = Global`COMNumber, RelNumber = Global`RelNumber
			},

			eigensystem = Model[COMNumber, RelNumber];
			
			particlesNumber = eigensystem["Property", "Particles"];

			partitionFunction = Times[
				1/Factorial[particlesNumber]
				,
				Times[
					Exp[-inverseTemperature * eigensystem["Ground"]],
					Sum[Exp[-inverseTemperature * eigensystem["CenterOfMass"]], {COMNumber, 0, Infinity}],
					Sum[Exp[-inverseTemperature * eigensystem["Relative"]], {RelNumber, 0, Infinity}]
				]^particlesNumber
			];

			meanEnergy = - D[Log[partitionFunction], inverseTemperature];

			inverseTemperature = ($JouleToEV[BoltzmannConstantSI] * temperature)^-1;

			heatCap = D[meanEnergy, temperature];

			freeEnergy = - $JouleToEV[BoltzmannConstantSI] * temperature * Log[partitionFunction];

			entropy = - D[freeEnergy, temperature];

			parameter = Switch[Characteristic
				,
				"PartitionFunction",
				partitionFunction
				,
				"MeanEnergy",
				meanEnergy
				,
				"FreeEnergy",
				freeEnergy
				,
				"Entropy",
				entropy
				,
				"HeatCapacity",
				heatCap
			];

			ToExpression[
				StringReplace[
					ToString[parameter, InputForm]
					,
					"temperature" -> "#1"
				] <> "&"
			]
		]
	];
GetThermodynamicParameter[___] := $$FailureFunctionSignature["Dependencies`Private`GetThermodynamicParameter"];

End[];
EndPackage[];