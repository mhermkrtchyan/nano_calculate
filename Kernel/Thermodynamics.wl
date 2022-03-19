BeginPackage["Tool`Thermodynamics`"];

Block[{$ContextPath}, Needs["GeneralUtilities`"];];

ClearAll[GetThermodynamicParameter];
GeneralUtilities`SetUsage[GetThermodynamicParameter,
"GetThermodynamicParameter[Model$, Characteristic$]
     This function gives programmatically acses to shows IP features measured results comparison for given IPlibrary$, IPcategory$ and feature$.

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
			partitionFunction, parameter
		},

		
		With[
			{
				temperature = Global`temperature,
				COMNumber = Global`COMNumber, RelNumber = Global`RelNumber
			},

			eigensystem = Model[COMNumber, RelNumber];
			
			particlesNumber = eigensystem["Property", "Particles"];

			inverseTemperature = $JouleToEV[BoltzmannConstantSI * temperature]^-1;

			partitionFunction = Times[
				Exp[-inverseTemperature * eigensystem["Ground"]],
				Sum[Exp[-inverseTemperature * eigensystem["CenterOfMass"]], {COMNumber, 0, Infinity}],
				Power[
					Sum[Exp[-inverseTemperature * eigensystem["Relative"]], {RelNumber, 0, Infinity}],
					particlesNumber * (particlesNumber - 1)
				]
			];

			parameter = Switch[Characteristic
				,
				"PartitionFunction",
				partitionFunction
				,
				"MeanEnergy",
				$JouleToEV[
					BoltzmannConstantSI  * temperature^2 * D[Log @ partitionFunction, temperature]
				] 
				,
				"FreeEnergy",
				-$JouleToEV[
					BoltzmannConstantSI * temperature * Log @ partitionFunction
				]
				,
				"Entropy",
				-D[
					-$JouleToEV[
						BoltzmannConstantSI * temperature * Log @ partitionFunction
					],
					temperature
				]
				,
				"HeatCapacity",
				D[
					$JouleToEV[
						BoltzmannConstantSI  * temperature^2 * D[Log @ partitionFunction, temperature]
					],
					temperature
				] 
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