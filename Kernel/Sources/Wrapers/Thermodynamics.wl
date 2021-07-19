BeginPackage["Tool`Thermodynamics`"];
Begin["`Private`"]

(*	
	██████╗ ███████╗███████╗██╗███╗   ██╗██╗████████╗██╗ ██████╗ ███╗   ██╗███████╗
	██╔══██╗██╔════╝██╔════╝██║████╗  ██║██║╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
	██║  ██║█████╗  █████╗  ██║██╔██╗ ██║██║   ██║   ██║██║   ██║██╔██╗ ██║███████╗
	██║  ██║██╔══╝  ██╔══╝  ██║██║╚██╗██║██║   ██║   ██║██║   ██║██║╚██╗██║╚════██║
	██████╔╝███████╗██║     ██║██║ ╚████║██║   ██║   ██║╚██████╔╝██║ ╚████║███████║
	╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝
*)

Needs["Tool`Constants`"];

ClearAll[$$BoltzmannConstantSI];
$$BoltzmannConstantSI      = Tool`Constants`Private`$$BoltzmannConstantSI;

Needs["Tool`Helpers`"];

ClearAll[$$FailureFunctionSignature];
$$FailureFunctionSignature = Tool`Helpers`Private`$FailureFunctionSignature;

ClearAll[$JouleToEV];
$JouleToEV                  = Tool`Helpers`Private`$JouleToEV;

Needs["Tool`Eigensystems`"];

ClearAll[tempStronglyProlateEllipsoidalQuantumDotWithMoshinsky1D];
tempStronglyProlateEllipsoidalQuantumDotWithMoshinsky1D = Tool`Eigensystems`Private`tempStronglyProlateEllipsoidalQuantumDotWithMoshinsky1D;

(*	
	████████╗██╗  ██╗███████╗██████╗ ███╗   ███╗ ██████╗ ██████╗ ██╗   ██╗███╗   ██╗ █████╗ ███╗   ███╗██╗ ██████╗███████╗
	╚══██╔══╝██║  ██║██╔════╝██╔══██╗████╗ ████║██╔═══██╗██╔══██╗╚██╗ ██╔╝████╗  ██║██╔══██╗████╗ ████║██║██╔════╝██╔════╝
	   ██║   ███████║█████╗  ██████╔╝██╔████╔██║██║   ██║██║  ██║ ╚████╔╝ ██╔██╗ ██║███████║██╔████╔██║██║██║     ███████╗
	   ██║   ██╔══██║██╔══╝  ██╔══██╗██║╚██╔╝██║██║   ██║██║  ██║  ╚██╔╝  ██║╚██╗██║██╔══██║██║╚██╔╝██║██║██║     ╚════██║
	   ██║   ██║  ██║███████╗██║  ██║██║ ╚═╝ ██║╚██████╔╝██████╔╝   ██║   ██║ ╚████║██║  ██║██║ ╚═╝ ██║██║╚██████╗███████║
	   ╚═╝   ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝ ╚═════╝ ╚═════╝    ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝ ╚═════╝╚══════╝
*)

ClearAll[GetThermodynamicParameterTemperatureDependecy];
GetThermodynamicParameterTemperatureDependecy[semiconductor_, sizes_, interaction_, particlesNumber_, {ElectricField_, MagneticField_}, gas_, thermodynamicParameter_] := 
	Block[
		{
			BoltzmannConstantSI = QuantityMagnitude @ $$BoltzmannConstantSI,

			eigensystem, inverseTemperature, partitionFunction
		},

		eigensystem = tempStronglyProlateEllipsoidalQuantumDotWithMoshinsky1D[
			semiconductor,
			sizes,
			interaction,
			particlesNumber,
			{ElectricField, MagneticField},
			{COMNumber, RelNumber}
		][gas];

		With[
			{
				temperature = Global`temperature
			},

			inverseTemperature = $JouleToEV[BoltzmannConstantSI * temperature]^-1;

			partitionFunction = Times[
				Exp[-inverseTemperature * eigensystem["Ground"]],
				Sum[Exp[-inverseTemperature * eigensystem["CenterOfMass"]], {COMNumber, 0, Infinity}],
				Power[
					Sum[Exp[-inverseTemperature * eigensystem["Relative"]], {RelNumber, 0, Infinity}],
					particlesNumber * (particlesNumber - 1)
				]
			];

			Switch[thermodynamicParameter,
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
			]
		]
	];
GetThermodynamicParameterTemperatureDependecy[___] := $$FailureFunctionSignature["Dependencies`Private`GetThermodynamicParameter"];

ClearAll[GetThermodynamicParameterSizeDependecy];
GetThermodynamicParameterSizeDependecy[semiconductor_, absoluteTemperature_, interaction_, particlesNumber_, {ElectricField_, MagneticField_}, gas_, thermodynamicParameter_] := 
	Block[
		{
			BoltzmannConstantSI = QuantityMagnitude @ $$BoltzmannConstantSI,

			eigensystem, inverseTemperature, partitionFunction
		},

		With[
			{
				semiaxis = Global`semiaxis
			},

			eigensystem = tempStronglyProlateEllipsoidalQuantumDotWithMoshinsky1D[
				semiconductor,
				{semiaxis, 5 * semiaxis},
				interaction,
				particlesNumber,
				{ElectricField, MagneticField},
				{COMNumber, RelNumber}
			][gas];

			inverseTemperature = $JouleToEV[BoltzmannConstantSI * temperature]^-1;

			partitionFunction = Times[
				Exp[-inverseTemperature * eigensystem["Ground"]],
				Sum[Exp[-inverseTemperature * eigensystem["CenterOfMass"]], {COMNumber, 0, Infinity}],
				Power[
					Sum[Exp[-inverseTemperature * eigensystem["Relative"]], {RelNumber, 0, Infinity}],
					particlesNumber - 1
				]
			];

			Switch[thermodynamicParameter,
				"PartitionFunction",
				ReplaceAll[
					partitionFunction
					,
					temperature -> absoluteTemperature
				]
				,
				"MeanEnergy",
				ReplaceAll[
					$JouleToEV[
						BoltzmannConstantSI  * temperature^2 * D[Log @ partitionFunction, temperature]
					]
					,
					temperature -> absoluteTemperature
				] 
				,
				"FreeEnergy",
				ReplaceAll[
					-$JouleToEV[
						BoltzmannConstantSI * temperature * Log @ partitionFunction
					]
					,
					temperature -> absoluteTemperature
				]
				,
				"Entropy",
				ReplaceAll[
					-D[
						-$JouleToEV[
							BoltzmannConstantSI * temperature * Log @ partitionFunction
						],
						temperature
					]
					,
					temperature -> absoluteTemperature
				]
				,
				"HeatCapacity",
				ReplaceAll[
					D[
						$JouleToEV[
							BoltzmannConstantSI  * temperature^2 * D[Log @ partitionFunction, temperature]
						],
						temperature
					]
					,
					temperature -> absoluteTemperature
				] 
			]
		]
	];
GetThermodynamicParameterSizeDependecy[___] := $$FailureFunctionSignature["Dependencies`Private`GetThermodynamicParameter"];

End[];
EndPackage[];