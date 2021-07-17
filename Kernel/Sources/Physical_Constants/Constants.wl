BeginPackage["Tool`Constants`"];
Begin["`Private`"];

(*
	 ██████╗ ██████╗ ███╗   ██╗███████╗████████╗ █████╗ ███╗   ██╗████████╗███████╗
	██╔════╝██╔═══██╗████╗  ██║██╔════╝╚══██╔══╝██╔══██╗████╗  ██║╚══██╔══╝██╔════╝
	██║     ██║   ██║██╔██╗ ██║███████╗   ██║   ███████║██╔██╗ ██║   ██║   ███████╗
	██║     ██║   ██║██║╚██╗██║╚════██║   ██║   ██╔══██║██║╚██╗██║   ██║   ╚════██║
	╚██████╗╚██████╔╝██║ ╚████║███████║   ██║   ██║  ██║██║ ╚████║   ██║   ███████║
	 ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝
*)

ClearAll[$$ElectronChargeSI, $$PlanckConstantSI, $$ElectronMassSI, $$BoltzmannConstantSI, $$SpeedOfLightSI, $$VacuumPremittivitySI];
$$ElectronChargeSI     	= Quantity[1.602*10^-19   , "Coulombs"        		];
$$PlanckConstantSI     	= Quantity[1.054*10^-34   , "Joules"*"Seconds"		];
$$ElectronMassSI      	= Quantity[9.1*10^-31     , "Kilograms"				];
$$BoltzmannConstantSI  	= Quantity[1.38*10^-23    , "Joules"/"Kelvins"		];
$$SpeedOfLightSI       	= Quantity[3*10^8         , "Meters"/"Seconds"		];
$$VacuumPremittivitySI	= Quantity[8.85*10^-12    , "Farads"/"Meters"		];

ClearAll[$$ElectronChargeCGS, $$PlanckConstantCGS, $$ElectronMassCGS, $$BoltzmannConstantCGS, $$SpeedOfLightCGS];
$$ElectronChargeCGS    	= Quantity[3.33*10^-10    , "StatCoulombs"         	];
$$PlanckConstantCGS    	= Quantity[1.054*10^-27   , "Ergs"*"Seconds"       	];
$$ElectronMassCGS      	= Quantity[9.1*10^-28     , "Grams"                	];
$$BoltzmannConstantCGS 	= Quantity[1.38*10^-16    , "Ergs"/"Kelvins"       	];
$$SpeedOfLightCGS      	= Quantity[3*10^10        , "Centimeters"/"Seconds"	];

End[];
EndPackage[];