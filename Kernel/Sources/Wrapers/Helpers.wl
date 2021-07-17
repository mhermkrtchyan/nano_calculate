BeginPackage["Tool`Helpers`"];
Begin["`Private`"];

(*
	██╗  ██╗███████╗██╗     ██████╗ ███████╗██████╗ ███████╗
	██║  ██║██╔════╝██║     ██╔══██╗██╔════╝██╔══██╗██╔════╝
	███████║█████╗  ██║     ██████╔╝█████╗  ██████╔╝███████╗
	██╔══██║██╔══╝  ██║     ██╔═══╝ ██╔══╝  ██╔══██╗╚════██║
	██║  ██║███████╗███████╗██║     ███████╗██║  ██║███████║
	╚═╝  ╚═╝╚══════╝╚══════╝╚═╝     ╚══════╝╚═╝  ╚═╝╚══════╝
*)

ClearAll[$$ToolOptions];
Options[$$ToolOptions] = {};

(*
	Failures
*)

ClearAll[$FailureFunctionSignature, $FailureQuantumNumber];
$FailureFunctionSignature	:= Failure["Use "<> "?" <> ToString[#] , "Wrong Function Call"] &;
$FailureQuantumNumber 		:= Failure["Possible values: [" <> ToString[#1] <> ", " <> ToString[#2] <> "]", "Wrong in " <> #3] &;

(*
	Directories
*)

ClearAll[$$ToolDir, $$StorageDir, $$EigensystemDir];
$$ToolDir = "C:\\Users\\Acer\\Dropbox\\Article\\_Tool_";
$$StorageDir = FileNameJoin[{$$ToolDir, "Kernel", "Storage", ToString[#] <> ".wl"}] &;
$$EigensystemDir = FileNameJoin[{$$ToolDir, "Kernel", "Storage", "Eigensystem", ToString[#] <> ".wl"}] &;

(*
	Adapters
*)

ClearAll[$JouleToEV];
$JouleToEV[value_] :=
	Block[
		{
			toEV
		},
		
		value * 0.624*10^19
	];
$JouleToEV[___] := $FailureFunctionSignature["Tool`Helpers`Private`$JouleToEV"];

(*
	Math functions
*)

ClearAll[$GaussDistribution];
$GaussDistribution[value_] :=
	Block[
		{
			median = 1, variance = 0.1867
		},

		Times[
			1/(variance * Sqrt[2*Pi]),
			Exp[-(value - median)^2 / (2 * variance^2)]
		]
	];
$GaussDistribution[___] := $FailureFunctionSignature["Tool`Helpers`Private`$GaussDistribution"];

End[];
EndPackage[];