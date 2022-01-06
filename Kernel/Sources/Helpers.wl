BeginPackage["Tool`Helpers`"];
Begin["`Private`"];

(*
	Failures
*)

ClearAll[$FailureFunctionSignature, $FailureQuantumNumber];
$FailureFunctionSignature	:= Failure["Use "<> "?" <> ToString[#] , "Wrong Function Call"] &;
$FailureQuantumNumber 		:= Failure["Possible values: [" <> ToString[#1] <> ", " <> ToString[#2] <> "]", "Wrong in " <> #3] &;

(*
	Directories
*)

ClearAll[$$ToolDir, $$EigensystemDir];
$$ToolDir	= FileNameJoin[{FileNameDrop[$InputFileName, -4]}];

(*
	Adapters
*)

ClearAll[$JouleToEV];
$JouleToEV[value_] := value * 0.624*10^19;
$JouleToEV[___] := $FailureFunctionSignature["Tool`Helpers`Private`$JouleToEV"];

(*
	Mathematical functions
*)

ClearAll[$GaussDistribution];
$GaussDistribution[value_] :=
	Block[
		{
			median = 1,
			variance = 0.1867
		},

		Times[
			Divide[
				1,
				variance * Sqrt[2*Pi]
			],
			Exp @ Divide[
				-(value - median)^2,
				2 * variance^2
			]
		]
	];
$GaussDistribution[___] := $FailureFunctionSignature["Tool`Helpers`Private`$GaussDistribution"];

End[];
EndPackage[];