BeginPackage["Tool`Helpers`"];
Begin["`Private`"];

(* Failures and Directories *)
ClearAll[$FailureFunctionSignature, $FailureQuantumNumber, $$ToolDir];
$FailureFunctionSignature	:= Failure["Use "<> "?" <> ToString[#] , "Wrong Function Call"] &;
$FailureQuantumNumber 		:= Failure["Possible values: [" <> ToString[#1] <> ", " <> ToString[#2] <> "]", "Wrong in " <> #3] &
$$ToolDir					 = FileNameJoin[{FileNameDrop[$InputFileName, -4]}];

(* Adapters *)
ClearAll[$JouleToEV];
$JouleToEV[value_] := value * 0.624*10^19;
$JouleToEV[___] := $FailureFunctionSignature["Tool`Helpers`Private`$JouleToEV"];

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

ClearAll[$PlotResults2D];
$PlotResults2D[showData_, range_, {yAxisName_, yAxisDim_}, {xAxisName_, xAxisDim_}, legends_, describers_, theme_, style_, {image_, imagePosition_}, export_] :=
	Block[
		{
			plot 
		},

		plot = ListLinePlot[
			showData
			,
			Frame -> True,
			FrameStyle -> Directive[Black, Thick],
			FrameLabel -> {
				Style[xAxisName <> ", " <> xAxisDim, Italic, Bold, 20],
				Style[yAxisName <> ", " <> yAxisDim, Italic, Bold, 20]
			},
			FrameTicksStyle -> Directive[Black, 20],
			PlotRange -> range,
			PlotRangePadding -> 0,
			PlotStyle -> style,
			PlotTheme -> theme,
			PlotLegends -> Placed[
				LineLegend[
					# & /@ legends,
					LegendFunction -> (
						Framed[
							#,
							RoundingRadius -> 10,
							Background -> Lighter[Gray, 0.95],
							FrameMargins -> 2
						] &
					),
					LabelStyle -> Directive[18, Black, Bold, FontFamily -> "Times"]
				],
				{Right, Center}
			],
			FillingStyle -> Directive[Gray, Dashed],
			ImageSize -> {700, 500},
			If[UnsameQ[image, False],
				Epilog -> {
					(
						Text[
							Framed[
								Style[#[[1]], 18, Black, FontFamily -> "Times"],
								RoundingRadius -> 5,
								FrameMargins -> 2,
								Background -> Lighter[Gray, 0.97]
							],
							Scaled[{0.5, 0.5}],
							{#[[2]], #[[3]]}
						] & /@ describers
					),
					Inset[image, imagePosition]
				},
				Epilog -> (
					Text[
						Framed[
							Style[#[[1]], 18, Black, FontFamily -> "Times"],
							RoundingRadius -> 5,
							FrameMargins -> 2,
							Background -> Lighter[Gray, 0.97]
						],
						Scaled[{0.5, 0.5}],
						{#[[2]], #[[3]]}
					] & /@ describers
				)
			]
		];
		
		If[UnsameQ[export, False],
			Export[FileNameJoin[{NotebookDirectory[], export <> ".jpeg"}], plot, "JPEG", ImageResolution -> 1000]
		];

		plot
	];
$PlotResults2D[___] := $FailureFunctionSignature["Tool`Helpers`Private`$PlotResults2D"];

(* Palette *)
ClearAll[CreateIPPalette];
CreateIPPalette[] :=
	CreatePalette[
		DynamicModule[
			{
				functionList, directory = $HomeDirectory, tempDir
			}
			,
			Column[{
				Pane[
					Row[{
						Pane @ Dynamic[
							Button[
								Style["Go", 10, Bold],
								(
									updateIPBenchmarks[] :=
										Block[
											{
												progressWidget, repo, commit, panel = "", nb, 
												repoUri = "ssh://git@stash.wolfram.com:7999/~mikayele/ipbenchmarks.git",
												clonePath = FileNameJoin[{$HomeDirectory, "IPBenchmarks"}]
											},

											nb = CreatePalette[Dynamic[panel, SynchronousUpdating -> False], WindowMargins -> Automatic, WindowFrame -> "Frameless"];
											Progress`EvaluateWithProgress[
												Quiet @ Check[
													Needs["GitLink`"];
													Needs["StashLink`"];
													,
													Check[
														PacletUpdate[ToString @ functionList, "Site" -> "http://paclet-int.wolfram.com:8080/PacletServerInternal"] & /@ 
															{"StashLink", "GitLink"};
														Needs["GitLink`"];
														Needs["StashLink`"];
														,
														Message[ImportExport::general, "Could not Get[] or PacletUpdate[] GitLink. Try again on VPN."];
														Throw[$Failed];
													];
												];
												
												If[!DirectoryQ[clonePath],
													CreateDirectory[clonePath];
												];

												If[!DirectoryQ[clonePath],
													Message[ImportExport::general, "Cannot create " <> clonePath <> "."];
													Throw[$Failed];
												];

												(* Open/Clone repo *)
												If[!GitLink`GitRepoQ[clonePath],	
													StashLink`Private`NewProgressWidget[Hold[progressWidget], clonePath];
													StashLink`Private`showProgress["Cloning `1`\n into `2``3`", repoUri, clonePath, 
														progressWidget["ShowWidget"][]];
													
													CheckAbort[
														repo = GitLink`GitClone[repoUri, clonePath, "ProgressMonitor"->progressWidget["SetWidget"]],
														progressWidget["Done"][]
													];
												
													If[GitLink`GitRepoQ[clonePath] && MatchQ[repo, _GitLink`GitRepo],
														progressWidget["Done"][];
														,
														StashLink`Private`showProgress["*** CLONE FAILED: `1`", clonePath];
													];
													,
													repo = GitLink`GitOpen[clonePath];
												];

												If[!GitLink`GitRepoQ[clonePath] || !MatchQ[repo, _GitLink`GitRepo],
													Message[ImportExport::general, "Unable to clone or find git repository. Delete "
														<> clonePath <> " and try again."];
													Throw[$Failed];	
												];

												(* Fetch/Pull repo *)
												If[FailureQ@GitLink`GitFetch[repo, "origin"],
													Message[ImportExport::general, "Unable to fetch git repository."];
													Throw[$Failed];	
												];

												If[!GitLink`GitCommitQ[commit = GitLink`GitPull[repo]],
													Message[ImportExport::general, "Unable to pull git repository."];
													Throw[$Failed];	
												];

												Quiet @ Check[
													PacletDirectoryLoad[clonePath],
													PacletDirectoryLoad[clonePath]
												];
												Needs["IPBenchmarks`"];
												,
												<|
													"Text" -> "Cloning " <> repoUri,
													"Detail" -> clonePath,
													"ElapsedTime" -> Automatic
												|>
												,
												"Delay" -> 0,
												"Buttons" -> <|"Stop" :> (FrontEndTokenExecute["EvaluatorAbort"] && NotebookClose[nb])|>,
												"Container" :> panel
											];

											NotebookClose[nb];
										];

									Quiet @ PacletDirectoryLoad[FileNameJoin[{$HomeDirectory, "IPBenchmarks"}]];
									If[FailureQ[Quiet[Needs["IPBenchmarks`"]]],
										Check[updateIPBenchmarks[], Return[$Failed]];
									];

									NotebookSave[
										Notebook[{
											Cell[ToString @ functionList <> " benchmarks", "Chapter"]
											,
											Cell[BoxData[ButtonBox["Update IPBenchmarks", 
												ButtonFunction :> (
													updateIPBenchmarks[] :=
														Block[
															{
																progressWidget, repo, commit, 
																repoUri = "ssh://git@stash.wolfram.com:7999/~mikayele/ipbenchmarks.git",
																clonePath = FileNameJoin[{$HomeDirectory, "IPBenchmarks"}]
															},

															Quiet@Check[
																Needs["GitLink`"];
																Needs["StashLink`"];
																,
																Check[
																	PacletUpdate[ToString @ functionList, "Site" -> "http://paclet-int.wolfram.com:8080/PacletServerInternal"] & /@ 
																		{"StashLink", "GitLink"};
																	Needs["GitLink`"];
																	Needs["StashLink`"];
																	,
																	Message[ImportExport::general, "Could not Get[] or PacletUpdate[] GitLink. Try again on VPN."];
																	Throw[$Failed];
																];
															];
															
															If[!DirectoryQ[clonePath],
																CreateDirectory[clonePath];
															];

															If[!DirectoryQ[clonePath],
																Message[ImportExport::general, "Cannot create " <> clonePath <> "."];
																Throw[$Failed];	
															];

															(* Open/Clone repo *)
															If[!GitLink`GitRepoQ[clonePath],	
																StashLink`Private`NewProgressWidget[Hold[progressWidget], clonePath];
																StashLink`Private`showProgress["Cloning `1`\n into `2``3`", repoUri, clonePath, 
																	progressWidget["ShowWidget"][]];
																
																CheckAbort[
																	repo = GitLink`GitClone[repoUri, clonePath, "ProgressMonitor"->progressWidget["SetWidget"]],
																	progressWidget["Done"][]
																];
															
																If[GitLink`GitRepoQ[clonePath] && MatchQ[repo, _GitLink`GitRepo],
																	progressWidget["Done"][];
																	,
																	StashLink`Private`showProgress["*** CLONE FAILED: `1`", clonePath];
																];
																,
																repo = GitLink`GitOpen[clonePath];
															];

															If[!GitLink`GitRepoQ[clonePath] || !MatchQ[repo, _GitLink`GitRepo],
																Message[ImportExport::general, "Unable to clone or find git repository. Delete "
																	<> clonePath <> " and try again."];
																Throw[$Failed];	
															];

															(* Fetch/Pull repo *)
															If[FailureQ@GitLink`GitFetch[repo, "origin"],
																Message[ImportExport::general, "Unable to fetch git repository."];
																Throw[$Failed];	
															];

															If[!GitLink`GitCommitQ[commit = GitLink`GitPull[repo]],
																Message[ImportExport::general, "Unable to pull git repository."];
																Throw[$Failed];	
															];

															Quiet @ Check[
																PacletDirectoryLoad[clonePath],
																PacletDirectoryLoad[clonePath]
															];
															Needs["IPBenchmarks`"];

															CellPrint[
																Cell[
																	TextData[
																		"Commit:" <> commit[[1]] <> "\n" <>
																		"Pulled at: " <> DateString[]
																	]
																	,
																	"Text"
																]
															];
														];
													updateIPBenchmarks[]
												),
												Evaluator -> Automatic]
											]]
											,
											Cell["Get results", "Subtitle"]
											,
											Cell[BoxData[RowBox[{"FunctionSummary" <> "[" <> ToString[functionList] <> "]"}]], "Input"]
											,
											Sequence @@ DeleteCases[Flatten @ Map[
												Function[x,
													{
														Cell[x, "Subsection"],
														Map[
															Function[d,
																If[TrueQ[$$listOfAllFunctions[Symbol @ functionList, ToString[d] <> "DQ"]],
																	{
																		Cell[ToString[d] <> "D", "Subsubsection"],
																		{
																			Cell["Master", "Subsubsubsection"],
																			Cell[
																				BoxData[
																					RowBox[{ToString[functionList] <> x <> ToString[d] <> "DMaster = <||>;"}]
																				], 
																				"Input"
																			],
																			Cell["Branch", "Subsubsubsection"],
																			Cell[
																				BoxData[
																					RowBox[{ToString[functionList] <> x <> ToString[d] <> "DBranch = <||>;"}]
																				], 
																				"Input"
																			],												
																			Cell["Comparison", "Subsubsubsection"],
																			Cell[
																				BoxData[
																					RowBox[{
																						StringJoin[
																							x <> "Comparison",
																							"[",
																								ToString[functionList] <> x <> ToString[d] <> "DMaster",
																								", ",
																								ToString[functionList] <> x <> ToString[d] <> "DBranch",
																							"]"
																						]
																					}]
																				], 
																				"Input"
																			]
																		}
																	}
																]
															],
															{2, 3}
														]
													}
												]
												,
												{"Overview", "Timings", "Issues"}
											], Null]
										}],
										FileNameJoin[{directory, ToString[functionList] <> ".nb"}]
									]
								)
								,
								Background -> Darker@Green,
								Appearance -> "AbuttingLeft",
								Method -> "Queued"
							]
							,
							SynchronousUpdating -> False
						]
					}]
					,
					ImageSize -> {270, 50},
					Alignment -> {Left, Center}
				]
				,
				Pane[
					OpenerView[{
						"Benchmark notebook directory"
						,
						Dynamic @ Grid[
							{{
								If[FileNameDepth[directory] > 3,
									FileNameTake[directory, 1] <> "..." <> FileNameTake[directory, -2],
									directory
								]
								,
								Button[
									"...",
									(
										directory = SystemDialogInput[
											"Directory",
											WindowTitle -> "Select the desired directory ..."
										];
										If[SameQ[directory, $Canceled],
											directory = $HomeDirectory
										];
									)
									,
									Appearance -> "Frameless",
									Method -> "Queued"
								]
							}},
							ItemSize -> {{20, Automatic}},
							Alignment -> Left
						]
					}]
				]
			}]
		]
		,
		WindowTitle -> "IPBenchmarks",
		Magnification -> 1.0,
		DynamicUpdating -> True,
		Saveable -> False,
		WindowFloating -> False,
		WindowClickSelect -> True,
		WindowMargins -> Automatic
	];

InstallIPPalette[] :=
	Block[
		{
			result = CreateIPPalette[], path
		},

		NotebookSave[result, path = FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes", "ipbenchmarks.nb"}]];
		NotebookClose[result];
		
		path

	];
InstallIPPalette[___] := $FailureFunctionSignature["IPBenchmarks`InstallIPPalette"];

UnInstallIPPalette[] :=	Quiet @ DeleteFile[FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes", "ipbenchmarks.nb"}]];
UnInstallIPPalette[___] := $FailureFunctionSignature["IPBenchmarks`UnInstallIPPalette"];

End[];
EndPackage[];