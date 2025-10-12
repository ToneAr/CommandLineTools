(* ::Section:: *)(* Patterns *)
stringFormP = (_String|_StringExpression|_RegularExpression|_Alternatives|_Repeated);

argvP = HoldPattern[<|(_String -> _)... |>];

cliInterfaceCommandsP = KeyValuePattern[{(stringFormP :> expr_)...}] | <||>;

inputCommandP = { __String } | _String;

logTypeP = "INFO"|"ERROR"|"SUCCESS"|"WARN";

optionLongP = _String?(StringStartsQ["--"]);
optionShortP = _String?(StringStartsQ["-"]);
optionsP = optionLongP | optionShortP;

argumentSpecP = HoldPattern[{
	Alternatives[
		(name_String | {longName_String, shortName_String}),
		(name_String | {longName_String, shortName_String}) -> Except[_Rule],
		(name_String | {longName_String, shortName_String}) -> pattern_ -> default_
	]...
}];


(* ::Section:: *)(* Utilities *)
(* shouldExit = False; *)

validateOptions[o: OptionsPattern[CLITools]] := Module[{
		opts = <|o|>,
		pairs = {
			{"CommandLine",            inputCommandP},
			{"InterfacePrompt",        _String},
			{"OptionDelimiter",        _String},
			{"HelpMessage",            _String},
			{"LogToFile",              _String},
			{"LogToStdOut",            _String},
			{"LogDirectory",           _String},
			{"ArgumentDelimiter",      _String},
			{"ArgumentSpecification",  None | argumentSpecP},
			{"InterfaceCommands",      <|(_ :> _ )...|> }
		}
	},
	Enclose[
		Function[{name, pattern},
			If[KeyExistsQ[opts, name],
				ConfirmMatch[
					opts[name],
					pattern,
					"Invalid \"" <> name <> "\" option"
				]
			]
		] @@@ pairs;
		Success["options-validate-success", <||>]
	]
];

(* ::Section:: *)(* Options *)
CLITools // Options = {
	"CommandLine" -> Automatic,
	"InterfacePrompt" -> "wls-cli>",
	"InterfaceCommands" -> <|
		{("help" | "h" | "?"), "Show this help message"} :> Print[helpMessage],
		{("exit" | "quit" | "q"), "Exit the interface"} :> (
			shouldExit = True
		),
		{("eval" | "e"), "Evaluate Wolfram Language expression"} :> (
			With[{
					inLbl = StringTemplate["In[``]:= "][$Line],
					outLbl = StringTemplate["Out[``]:= "][$Line]
				},
				Print[outLbl, Input[inLbl]];
				$Line += 1;
			]
		)
	|>,
	"POSIX" -> False,
	"OptionDelimiter" -> (" " | "="),
	"ArgumentDelimiter" -> ",",
	"HelpMessage" -> "CLITools example interface",
	"LogToFile" -> True,
	"LogToStdOut" -> True,
	"LogDirectory" -> "./logs/wls-cli",
	"ArgumentSpecification" -> None,
	"StrictParse" -> False
};

(* ::Section:: *)(* Methods *)
(* ::Subsection:: *)(* Parse *)
(* ::Subsubsection:: *)(* Utilities *)
selectArgDefault[spec_, flag_String] := With[{res = selectArg[spec, flag]},
	Switch[res,
		(_String | {_String, _String}),
			False,
		Rule[_String | {_String, _String}, Except[_Rule]],
			Missing["NoDefault"],
		Rule[_String | {_String, _String}, _Rule],
			res[[2, 2]],
		_Missing,
			False,
		_,
			$Failed
	]
];

selectArgPattern[spec_, flag_String] := With[{
		res = selectArg[spec, flag]
	},
	Switch[res,
		(_String | {_String, _String}),
			_?BooleanQ,
		Rule[_String, Except[_Rule]],
			res[[2]],
		Rule[_String, _Rule],
			res[[2, 1]],
		Rule[{_String, _String}, Except[_Rule]],
			res[[2]],
		Rule[{_String, _String}, _Rule],
			res[[2, 1]],
		_,
			$Failed
	]
];

shortFlagMemberQ[spec_, flag_String] :=
	MemberQ[ getShortFlags @ spec, flag];

longFlagMemberQ[spec_, flag_String] :=
	MemberQ[ getLongFlags @ spec, flag];

flagMemberQ[spec_, flag_String] :=
	MemberQ[ getAllFlags @ spec, flag];

getLongFlags[ spec_ ] :=
	Flatten @ Map[
		Function @ Switch[#,
			_String,
				#,
			{_String, _String},
				#[[1]],
			Rule[_String, _],
				#[[1]],
			Rule[{_String, _String}, _],
				#[[1, 1]],
			_,
				Nothing
		],
		spec
	];

getShortFlags[ spec_ ] :=
	Map[
		Function @ Switch[#,
			{_String, _String},
				#[[2]],
			Rule[{_String, _String},_],
				#[[1,2]],
			_,
				Nothing
		],
		spec
	];

getAllFlags[ spec_ ] :=
	Flatten @ Map[
		Function @ Switch[#,
			_String | {_String, _String},
				{#},
			Rule[_String | {_String, _String}, _],
				{#[[1]]},
			_,
				Nothing
		],
		spec
	];

selectArg[ spec_, name_String ] := With[{
		form = StringDelete[name, StartOfString ~~ Repeated["-", {1, 2}]]
	},
	FirstCase[
		spec,
		Alternatives[
			_String?(StringMatchQ[form]),
			{_String, _String}?(Function[
				Or[
					StringMatchQ[#[[1]], form],
					StringMatchQ[#[[2]], form]
				]
			]),
			Rule[_String, _]?(Function[
				StringMatchQ[#[[1]], form]
			]),
			Rule[{_String, _String}, _]?(Function[
				Or[
					StringMatchQ[#[[1, 1]], form],
					StringMatchQ[#[[1, 2]], form]
				]
			])
		]
	]
];

parseUndefinedArgAndFlag // Options = {
	"POSIX" -> False,
	"ArgumentDelimiter" -> ","
};
parseUndefinedArgAndFlag[ optionList_] :=
	Switch[First @ optionList,
		optionLongP /; Length[optionList] == 1,
			StringDelete[StartOfString ~~ "--"][
					First[optionList]
				] -> True,
		optionLongP,
			StringDelete[StartOfString ~~ "--"][
					First[optionList]
				] -> Replace[StringSplit[Last[optionList], delim],
					{ a_ } :> a
				],
		optionShortP /; Length[optionList] == 1,
			If[ OptionValue["POSIX"],
				With[{
						opts = StringPartition[
							StringDelete[StartOfString ~~ "-"][First[optionList]],
							1
						]
					},
					Map[(# -> True)&] @ opts
				]
				,
				StringDelete[StartOfString ~~ "-"][
					First[optionList]
				] -> True
			],
		optionShortP,
			If[ OptionValue["POSIX"],
				With[{
						opts = StringPartition[
							StringDelete[StartOfString ~~ "-"][First[optionList]],
							1
						]
					},
					Flatten[{
						Map[(# -> True)&] @ Most[opts],
						Last[opts] -> Last[optionList]
					}]
				]
				,
				StringDelete[StartOfString ~~ "-"][
					First[optionList]
				] -> Replace[StringSplit[Last[optionList], OptionValue["ArgumentDelimiter"]],
					{ a_ } :> a
				]
			]
		,
		_,
			$Failed
	];

parseForDefinedArgs // Options = {
	"StrictParse" -> False,
	"POSIX"       -> False,
	"ArgumentDelimiter" -> ","
};
parseForDefinedArgs[flagList: {__String}, spec_, OptionsPattern[]] :=
	Block[{
			shortOption = First[flagList] // StringDelete[StartOfString ~~ "-"],
			argument = Last[flagList, Missing[]],
			opts, most, last
		},
		opts = If[OptionValue["POSIX"],
			StringPartition[shortOption, 1],
			{shortOption}
		];
		most = If[
			If[ OptionValue["StrictParse"],
				And[
					shortFlagMemberQ[spec, #],
					MatchQ[selectArgPattern[spec, #],
						_PatternTest?(Last[#] === BooleanQ &) |
						_Alternatives?(MatchQ[List@@#, {OrderlessPatternSequence[True, False]}] &) |
						_Pattern?(Last[#] === (True|False))
					]
				],
				True
			],
			# -> True,
			Failure["arg-match-fail", <|
				"Argument" -> #
			|>]
		]& /@ Most[opts];
		last = If[
			If[ OptionValue["StrictParse"],
				shortFlagMemberQ[spec, Last[opts]],
				True
			],
			Last[opts] -> Replace[
				If[Length[flagList] > 1,
					Last[
						(*StringSplit[*)
						flagList,
						(*, OptionValue["ArgumentDelimiter"]],*)
						selectArgDefault[spec, Last[opts]]
					],
					selectArgDefault[spec, Last[opts]]
				],
				b_?BooleanQ :> !b
			],
			Failure["unknown-arg", <|
				"Argument" -> Last[opts]
			|>]
		];
		Join[most, {last}]
	];

parseDefinedArgAndFlag // Options = {
	"StrictParse" -> False,
	"POSIX"       -> False,
	"ArgumentDelimiter" -> ","
};
parseDefinedArgAndFlag[optionList_, spec_, OptionsPattern[]] := With[{
		name = StringDelete[StartOfString ~~ "--"][First[optionList]]
	},
	Enclose[
		Switch[First @ optionList,
			optionLongP /; Length[optionList] == 1,
				If[ OptionValue["StrictParse"],
					ConfirmAssert[
						longFlagMemberQ[spec, name],
						"Unknown option \"" <> name <> "\""
					]
				];
				name -> Replace[
					selectArgDefault[spec, name],
					b_?BooleanQ :> !b
				],
			optionLongP,
				With[{ pat = selectArgPattern[spec, name] },
					If[ OptionValue["StrictParse"],
						ConfirmAssert[
							longFlagMemberQ[spec, name],
							"Unknown option \"" <> name <> "\""
						]
					];
					name -> Replace[
						Map[
							ConfirmMatch[
								ResourceFunction["ToExpressionMatched"][#, pat],
								pat,
								"Failed to match value '"<> ToString[#] <>
								"' passed to option '"<> name <>
								"' with pattern '"<> ToString[pat] <>"'"
							]&,
							StringSplit[Last[optionList], delim]
						],
						{ a_ } :> a
					]
				],
			optionShortP /; Length[optionList] == 1,
				Confirm /@ parseForDefinedArgs[
					optionList, spec,
					"ArgumentDelimiter" -> OptionValue["ArgumentDelimiter"],
					"StrictParse" -> OptionValue["StrictParse"],
					"POSIX" -> OptionValue["POSIX"]
				],
			optionShortP,
				MapApply[
					Function[{n, v},
						With[{pat = selectArgPattern[spec, n]},
							n -> If[pat === _String,
								v,
								Map[
									ConfirmMatch[
										ResourceFunction["ToExpressionMatched"][#, pat],
										pat,
										"Failed to match value '"<> ToString[#] <>
										"' passed to option '"<> n <>
										"' with pattern '"<> ToString[pat] <>"'"
									]&,
									Flatten[{ v }]
								] // Replace[ { a_ } :> a ]
							]
						]
					],
					List @@@
					(Confirm /@ parseForDefinedArgs[
						optionList, spec,
						"ArgumentDelimiter" -> OptionValue["ArgumentDelimiter"],
						"StrictParse" -> OptionValue["StrictParse"],
						"POSIX" -> OptionValue["POSIX"]
					])
				],
			_,
				$Failed
		]
	]
];

parseFlagsAndArgs // Options = {
	"StrictParse" -> False,
	"POSIX" -> False,
	"ArgumentDelimiter" -> ","
};
parseFlagsAndArgs[flagsAndArgs_, spec_, OptionsPattern[]] :=
	Enclose[
		If[ spec === None,
			Association @ Map[
				Confirm[
					parseUndefinedArgAndFlag[
						#,
						OptionValue["ArgumentDelimiter"],
						OptionValue["POSIX"]
					]
				]&,
				flagsAndArgs
			],
			addDefaults[
				spec,
				Flatten @ Map[
					Confirm[
						parseDefinedArgAndFlag[
							#, spec,
							"StrictParse" -> OptionValue["StrictParse"],
							"POSIX" -> OptionValue["POSIX"],
							"ArgumentDelimiter" -> OptionValue["ArgumentDelimiter"]
						]
					]&,
					flagsAndArgs
				]
			]
		 ]
	];

getAlias[spec_, name_String] :=
	FirstCase[
		spec,
		{OrderlessPatternSequence[name, alias_String]} :> alias,
		Missing[],
		{1, 2}
	];

addDefaults[spec_, parsed_]:=
	Association[
		Flatten @ Join[
			Cases[
				spec,
				Rule[n_String, Rule[_, def_]] :> (n -> def)
			],
			Cases[
				spec,
				Rule[{ln_String, sn_String}, Rule[_, def_]] :> {ln -> def, sn -> def}
			]
		],
		ReplaceAll[
			parsed,
			((r: Rule[n_String, val_]) /; !MissingQ[getAlias[spec, n]]) :> Sequence[
				r,
				getAlias[spec, n] -> val
			]
		]
	];
(* ::Subsubsection:: *)(* Main *)
CLITools["Parse", form: stringFormP, opts:OptionsPattern[]] := Module[{
		args = CLITools["Parse", opts]
	},
	If[FailureQ[args],
		Return[args]
	];
	Replace[
		KeySelect[args, StringMatchQ[form]],
		{
			<| r_Rule |> :> Last[r],
			<||> -> Failure["flag-not-found", <|
				"MessageTemplate" -> "Flag `` not found in the command line",
				"MessageParameters" -> ToString[form]
			|>],
			(* Merge duplicate values *)
			a: <| __Rule |> /; Length[DeleteDuplicates[Values[a]]] === 1 :>
				First[a]
		}
	]
];
CLITools["Parse", opts: OptionsPattern[]] := Block[{
		from, flagPositions, flagsAndArgs, commands
	},
	(* Cache args in kernel session as args will never change during a session *)
	(*--Once[--*)
		Enclose[
			Confirm @ validateOptions[opts];
			(* Parse command line *)
			commands = Replace[
				OptionValue["CommandLine"] // Replace[{
					(Automatic /; $EvaluationEnvironment === "Script") :> $ScriptCommandLine,
					Automatic :> $CommandLine
				}],
				s_String :> StringSplit[s, " "]
			];
			(* Find position of 1st option *)
			from = First[
				FirstPosition[commands, optionsP, {1}],
				$Failed
			];
			(* Split command list to include only options *)
			commands = commands[[from;;All]];
			(* Split options delimited by non-space characters *)
			commands = Flatten @ ReplaceAt[ commands,
				s_String :> StringSplit[s, OptionValue["OptionDelimiter"]],
				 {All}
			];
			(* Get all option options *)
			flagPositions = Position[commands, optionsP, {1}];
			(* Partition a list with all options and any proceeding arguments *)
			flagsAndArgs = Table[
				With[{
						start = Confirm[
							First[flagPositions[[i]], $Failed],
							"Failed to select starting position"
						],
						end = Confirm[
							If[i === Length[flagPositions],
								Length[commands],
								First[flagPositions[[i + 1]] - 1, $Failed]
							],
							"Failed to select ending position"
						]
					},
					commands[[start;;end]]
				]
				,
				{i, Length[flagPositions]}
			];
			ConfirmMatch[
				parseFlagsAndArgs[
					flagsAndArgs,
					OptionValue["ArgumentSpecification"],
					"StrictParse" -> OptionValue["StrictParse"],
					"POSIX" -> OptionValue["POSIX"],
					"ArgumentDelimiter" -> OptionValue["ArgumentDelimiter"]
				],
				argvP
			]
		]
		(*--,
		PersistenceLocation["KernelSession"]
	]--*)
];
(* ::Subsection:: *)(* FlagQ *)
CLITools["FlagQ",
	form:(stringFormP | {stringFormP..}),
	opts: OptionsPattern[]
] := MemberQ[
	Flatten @ StringSplit[
		Replace[OptionValue["CommandLine"], {
			Automatic /; (
				$EvaluationEnvironment === "Script"
			) :> $ScriptCommandLine,
			Automatic :> $CommandLine
		}],
		OptionValue["OptionDelimiter"]
	],
	_String?(StringMatchQ[Repeated["-", {0, 2}] ~~ form])
];
(* ::Subsection:: *)(* Interface *)
CLITools["Interface", opts:OptionsPattern[]] :=
	CLITools["Interface", <||>, opts];
CLITools["Interface", commands:(<|___RuleDelayed|> | {___RuleDelayed}), opts:OptionsPattern[]] :=
	Block[{
			in, helpMessage,
			shouldExit = False,
			allCommands
		},
		Enclose[
			Confirm @ validateOptions[opts];
			allCommands = Join[OptionValue["InterfaceCommands"], commands];
			helpMessage = CLITools["Help",
				"InterfaceCommands" -> allCommands,
				opts
			];
			allCommands = ReplaceAll[
				Normal @ allCommands, {
				{form_, _String} :> form
			}];
			While[!shouldExit,
				Pause[.01];
				in = InputString[OptionValue["InterfacePrompt"] <> " "];
				Replace[in, allCommands]
			]
		]
	];
(* ::Subsection:: *)(* Log *)
CLITools["Log", type: logTypeP, opts: OptionsPattern[]][ msg_String ] :=
	CLITools["Log", type, msg, opts];
CLITools["Log", type: logTypeP, msg_String, OptionsPattern[]] := Module[{
		logStream, logName, logPath, message, logLevel
	},
	logName = DateString[{"Year", "Month", "Day", ".log"}];
	logPath = FileNameJoin[{OptionValue["LogDirectory"], logName}];
	Enclose[
		If[OptionValue["LogToFile"],
			message = StringTemplate["`timestamp` - [`type`]: `body`"][<|
				"timestamp" -> DateString[{"Hour",":","Minute",":","Second"}],
				"type" -> type,
				"body" -> msg
			|>];
			WithCleanup[
				If[ !DirectoryQ[OptionValue["LogDirectory"]],
					CreateDirectory[OptionValue["LogDirectory"], CreateIntermediateDirectories -> True]
				];
				logStream = Confirm @ OpenAppend[logPath],
				WriteLine[logStream, message],
				Close[logStream]
			]
		];
		If[OptionValue["LogToStdOut"],
			logLevel = ResourceFunction["ANSITools"]["Style",
				"["<>type<>"]: ",
				Switch[type,
					"INFO",    Gray,
					"WARN",    Yellow,
					"SUCCESS", Green,
					"ERROR",   Red,
					_,         Return@$Failed
				]
			];
			Print[logLevel<>msg]
		];
		Success["messaged-logged", <|
			"Location" ->  logPath,
			"LogLevel" -> type,
			"Message" -> msg
		|>]
	]
];
(* ::Subsection:: *)(* Help *)
(* ::Subsubsection:: *)(* Utilities *)
generateHelpFromSpec[spec_List] := Module[{info, patternLabel, fmtLine, longest},
	patternLabel[pat_] := Which[
		MatchQ[pat, _Integer], "<Integer>",
		MatchQ[pat, _Real], "<Real>",
		MatchQ[pat, _String], "<String>",
		MatchQ[pat, _Symbol], "<Symbol>",
		MatchQ[pat, _?BooleanQ | _?(# === True || # === False &)], "<Boolean>",
		True, "<" <> StringReplace[ToString[pat, InputForm], {"PatternTest" -> "PatTest"}] <> ">"
	];
	info = Map[
		Function[e,
			Which[
				MatchQ[e, _String], <|"Long" -> e, "Short" -> Missing[], "Pattern" -> _?BooleanQ, "Default" -> False|>,
				MatchQ[e, {_String, _String}], <|"Long" -> e[[1]], "Short" -> e[[2]], "Pattern" -> _?BooleanQ, "Default" -> False|>,
				MatchQ[e, Rule[_String | {_String, _String}, Except[_Rule]]], <|"Long" -> If[ListQ[e[[1]]], e[[1,1]], e[[1]]], "Short" -> If[ListQ[e[[1]]], e[[1,2]], Missing[]], "Pattern" -> e[[2]], "Default" -> Missing[]|>,
				MatchQ[e, Rule[_String | {_String, _String}, Rule[_, _]]], <|"Long" -> If[ListQ[e[[1]]], e[[1,1]], e[[1]]], "Short" -> If[ListQ[e[[1]]], e[[1,2]], Missing[]], "Pattern" -> e[[2,1]], "Default" -> e[[2,2]]|>,
				True, <|"Long" -> "UNKNOWN", "Short" -> Missing[], "Pattern" -> _String, "Default" -> Missing[]|>
			]
		],
		spec
	];
	longest = Max[ StringLength /@ (info[[All, "Long"]]) ];
	fmtLine[assoc_] := Module[{flagPart, pat = assoc["Pattern"], def = assoc["Default"], patTxt, defTxt},
		flagPart = "\t--" <> assoc["Long"] <> If[!MissingQ[assoc["Short"]], ", -" <> assoc["Short"], ""];
		flagPart = flagPart <> StringRepeat[" ", Max[0, longest + 8 - StringLength[flagPart]]];
		patTxt = If[MatchQ[pat, _?BooleanQ], "", patternLabel[pat]];
		defTxt = Which[
			MissingQ[def], "",
			def === False && MatchQ[pat, _?BooleanQ], "",
			True, " (default: " <> ToString[def, InputForm] <> ")"
		];
		flagPart <> patTxt <> defTxt
	];
	StringRiffle[fmtLine /@ info, "\n"]
];

generateInterfaceCommandsHelp[commands_Association] := Module[{cmdList, longest, fmtLine},
	cmdList = Keys[commands];
	longest = Max[StringLength /@ Map[
		Function[cmd,
			Which[
				MatchQ[cmd, {_, _String}], ToString[cmd[[1]]],
				True, ToString[cmd]
			]
		],
		cmdList
	]];
	fmtLine[cmd_] := Module[{cmdStr, desc},
		{cmdStr, desc} = Which[
			MatchQ[cmd, {_, _String}], {ToString[cmd[[1]]], cmd[[2]]},
			True, {ToString[cmd], "Custom command"}
		];
		"\t" <> cmdStr <> StringRepeat[" ", Max[0, longest + 4 - StringLength[cmdStr]]] <> desc
	];
	StringRiffle[fmtLine /@ cmdList, "\n"]
];
(* ::Subsubsection:: *)(* Main *)
CLITools["Help", opts: OptionsPattern[]] := Module[{
		spec = OptionValue["ArgumentSpecification"],
		base = OptionValue["HelpMessage"],
		commands = OptionValue["InterfaceCommands"],
		result
	},
	result = base;
	(* Add interface commands section *)
	If[Length[commands] > 0,
		result = result <> "\n\nCommands:\n\n" <> generateInterfaceCommandsHelp[commands]
	];
	(* Add options section if specification exists *)
	If[spec =!= None,
		result = result <> "\n\nOptions:\n\n" <> generateHelpFromSpec[spec]
	];
	result
];
