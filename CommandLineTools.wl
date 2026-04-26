(* ::Section:: *)
(* Patterns *)
stringFormP =
	(
		_String |
		_StringExpression |
		_RegularExpression |
		_Alternatives |
		_Repeated
	)

argvP = HoldPattern[<|(_String -> _)...|>]

cliInterfaceCommandsP = KeyValuePattern[{(stringFormP :> expr_)...}] | <||>

inputCommandP = {__String} | _String

optionLongP = _String?(StringStartsQ["--" ~~ WordCharacter])

optionShortP = _String?(StringStartsQ["-" ~~ WordCharacter])

optionsP = optionLongP | optionShortP

positionalSpecKeyP = _String?(StringMatchQ["$" ~~ DigitCharacter..])

argumentSpecP =
	HoldPattern[
		{
			Alternatives[
				(_String | {_String, _String}),
				(_String | {_String, _String}) -> Except[_Rule],
				(_String | {_String, _String}) -> _ -> _,
				positionalSpecKeyP -> Except[_Rule],
				positionalSpecKeyP -> _ -> _
			]...
		}
	]

(* ::Section:: *)
(* Utilities *)
validateOptions[o : OptionsPattern[CommandLineTools]] :=
	Module[{
		opts = <|o|>,
		pairs =
			{
				{"CommandLine", inputCommandP},
				{"InterfacePrompt", _String},
				{"OptionDelimiter", stringFormP},
				{"HelpMessage", _String},
				{"LogToFile", _String},
				{"LogToStdOut", _String},
				{"LogDirectory", _String},
				{"ArgumentDelimiter", _String},
				{"ArgumentSpecification", None | argumentSpecP},
				{"InterfaceCommands", <|(_ :> _)...|>}
			}
	},
		Enclose[
			Function[
				{name, pattern},
				If[
					KeyExistsQ[opts, name],
					ConfirmMatch[
						opts[name],
						pattern,
						"Invalid \"" <> name <> "\" option"
					]
				]
			] @@@ pairs;
			Success["options-validate-success", <||>]
		]
	]

defaultCommands =
	<|
		{("help" | "h" | "?"), "Show this help message"} :> Print[helpMessage],
		{("exit" | "quit" | "q"), "Exit the interface"} :> (
			(* wl-disable-next-line UnloadedContext *)
			CommandLineTools`ShouldExit = True
		),
		{("eval" | "e"), "Evaluate Wolfram Language expression"} :> (
			With[{
				inLbl = StringTemplate["In[``]:= "][$Line],
				outLbl = StringTemplate["Out[``]:= "][$Line]
			},
				Print[outLbl, Input[inLbl]]; $Line += 1
			]
		)
	|>

(* ::Section:: *)
(* Options *)
CommandLineTools // Options =
	{
		"CommandLine" -> Automatic,
		"InterfacePrompt" -> "wls-cli>",
		"InterfaceCommands" -> defaultCommands,
		"InterfaceHelp" -> True,
		"POSIX" -> False,
		"OptionDelimiter" -> (" " | "="),
		"OptionHelp" -> True,
		"HelpMessage" -> "CommandLineTools example interface",
		"LogToFile" -> True,
		"LogToStdOut" -> True,
		"LogDirectory" -> "./logs/wls-cli",
		"StrictParse" -> False,
		"ArgumentDelimiter" -> ",",
		"ArgumentSpecification" -> None,
		"LogStyles" -> {
			"INFO" -> Gray,
			"WARN" -> Yellow,
			"SUCC" -> Green,
			"FAILURE" -> Red,
			"SUCCESS" -> Green,
			"FAIL" -> Red,
			"ERROR" -> Red
		}
	}

(* ::Section:: *)
(* Methods *)
(* ::Subsection:: *)
(* Parse *)
(* ::Subsubsection:: *)
(* Utilities *)
selectArgDefault[spec_, flag_String] :=
	With[{res = selectArg[spec, flag]},
		Switch[
			res,
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
	]

selectArgPattern[spec_, flag_String] :=
	With[{res = selectArg[spec, flag]},
		Switch[
			res,
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
	]

shortFlagMemberQ[spec_, flag_String] := MemberQ[getShortFlags @ spec, flag]

longFlagMemberQ[spec_, flag_String] := MemberQ[getLongFlags @ spec, flag]

flagMemberQ[spec_, flag_String] := MemberQ[getAllFlags @ spec, flag]

getLongFlags[spec_] :=
	Flatten @
	Map[
		Function @
		Switch[
			#,
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
	]

getShortFlags[spec_] :=
	Map[
		Function @
		Switch[
			#,
			{_String, _String},
			#[[2]],
			Rule[{_String, _String}, _],
			#[[1, 2]],
			_,
			Nothing
		],
		spec
	]

getAllFlags[spec_] :=
	Flatten[
		Map[
			Function @
			Switch[
				#,
				_String,
				{#},
				{_String, _String},
				{#[[1]], #[[2]], #},
				Rule[_String, _],
				{#[[1]]},
				Rule[{_String, _String}, _],
				{#[[1, 1]], #[[1, 2]], #[[1]]},
				_,
				{}
			],
			spec
		],
		1
	]

selectArg[spec_, name_String] :=
	With[{form = StringDelete[name, StartOfString ~~ Repeated["-", {1, 2}]]},
		FirstCase[
			spec,
			Alternatives[
				_String?(StringMatchQ[form]),
				{_String, _String}?(
					Function[
						Or[
							StringMatchQ[#[[1]], form],
							StringMatchQ[#[[2]], form]
						]
					]
				),
				Rule[_String, _]?(Function[StringMatchQ[#[[1]], form]]),
				Rule[{_String, _String}, _]?(
					Function[
						Or[
							StringMatchQ[#[[1, 1]], form],
							StringMatchQ[#[[1, 2]], form]
						]
					]
				)
			]
		]
	]

parseUndefinedArgAndFlag // Options =
	{"POSIX" -> False, "ArgumentDelimiter" -> ","}

parseUndefinedArgAndFlag[optionList_, OptionsPattern[]] :=
	Switch[
		First @ optionList,
		optionLongP /; Length[optionList] == 1,
		StringDelete[StartOfString ~~ "--"][First[optionList]] -> True,
		optionLongP,
		StringDelete[StartOfString ~~ "--"][First[optionList]] -> Replace[
			StringSplit[Last[optionList], OptionValue["ArgumentDelimiter"]],
			{a_} :> a
		],
		optionShortP /; Length[optionList] == 1,
		If[
			OptionValue["POSIX"],
			With[{
				opts =
					StringPartition[
						StringDelete[StartOfString ~~ "-"][First[optionList]],
						1
					]
			},
				Map[(# -> True)&] @ opts
			],
			StringDelete[StartOfString ~~ "-"][First[optionList]] -> True
		],
		optionShortP,
		If[
			OptionValue["POSIX"],
			With[{
				opts =
					StringPartition[
						StringDelete[StartOfString ~~ "-"][First[optionList]],
						1
					]
			},
				Flatten[
					{
						Map[(# -> True)&] @ Most[opts],
						Last[opts] -> Last[optionList]
					}
				]
			],
			StringDelete[StartOfString ~~ "-"][First[optionList]] -> Replace[
				StringSplit[Last[optionList], OptionValue["ArgumentDelimiter"]],
				{a_} :> a
			]
		],
		_,
		$Failed
	]

parseForDefinedArgs // Options =
	{"StrictParse" -> False, "POSIX" -> False, "ArgumentDelimiter" -> ","}

parseForDefinedArgs[flagList : {__String}, spec_, OptionsPattern[]] :=
	Block[{
		shortOption = First[flagList] // StringDelete[StartOfString ~~ "-"],
		argument = Last[flagList, Missing[]],
		opts,
		most,
		last
	},
		opts =
			If[
				OptionValue["POSIX"],
				StringPartition[shortOption, 1],
				{shortOption}
			];
		most =
			If[
				If[
					OptionValue["StrictParse"],
					And[
						shortFlagMemberQ[spec, #],
						MatchQ[
							selectArgPattern[spec, #],
							_PatternTest?(Last[#] === BooleanQ&) |
							_Alternatives?(
								MatchQ[
									List @@ #,
									{OrderlessPatternSequence[True, False]}
								]&
							) |
							_Pattern?(Last[#] === (True | False))
						]
					],
					True
				],
				# -> True,
				Failure["arg-match-fail", <|"Argument" -> #|>]
			]& /@ Most[opts];
		last =
			If[
				If[
					OptionValue["StrictParse"],
					shortFlagMemberQ[spec, Last[opts]],
					True
				],
				Last[opts] -> Replace[
					If[
						Length[flagList] > 1,
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
				Failure["unknown-arg", <|"Argument" -> Last[opts]|>]
			];
		Join[most, {last}]
	]

parseDefinedArgAndFlag // Options =
	{"StrictParse" -> False, "POSIX" -> False, "ArgumentDelimiter" -> ","}

parseDefinedArgAndFlag[optionList_, spec_, OptionsPattern[]] :=
	With[{
		name = StringDelete[StartOfString ~~ "--"][First[optionList]]
	},
		Enclose[
			Switch[
				First @ optionList,
				optionLongP /; Length[optionList] == 1,
				If[
					OptionValue["StrictParse"],
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
				With[{pat = selectArgPattern[spec, name]},
					If[
						OptionValue["StrictParse"],
						ConfirmAssert[
							longFlagMemberQ[spec, name],
							"Unknown option \"" <> name <> "\""
						]
					];
					name -> Replace[
						If[
							pat === $Failed,
							(* Unknown flag, StrictParse off: pass value through raw *)
							StringSplit[
								Last[optionList],
								OptionValue["ArgumentDelimiter"]
							],
							Map[
								If[
									pat === _String,
									(* String pattern: keep raw token as-is *)
									#,
									ConfirmMatch[
										ResourceFunction["ToExpressionMatched"][
											#,
											pat
										],
										pat,
										"Failed to match value '" <>
										ToString[#] <>
										"' passed to option '" <>
										name <>
										"' with pattern '" <>
										ToString[pat] <>
										"'"
									]
								]&,
								StringSplit[
									Last[optionList],
									OptionValue["ArgumentDelimiter"]
								]
							]
						],
						{a_} :> a
					]
				],
				optionShortP /; Length[optionList] == 1,
				Confirm /@ parseForDefinedArgs[
					optionList,
					spec,
					"ArgumentDelimiter" -> OptionValue["ArgumentDelimiter"],
					"StrictParse" -> OptionValue["StrictParse"],
					"POSIX" -> OptionValue["POSIX"]
				],
				optionShortP,
				MapApply[
					Function[
						{n, v},
						With[{pat = selectArgPattern[spec, n]},
							n -> If[
								pat === $Failed || pat === _String,
								(* Unknown flag or plain string: pass value through raw *)
								v,
								Map[
									ConfirmMatch[
										ResourceFunction["ToExpressionMatched"][
											#,
											pat
										],
										pat,
										"Failed to match value '" <>
										ToString[#] <>
										"' passed to option '" <>
										n <>
										"' with pattern '" <>
										ToString[pat] <>
										"'"
									]&,
									Flatten[{v}]
								] //
								Replace[{a_} :> a]
							]
						]
					],
					List @@@ (
						Confirm /@ parseForDefinedArgs[
							optionList,
							spec,
							"ArgumentDelimiter" -> OptionValue[
								"ArgumentDelimiter"
							],
							"StrictParse" -> OptionValue["StrictParse"],
							"POSIX" -> OptionValue["POSIX"]
						]
					)
				],
				_,
				$Failed
			]
		]
	]

parseFlagsAndArgs // Options =
	{"StrictParse" -> False, "POSIX" -> False, "ArgumentDelimiter" -> ","}

parseFlagsAndArgs[flagsAndArgs_, spec_, OptionsPattern[]] :=
	Enclose[
		If[
			spec === None,
			Association @
			Flatten @
			Map[
				Confirm[
					parseUndefinedArgAndFlag[
						#,
						"ArgumentDelimiter" -> OptionValue["ArgumentDelimiter"],
						"POSIX" -> OptionValue["POSIX"]
					]
				]&,
				flagsAndArgs
			],
			addDefaults[
				spec,
				Flatten @
				Map[
					Confirm[
						parseDefinedArgAndFlag[
							#,
							spec,
							"StrictParse" -> OptionValue["StrictParse"],
							"POSIX" -> OptionValue["POSIX"],
							"ArgumentDelimiter" -> OptionValue[
								"ArgumentDelimiter"
							]
						]
					]&,
					flagsAndArgs
				]
			]
		]
	]

getAlias[spec_, name_String] :=
	FirstCase[
		spec,
		{OrderlessPatternSequence[name, alias_String]} :> alias,
		Missing[],
		{1, 2}
	]

applyPositionalSpec[spec_, positionals_List] :=
	Module[{
		specAssoc =
			Association @
			Cases[spec, Rule[k : positionalSpecKeyP, v_] :> (k -> v)]
	},
		MapIndexed[
			Function[
				{val, idx},
				With[{key = "$" <> ToString[idx[[1]]]},
					key -> With[{
						entry = Lookup[specAssoc, key, Missing[]]
					},
						Which[
							MissingQ[entry],
								val,
							MatchQ[entry, Except[_Rule]] && entry === _String,
								val,
							MatchQ[entry, Except[_Rule]],
								Replace[
									ResourceFunction["ToExpressionMatched"][
										val,
										entry
									],
									Except[entry] :> val
								],
							MatchQ[entry, _Rule] && entry[[1]] === _String,
								val,
							MatchQ[entry, _Rule],
								Replace[
									ResourceFunction["ToExpressionMatched"][
										val,
										entry[[1]]
									],
									Except[entry[[1]]] :> val
								]
						]
					]
				]
			],
			positionals
		]
	]

addDefaults[spec_, parsed_] :=
	Association[
		Flatten @
		Join[
			Cases[spec, Rule[n_String, Rule[_, def_]] :> (n -> def)],
			Cases[
				spec,
				Rule[{ln_String, sn_String}, Rule[_, def_]] :> {
					ln -> def,
					sn -> def
				}
			]
		],
		Flatten @
		ReplaceAll[
			parsed,
			(
				(r : Rule[n_String, val_]) /; !MissingQ[getAlias[spec, n]]
			) :> Sequence[r, getAlias[spec, n] -> val]
		]
	]

(* ::Subsubsection:: *)
(* Main *)
CommandLineTools["Parse", form : stringFormP, opts : OptionsPattern[]] :=
	Module[{args = CommandLineTools["Parse", opts]},
		If[FailureQ[args], Return[args]];
		Replace[
			KeySelect[args, StringMatchQ[form]],
			{
				<|r_Rule|> :> Last[r],
				<||> -> Failure[
					"flag-not-found",
					<|
						"MessageTemplate" -> "Flag `` not found in the command line",
						"MessageParameters" -> ToString[form]
					|>
				],
				(* Merge duplicate values *)
				a : <|__Rule|> /;
					Length[DeleteDuplicates[Values[a]]] === 1 :> First[a]
			}
		]
	]

CommandLineTools["Parse", opts : OptionsPattern[]] :=
	Block[
		{
			from,
			flagPositions,
			flagsAndArgs,
			commands,
			parsed,
			commandName,
			prePositionals,
			groupPositionals,
			allPositionals,
			spec = OptionValue["ArgumentSpecification"]
		},
		(* Cache args in kernel session as args will never change during a session *)
		(*!Once[!*)
		Enclose[
			Confirm @ validateOptions[opts];
			(* Parse command line *)
			commands =
				Replace[
					OptionValue["CommandLine"] //
					Replace[
						{
							(
								Automatic /; $EvaluationEnvironment === "Script"
							) :> $ScriptCommandLine,
							Automatic :> $CommandLine
						}
					],
					s_String :> ResourceFunction["CommandLineSplit"][
						s,
						"TokenDelimiters" -> (
							OptionValue[
								"OptionDelimiter"
							] /. Alternatives -> List
						)
					]
				];
			If[
				OptionValue["StrictParse"],
				ConfirmAssert[
					MemberQ[commands, optionsP],
					"No options found in command line"
				]
			];
			(* Find position of 1st option *)
			from =
				First[
					FirstPosition[commands, optionsP, {Length[commands] + 1}],
					$Failed
				];
			(* Capture command name (first token when it is not itself a flag) *)
			commandName = If[from > 1, commands[[1]], Missing[]];
			(* Capture positional arguments that precede the first flag (after command name) *)
			prePositionals = If[from > 2, commands[[2;;from - 1]], {}];
			(* Split command list to include only options *)
			commands = commands[[from;;All]];
			(*
			 * Split options delimited by non-space characters (e.g. --flag=value).
			 * Only apply to option tokens so that argument values containing spaces
			 * (produced by quoted strings in the command line) are not re-split.
			 *)
			commands =
				Flatten @
				ReplaceAt[
					commands,
					s_String?(StringStartsQ["-"]) :> StringSplit[
						s,
						OptionValue["OptionDelimiter"]
					],
					{All}
				];
			(* Get all option options *)
			flagPositions = Position[commands, optionsP, {1}];
			(* Partition a list with all options and any proceeding arguments *)
			flagsAndArgs =
				Table[
					With[{
						start =
							Confirm[
								First[flagPositions[[i]], $Failed],
								"Failed to select starting position"
							],
						end =
							Confirm[
								If[
									i === Length[flagPositions],
									Length[commands],
									First[flagPositions[[i + 1]] - 1, $Failed]
								],
								"Failed to select ending position"
							]
					},
						commands[[start;;end]]
					],
					{i, Length[flagPositions]}
				];
			(* Extract positional arguments, respecting the argument specification.
				When a spec is defined and a flag is boolean, every following
				non-flag token is a positional rather than the flag's value.
				Without a spec the original behaviour is preserved: the token
				immediately after a flag is treated as its value. *)
			If[
				Length[flagsAndArgs] > 0,
				{flagsAndArgs, groupPositionals} =
					Transpose @
					Map[
						Function[
							g,
							Which[
								Length[g] === 1,
								{g, {}},
								spec =!= None &&
								MatchQ[
									selectArgPattern[spec, g[[1]]],
									_PatternTest?(Last[#] === BooleanQ&) |
									_Alternatives?(
										MatchQ[
											List @@ #,
											{
												OrderlessPatternSequence[
													True,
													False
												]
											}
										]&
									) |
									_Pattern?(Last[#] === (True | False)&)
								],
								(* Boolean flag: all following tokens are positionals *)
								{{g[[1]]}, g[[2;;]]},
								True,
								(* Value-taking flag: keep flag + value, rest are positionals *)
								{
									g[[1;;Min[2, Length[g]]]],
									If[Length[g] > 2, g[[3;;]], {}]
								}
							]
						],
						flagsAndArgs
					];
				groupPositionals = Flatten[groupPositionals],
				groupPositionals = {}
			];
			(* Collect all positional arguments:
				when no flags exist at all, every token is positional *)
			allPositionals =
				If[
					Length[flagPositions] === 0,
					Join[prePositionals, commands],
					Join[prePositionals, groupPositionals]
				];
			parsed =
				Confirm @
				parseFlagsAndArgs[
					flagsAndArgs,
					OptionValue["ArgumentSpecification"],
					"StrictParse" -> OptionValue["StrictParse"],
					"POSIX" -> OptionValue["POSIX"],
					"ArgumentDelimiter" -> OptionValue["ArgumentDelimiter"]
				];
			ConfirmMatch[
				Join[
					If[!MissingQ[commandName], <|"$0" -> commandName|>, <||>],
					parsed,
					Association @
					With[{
						spec = OptionValue["ArgumentSpecification"]
					},
						If[
							spec =!= None,
							applyPositionalSpec[spec, allPositionals],
							MapIndexed[
								("$" <> ToString[#2[[1]]] -> #1)&,
								allPositionals
							]
						]
					]
				],
				argvP
			]
		]
		(*!
		,
		PersistenceLocation["KernelSession"]
		]!*)
	]

(* ::Subsection:: *)
(* FlagQ *)
CommandLineTools[
	"FlagQ",
	form : (stringFormP | {stringFormP..}),
	OptionsPattern[]
] :=
	MemberQ[
		Flatten[
			StringSplit[
				Replace[
					OptionValue["CommandLine"],
					{
						Automatic /;
							(
								$EvaluationEnvironment === "Script"
							) :> $ScriptCommandLine,
						Automatic :> $CommandLine
					}
				],
				OptionValue["OptionDelimiter"]
			]
		] //
		If[
			OptionValue["POSIX"],
			Flatten @*
			ReplaceAll[
				{
					short : optionShortP :> StringInsert[
						Rest @* Characters @ short,
						"-",
						1
					]
				}
			],
			Identity
		],
		_String?(StringMatchQ[Repeated["-", {1, 2}] ~~ form])
	]

(* ::Subsection:: *)
(* Interface *)
CommandLineTools["Interface", opts : OptionsPattern[]] :=
	CommandLineTools["Interface", <||>, opts]

CommandLineTools[
	"Interface",
	commands : (<|___RuleDelayed|> | {___RuleDelayed}),
	opts : OptionsPattern[]
] :=
	Block[{in, helpMessage, allCommands},
		Enclose[
			Confirm @ validateOptions[opts];
			(* wl-disable-next-line UnloadedContext *)
			CommandLineTools`ShouldExit = False;
			allCommands = Join[OptionValue["InterfaceCommands"], commands];
			helpMessage =
				CommandLineTools[
					"Help",
					"InterfaceCommands" -> allCommands,
					opts
				];
			allCommands =
				ReplaceAll[Normal @ allCommands, {{form_, _String} :> form}];
			While[
				(* wl-disable-next-line UnloadedContext *)
				!CommandLineTools`ShouldExit,
				Pause[.01];
				in = InputString[OptionValue["InterfacePrompt"] <> " "];
				Replace[in, allCommands]
			]
		]
	]

(* ::Subsection:: *)
(* Log *)
CommandLineTools["Log", type_String, opts : OptionsPattern[]][msg_String] :=
	CommandLineTools["Log", type, msg, opts]

CommandLineTools["Log", type_String, msg_String, OptionsPattern[]] :=
	Module[{
		logStream,
		logName,
		logPath,
		message,
		logLevel
	},
		logName = DateString[{"Year", "Month", "Day", ".log"}];
		logPath = FileNameJoin[{OptionValue["LogDirectory"], logName}];
		Enclose[
			If[
				OptionValue["LogToFile"],
				message =
					StringTemplate["`timestamp` - [`type`]: `body`"][
						<|
							"timestamp" -> DateString[
								{"Hour", ":", "Minute", ":", "Second"}
							],
							"type" -> type,
							"body" -> msg
						|>
					];
				WithCleanup[
					If[
						!DirectoryQ[OptionValue["LogDirectory"]],
						CreateDirectory[
							OptionValue["LogDirectory"],
							CreateIntermediateDirectories -> True
						]
					];
					logStream = Confirm @ OpenAppend[logPath],
					WriteLine[logStream, message],
					Close[logStream]
				]
			];
			If[
				OptionValue["LogToStdOut"],
				logLevel =
					ResourceFunction["ANSITools"][
						"Style",
						"[" <> type <> "]: ",
						Replace[
							type,
							Append[OptionValue["LogStyles"], _String -> Gray]
						]
					];
				Print[logLevel <> msg]
			];
			Success[
				"messaged-logged",
				<|"Location" -> logPath, "LogLevel" -> type, "Message" -> msg|>
			]
		]
	]

(* ::Subsection:: *)
(* Help *)
(* ::Subsubsection:: *)
(* Utilities *)
generateHelpFromSpec[spec_List] :=
	Module[{info, patternLabel, fmtLine, longest},
		patternLabel[pat_] :=
			Which[
				SameQ[pat, _Integer],
					"<Integer>",
				SameQ[pat, _Real],
					"<Real>   ",
				SameQ[pat, _String],
					"<String> ",
				SameQ[pat, _Symbol],
					"<Symbol> ",
				SameQ[True | False] || SameQ[pat, _?BooleanQ],
					"<Boolean>",
				True,
					"<" <> ToString[pat, InputForm] <> ">"
			];
		info =
			Map[
				Function[
					e,
					Which[
						MatchQ[e, _String],
							<|
								"Long" -> e,
								"Short" -> Missing[],
								"Pattern" -> _?BooleanQ,
								"Default" -> False
							|>,
						MatchQ[e, {_String, _String}],
							<|
								"Long" -> e[[1]],
								"Short" -> e[[2]],
								"Pattern" -> _?BooleanQ,
								"Default" -> False
							|>,
						MatchQ[
							e,
							Rule[_String | {_String, _String}, Except[_Rule]]
						],
							<|
								"Long" -> If[ListQ[e[[1]]], e[[1, 1]], e[[1]]],
								"Short" -> If[
									ListQ[e[[1]]],
									e[[1, 2]],
									Missing[]
								],
								"Pattern" -> e[[2]],
								"Default" -> Missing[]
							|>,
						MatchQ[
							e,
							Rule[_String | {_String, _String}, Rule[_, _]]
						],
							<|
								"Long" -> If[ListQ[e[[1]]], e[[1, 1]], e[[1]]],
								"Short" -> If[
									ListQ[e[[1]]],
									e[[1, 2]],
									Missing[]
								],
								"Pattern" -> e[[2, 1]],
								"Default" -> e[[2, 2]]
							|>,
						True,
							<|
								"Long" -> "UNKNOWN",
								"Short" -> Missing[],
								"Pattern" -> _String,
								"Default" -> Missing[]
							|>
					]
				],
				spec
			];
		longest = Max[StringLength /@ (info[[All, "Long"]])];
		fmtLine[assoc_] :=
			Module[{
				flagPart,
				patTxt,
				defTxt,
				pat = assoc["Pattern"],
				def = assoc["Default"]
			},
				flagPart =
					"\t--" <>
					assoc["Long"] <>
					If[!MissingQ[assoc["Short"]], ", -" <> assoc["Short"], ""];
				flagPart =
					flagPart <>
					StringRepeat[
						" ",
						Max[0, longest + 8 - StringLength[flagPart]]
					];
				patTxt = If[MatchQ[pat, Except[_?BooleanQ]], patternLabel[pat]];
				defTxt =
					Which[
						MissingQ[def],
							"",
						def === False && MatchQ[pat, _?BooleanQ],
							"",
						True,
							" (default: " <> ToString[def, InputForm] <> ")"
					];
				flagPart <> patTxt <> defTxt
			];
		StringRiffle[fmtLine /@ info, "\n"]
	]

generateInterfaceCommandsHelp[commands_Association] :=
	Module[{cmdList, longest, fmtLine},
		cmdList = Keys[commands];
		longest =
			Max[
				StringLength /@ Map[
					Function[
						cmd,
						Which[
							MatchQ[cmd, {_, _String}],
								ToString[cmd[[1]]],
							True,
								ToString[cmd]
						]
					],
					cmdList
				]
			];
		fmtLine[cmd_] :=
			Module[{cmdStr, desc},
				{cmdStr, desc} =
					Which[
						MatchQ[cmd, {_, _String}],
							{ToString[cmd[[1]]], cmd[[2]]},
						True,
							{ToString[cmd], "Custom command"}
					];
				"\t" <>
				cmdStr <>
				StringRepeat[" ", Max[0, longest + 4 - StringLength[cmdStr]]] <>
				desc
			];
		StringRiffle[fmtLine /@ cmdList, "\n"]
	]

(* ::Subsubsection:: *)
(* Main *)
CommandLineTools["Help", opts : OptionsPattern[]] :=
	Module[{
		spec = OptionValue["ArgumentSpecification"],
		base = OptionValue["HelpMessage"],
		commands = OptionValue["InterfaceCommands"],
		result
	},
		result = base;
		(* Add interface commands section *)
		If[
			TrueQ[OptionValue["InterfaceHelp"]] && Length[commands] > 0,
			result =
				result <>
				"\n\nCommands:\n\n" <>
				generateInterfaceCommandsHelp[commands]
		];
		(* Add options section if specification exists *)
		If[
			TrueQ[OptionValue["OptionHelp"]] && spec =!= None,
			result = result <> "\n\nOptions:\n\n" <> generateHelpFromSpec[spec]
		];
		result
	]
