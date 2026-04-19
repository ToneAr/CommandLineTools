With[{file = FileNameJoin[DirectoryName @ $TestFileName, "CommandLineTools.wl"]},
	If[ FileExistsQ @ file,
		Get @ file
	]
];

(* ================================================================== *)
(* Section 1 - validateOptions                                         *)
(* ================================================================== *)

(* Valid options return Success *)
VerificationTest[
	!FailureQ @ validateOptions[
		"CommandLine" -> {"myscript", "--port", "8080"},
		"InterfacePrompt" -> "cli> ",
		"OptionDelimiter" -> (" " | "="),
		"ArgumentDelimiter" -> ","
	],
	True,
	TestID -> "validateOptions-valid-returns-success"
];

(* Invalid CommandLine value fails *)
VerificationTest[
	FailureQ @ validateOptions[ "CommandLine" -> 42 ],
	True,
	TestID -> "validateOptions-invalid-CommandLine"
];

(* Invalid ArgumentSpecification fails *)
VerificationTest[
	FailureQ @ validateOptions[ "ArgumentSpecification" -> "bad" ],
	True,
	TestID -> "validateOptions-invalid-ArgumentSpecification"
];

(* None is a valid ArgumentSpecification *)
VerificationTest[
	!FailureQ @ validateOptions[ "ArgumentSpecification" -> None ],
	True,
	TestID -> "validateOptions-ArgumentSpecification-None"
];


(* ================================================================== *)
(* Section 2 - Internal spec utilities                                 *)
(* ================================================================== *)

Block[{spec = {
		"verbose",
		{"output", "o"},
		"count" -> _Integer,
		{"format", "f"} -> _String -> "json",
		"mode" -> _String -> "fast"
	}},

	(* getLongFlags *)
	VerificationTest[
		getLongFlags[spec],
		{"verbose", "output", "count", "format", "mode"},
		TestID -> "getLongFlags-basic"
	];

	(* getShortFlags *)
	VerificationTest[
		getShortFlags[spec],
		{"o", "f"},
		TestID -> "getShortFlags-basic"
	];

	(* getAllFlags — long names + pair lists as nested lists *)
	VerificationTest[
		MemberQ[ getAllFlags[spec], "output" ],
		True,
		TestID -> "getAllFlags-contains-long"
	];
	VerificationTest[
		MemberQ[ getAllFlags[spec], {"format", "f"} ],
		True,
		TestID -> "getAllFlags-contains-pair"
	];

	(* selectArg — by long name *)
	VerificationTest[
		selectArg[spec, "output"],
		{"output", "o"},
		TestID -> "selectArg-by-long-name"
	];

	(* selectArg — by short name *)
	VerificationTest[
		selectArg[spec, "o"],
		{"output", "o"},
		TestID -> "selectArg-by-short-name"
	];

	(* selectArg — by long name with leading dashes stripped *)
	VerificationTest[
		selectArg[spec, "--count"],
		"count" -> _Integer,
		TestID -> "selectArg-strips-dashes"
	];

	(* selectArg — missing entry *)
	VerificationTest[
		MissingQ @ selectArg[spec, "unknown"],
		True,
		TestID -> "selectArg-missing"
	];

	(* selectArgDefault — boolean-only entry → False *)
	VerificationTest[
		selectArgDefault[spec, "verbose"],
		False,
		TestID -> "selectArgDefault-boolean-flag"
	];

	(* selectArgDefault — pattern-only entry (no default) → Missing *)
	VerificationTest[
		MissingQ @ selectArgDefault[spec, "count"],
		True,
		TestID -> "selectArgDefault-no-default"
	];

	(* selectArgDefault — entry with explicit default *)
	VerificationTest[
		selectArgDefault[spec, "format"],
		"json",
		TestID -> "selectArgDefault-with-default"
	];

	(* selectArgPattern — boolean flag *)
	VerificationTest[
		MatchQ[ selectArgPattern[spec, "verbose"], _PatternTest | _Alternatives | _Pattern ],
		True,
		TestID -> "selectArgPattern-boolean"
	];

	(* selectArgPattern — typed flag *)
	VerificationTest[
		selectArgPattern[spec, "count"],
		_Integer,
		TestID -> "selectArgPattern-integer"
	];

	(* selectArgPattern — typed flag with default *)
	VerificationTest[
		selectArgPattern[spec, "format"],
		_String,
		TestID -> "selectArgPattern-string-with-default"
	];
];

(* addDefaults fills in declared defaults *)
VerificationTest[
	addDefaults[
		{"port" -> _Integer -> 3000, {"verbose", "v"} -> _?BooleanQ -> False},
		{"verbose" -> True}
	],
	<| "port" -> 3000, "verbose" -> True, "v" -> True |>,
	TestID -> "addDefaults-fills-defaults-and-mirrors-alias"
];

(* addDefaults already-set values are not overwritten *)
VerificationTest[
	addDefaults[
		{"port" -> _Integer -> 3000},
		{"port" -> 9090}
	],
	<| "port" -> 9090 |>,
	TestID -> "addDefaults-existing-value-not-overwritten"
];


(* ================================================================== *)
(* Section 3 - Parse (no spec)                                         *)
(* ================================================================== *)

(* Long flag with value *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> {"myscript", "--port", "8080"}
	],
	<| "$0" -> "myscript", "port" -> "8080" |>,
	TestID -> "Parse-long-flag-value"
];

(* Long flag with = delimiter *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> {"myscript", "--port=8080"}
	],
	<| "$0" -> "myscript", "port" -> "8080" |>,
	TestID -> "Parse-long-flag-equals-delimiter"
];

(* Boolean long flag (no following value) *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> {"myscript", "--verbose"}
	],
	<| "$0" -> "myscript", "verbose" -> True |>,
	TestID -> "Parse-boolean-long-flag"
];

(* Short flag with value *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> {"myscript", "-p", "8080"}
	],
	<| "$0" -> "myscript", "p" -> "8080" |>,
	TestID -> "Parse-short-flag-value"
];

(* Boolean short flag *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> {"myscript", "-v"}
	],
	<| "$0" -> "myscript", "v" -> True |>,
	TestID -> "Parse-boolean-short-flag"
];

(* Multiple flags *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> {"myscript", "--host", "localhost", "--port", "3000"}
	],
	<| "$0" -> "myscript", "host" -> "localhost", "port" -> "3000" |>,
	TestID -> "Parse-multiple-long-flags"
];

(* Positional argument after flags *)
VerificationTest[
	With[{result =
		CommandLineTools["Parse",
			"CommandLine" -> {"myscript", "--verbose", "file.txt"},
			"ArgumentSpecification" -> {"verbose"}
		]
	},
	{result["verbose"], result["$1"]}
	],
	{True, "file.txt"},
	TestID -> "Parse-positional-after-boolean-flag"
];

(* $0 absent when first token is a flag *)
VerificationTest[
	KeyExistsQ[
		CommandLineTools["Parse",
			"CommandLine" -> {"--verbose"}
		],
		"$0"
	],
	False,
	TestID -> "Parse-no-command-name-when-first-token-is-flag"
];

(* Command line as single string *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> "myscript --host localhost"
	],
	<| "$0" -> "myscript", "host" -> "localhost" |>,
	TestID -> "Parse-string-command-line"
];

(* Multiple comma-delimited values *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> {"myscript", "--tags", "a,b,c"}
	]["tags"],
	{"a", "b", "c"},
	TestID -> "Parse-comma-delimited-values"
];

(* Custom argument delimiter *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> {"myscript", "--tags", "a:b:c"},
		"ArgumentDelimiter" -> ":"
	]["tags"],
	{"a", "b", "c"},
	TestID -> "Parse-custom-argument-delimiter"
];


(* ================================================================== *)
(* Section 4 - Parse (with ArgumentSpecification)                     *)
(* ================================================================== *)

Block[{spec = {
		{"verbose", "v"},
		{"port", "p"} -> _Integer -> 8080,
		{"host", "h"} -> _String -> "localhost",
		"output" -> _String
	}},

	(* Default values are filled in when flags absent *)
	VerificationTest[
		CommandLineTools["Parse",
			"CommandLine" -> {"myscript", "--verbose"},
			"ArgumentSpecification" -> spec
		],
		KeyValuePattern[{
			"$0" -> "myscript", "verbose" -> True, "v" -> True,
			"port" -> 8080, "p" -> 8080,
			"host" -> "localhost", "h" -> "localhost"
		}],
		SameTest -> MatchQ,
		TestID -> "Parse-spec-defaults-filled"
	];

	(* Integer flag is converted from string token *)
	VerificationTest[
		CommandLineTools["Parse",
			"CommandLine" -> {"myscript", "--port", "9090"},
			"ArgumentSpecification" -> spec
		]["port"],
		9090,
		TestID -> "Parse-spec-integer-conversion"
	];

	(* Short flag mirrors to long name *)
	VerificationTest[
		With[{r = CommandLineTools["Parse",
				"CommandLine" -> {"myscript", "-p", "4000"},
				"ArgumentSpecification" -> spec
			]},
			{r["port"], r["p"]}
		],
		{4000, 4000},
		TestID -> "Parse-spec-short-flag-mirrors-to-long"
	];

	(* Boolean flag via short form *)
	VerificationTest[
		CommandLineTools["Parse",
			"CommandLine" -> {"myscript", "-v"},
			"ArgumentSpecification" -> spec
		]["verbose"],
		True,
		TestID -> "Parse-spec-boolean-short-form"
	];

	(* StrictParse: unknown flag → Failure *)
	VerificationTest[
		FailureQ @ CommandLineTools["Parse",
			"CommandLine" -> {"myscript", "--unknown"},
			"ArgumentSpecification" -> spec,
			"StrictParse" -> True
		],
		True,
		TestID -> "Parse-strict-unknown-flag-fails"
	];

	(* StrictParse: value pattern mismatch → Failure *)
	VerificationTest[
		FailureQ @ CommandLineTools["Parse",
			"CommandLine" -> {"myscript", "--port", "notanumber"},
			"ArgumentSpecification" -> spec,
			"StrictParse" -> True
		],
		True,
		TestID -> "Parse-strict-pattern-mismatch-fails"
	];

	(* StrictParse: valid input succeeds *)
	VerificationTest[
		AssociationQ @ CommandLineTools["Parse",
			"CommandLine" -> {"myscript", "--port", "9090"},
			"ArgumentSpecification" -> spec,
			"StrictParse" -> True
		],
		True,
		TestID -> "Parse-strict-valid-input-succeeds"
	];
];


(* ================================================================== *)
(* Section 5 - Parse: POSIX clustering                                *)
(* ================================================================== *)

(* -abc clusters → a, b, c each True *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"myscript", "-abc"},
			"POSIX" -> True
		]},
		{r["a"], r["b"], r["c"]}
	],
	{True, True, True},
	TestID -> "Parse-POSIX-cluster-all-boolean"
];

(* -abV value → a, b True; V -> value *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"myscript", "-ab", "foo"},
			"POSIX" -> True
		]},
		{r["a"], r["b"]}
	],
	{True, "foo"},
	TestID -> "Parse-POSIX-cluster-last-gets-value"
];


(* ================================================================== *)
(* Section 6 - Parse form overload (key extraction)                   *)
(* ================================================================== *)

(* Extract a single named key *)
VerificationTest[
	CommandLineTools["Parse", "port",
		"CommandLine" -> {"myscript", "--port", "1234"}
	],
	"1234",
	TestID -> "Parse-form-single-key"
];

(* Extract key matching Alternatives *)
VerificationTest[
	CommandLineTools["Parse", "port" | "p",
		"CommandLine" -> {"myscript", "-p", "5000"}
	],
	"5000",
	TestID -> "Parse-form-alternatives-key"
];

(* Key not present → Failure *)
VerificationTest[
	FailureQ @ CommandLineTools["Parse", "missing-flag",
		"CommandLine" -> {"myscript", "--verbose"}
	],
	True,
	TestID -> "Parse-form-missing-key-failure"
];


(* ================================================================== *)
(* Section 7 - FlagQ                                                   *)
(* ================================================================== *)

(* Long flag present *)
VerificationTest[
	CommandLineTools["FlagQ", "verbose",
		"CommandLine" -> {"myscript", "--verbose"}
	],
	True,
	TestID -> "FlagQ-long-present"
];

(* Long flag absent *)
VerificationTest[
	CommandLineTools["FlagQ", "verbose",
		"CommandLine" -> {"myscript", "--port", "80"}
	],
	False,
	TestID -> "FlagQ-long-absent"
];

(* Short flag present *)
VerificationTest[
	CommandLineTools["FlagQ", "v",
		"CommandLine" -> {"myscript", "-v"}
	],
	True,
	TestID -> "FlagQ-short-present"
];

(* Short flag absent *)
VerificationTest[
	CommandLineTools["FlagQ", "v",
		"CommandLine" -> {"myscript", "--verbose"}
	],
	False,
	TestID -> "FlagQ-short-absent"
];

(* Alternatives form *)
VerificationTest[
	CommandLineTools["FlagQ", "verbose" | "v",
		"CommandLine" -> {"myscript", "-v"}
	],
	True,
	TestID -> "FlagQ-alternatives-match-short"
];

VerificationTest[
	CommandLineTools["FlagQ", "verbose" | "v",
		"CommandLine" -> {"myscript", "--verbose"}
	],
	True,
	TestID -> "FlagQ-alternatives-match-long"
];

(* POSIX mode: -abc should match -a, -b, -c *)
VerificationTest[
	CommandLineTools["FlagQ", "b",
		"CommandLine" -> {"myscript", "-abc"},
		"POSIX" -> True
	],
	True,
	TestID -> "FlagQ-POSIX-cluster-match"
];

(* FlagQ with string command line *)
VerificationTest[
	CommandLineTools["FlagQ", "port",
		"CommandLine" -> "myscript --port 80"
	],
	True,
	TestID -> "FlagQ-string-command-line"
];

(* Flag with equals delimiter *)
VerificationTest[
	CommandLineTools["FlagQ", "port",
		"CommandLine" -> {"myscript", "--port=80"}
	],
	True,
	TestID -> "FlagQ-equals-delimiter"
];


(* ================================================================== *)
(* Section 8 - Help                                                    *)
(* ================================================================== *)

(* Help returns a String *)
VerificationTest[
	StringQ @ CommandLineTools["Help",
		"HelpMessage" -> "Test tool"
	],
	True,
	TestID -> "Help-returns-string"
];

(* Help includes base message *)
VerificationTest[
	StringContainsQ[
		CommandLineTools["Help", "HelpMessage" -> "My special tool"],
		"My special tool"
	],
	True,
	TestID -> "Help-contains-base-message"
];

(* Help includes Options section when spec given *)
VerificationTest[
	StringContainsQ[
		CommandLineTools["Help",
			"HelpMessage" -> "Tool",
			"ArgumentSpecification" -> { {"verbose", "v"}, "port" -> _Integer -> 3000 },
			"OptionHelp" -> True
		],
		"Options"
	],
	True,
	TestID -> "Help-contains-options-section"
];

(* Help lists flag names from spec *)
VerificationTest[
	StringContainsQ[
		CommandLineTools["Help",
			"HelpMessage" -> "Tool",
			"ArgumentSpecification" -> { {"verbose", "v"}, "port" -> _Integer -> 3000 },
			"OptionHelp" -> True
		],
		"--port"
	],
	True,
	TestID -> "Help-contains-flag-name"
];

(* Help shows default value *)
VerificationTest[
	StringContainsQ[
		CommandLineTools["Help",
			"HelpMessage" -> "Tool",
			"ArgumentSpecification" -> { "port" -> _Integer -> 3000 },
			"OptionHelp" -> True
		],
		"3000"
	],
	True,
	TestID -> "Help-contains-default-value"
];

(* OptionHelp -> False omits the Options section *)
VerificationTest[
	StringContainsQ[
		CommandLineTools["Help",
			"HelpMessage" -> "Tool",
			"ArgumentSpecification" -> { "port" -> _Integer -> 3000 },
			"OptionHelp" -> False
		],
		"Options"
	],
	False,
	TestID -> "Help-OptionHelp-False-omits-section"
];

(* InterfaceHelp -> True adds Commands section *)
VerificationTest[
	StringContainsQ[
		CommandLineTools["Help",
			"HelpMessage" -> "Tool",
			"InterfaceHelp" -> True,
			"InterfaceCommands" -> <|
				{"run", "Execute something"} :> Null
			|>
		],
		"Commands"
	],
	True,
	TestID -> "Help-InterfaceHelp-adds-commands-section"
];


(* ================================================================== *)
(* Section 9 - Regression tests                                        *)
(* ================================================================== *)

(* Regression: flag appearing only after $0 has correct positionals *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"script.wls", "sub", "--flag"}
		]},
		{r["$0"], r["flag"]}
	],
	{"script.wls", True},
	TestID -> "Regression-subcommand-positional-and-flag"
];

(* Regression: multiple positionals indexed correctly *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"script.wls", "--verbose", "a", "b", "c"},
			"ArgumentSpecification" -> {"verbose"}
		]},
		{r["$1"], r["$2"], r["$3"]}
	],
	{"a", "b", "c"},
	TestID -> "Regression-multiple-positionals-indexed"
];

(* Regression: empty command line (no flags) returns all positionals *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"script.wls", "pos1", "pos2"}
		]},
		{r["$0"], r["$1"], r["$2"]}
	],
	{"script.wls", "pos1", "pos2"},
	TestID -> "Regression-no-flags-all-positional"
];

(* Regression: alias mirroring works for short->long and long->short *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"s", "--output", "file.txt"},
			"ArgumentSpecification" -> {{"output", "o"} -> _String}
		]},
		r["o"]
	],
	"file.txt",
	TestID -> "Regression-alias-short-mirrors-from-long"
];

(* Regression: StrictParse with no flags at all → Failure *)
VerificationTest[
	FailureQ @ CommandLineTools["Parse",
		"CommandLine" -> {"script.wls", "pos1"},
		"StrictParse" -> True
	],
	True,
	TestID -> "Regression-StrictParse-no-flags-fails"
];

(* Regression: boolean flag with spec does not consume following positional *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"s", "--verbose", "myfile"},
			"ArgumentSpecification" -> { "verbose" }
		]},
		{r["verbose"], r["$1"]}
	],
	{True, "myfile"},
	TestID -> "Regression-spec-boolean-does-not-consume-positional"
];

(* Regression: long flag via = delimiter with spec and integer pattern *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> {"s", "--count=5"},
		"ArgumentSpecification" -> {"count" -> _Integer}
	]["count"],
	5,
	TestID -> "Regression-equals-delimiter-with-spec-integer"
];

(* Regression: FlagQ returns False for empty command line (no flags) *)
VerificationTest[
	CommandLineTools["FlagQ", "verbose",
		"CommandLine" -> {"script.wls"}
	],
	False,
	TestID -> "Regression-FlagQ-no-flags-returns-false"
];

(* Regression: generateHelpFromSpec handles spec with no short form *)
VerificationTest[
	StringContainsQ[
		CommandLineTools["Help",
			"HelpMessage" -> "t",
			"ArgumentSpecification" -> {"output" -> _String},
			"OptionHelp" -> True
		],
		"--output"
	],
	True,
	TestID -> "Regression-Help-no-short-flag-in-spec"
];

(* Regression: spec entry with pattern but no default — Missing[] default *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"s", "--output", "file.csv"},
			"ArgumentSpecification" -> {"output" -> _String}
		]},
		r["output"]
	],
	"file.csv",
	TestID -> "Regression-spec-no-default-value-taken-from-cli"
];


(* ================================================================== *)
(* Section 10 - Positional argument specification                      *)
(* ================================================================== *)

(* Positional with integer pattern is converted *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"s", "42"},
			"ArgumentSpecification" -> {"$1" -> _Integer}
		]},
		r["$1"]
	],
	42,
	TestID -> "Parse-positional-spec-integer-conversion"
];

(* Positional with string pattern is kept as string *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"s", "hello"},
			"ArgumentSpecification" -> {"$1" -> _String}
		]},
		r["$1"]
	],
	"hello",
	TestID -> "Parse-positional-spec-string-kept"
];

(* Positional with default — value from CLI takes precedence *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"s", "99"},
			"ArgumentSpecification" -> {"$1" -> _Integer -> 0}
		]},
		r["$1"]
	],
	99,
	TestID -> "Parse-positional-spec-default-overridden-by-cli"
];

(* Multiple positionals with mixed patterns *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"s", "hello", "7"},
			"ArgumentSpecification" -> {"$1" -> _String, "$2" -> _Integer}
		]},
		{r["$1"], r["$2"]}
	],
	{"hello", 7},
	TestID -> "Parse-positional-spec-multiple-mixed"
];

(* Positional spec alongside flag spec *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"s", "--port", "3000", "myfile"},
			"ArgumentSpecification" -> {"port" -> _Integer, "$1" -> _String}
		]},
		{r["port"], r["$1"]}
	],
	{3000, "myfile"},
	TestID -> "Parse-positional-spec-with-flag-spec"
];

(* Positional without matching spec entry is kept as raw string *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> {"s", "extra"},
			"ArgumentSpecification" -> {}
		]},
		r["$1"]
	],
	"extra",
	TestID -> "Parse-positional-no-spec-entry-kept-raw"
];


(* ================================================================== *)
(* Section 11 - Quoted string handling in splitCommandLine / Parse     *)
(* ================================================================== *)

(* splitCommandLine: double-quoted value with spaces is one token *)
VerificationTest[
	splitCommandLine["--name \"John Doe\""],
	{"--name", "John Doe"},
	TestID -> "splitCommandLine-double-quoted-value"
];

(* splitCommandLine: single-quoted value with spaces is one token *)
VerificationTest[
	splitCommandLine["--tag 'foo bar'"],
	{"--tag", "foo bar"},
	TestID -> "splitCommandLine-single-quoted-value"
];

(* splitCommandLine: unquoted tokens are split normally *)
VerificationTest[
	splitCommandLine["myscript --verbose --port 8080"],
	{"myscript", "--verbose", "--port", "8080"},
	TestID -> "splitCommandLine-unquoted-normal-split"
];

(* splitCommandLine: quotes stripped from result *)
VerificationTest[
	splitCommandLine["--file \"my file.txt\""],
	{"--file", "my file.txt"},
	TestID -> "splitCommandLine-quotes-stripped"
];

(* splitCommandLine: double-quoted string containing a single quote is one token *)
VerificationTest[
	splitCommandLine["--msg \"it's fine\""],
	{"--msg", "it's fine"},
	TestID -> "splitCommandLine-double-quote-contains-single-quote"
];

(* splitCommandLine: single-quoted string containing a double quote is one token *)
VerificationTest[
	splitCommandLine["--msg 'say \"hi\"'"],
	{"--msg", "say \"hi\""},
	TestID -> "splitCommandLine-single-quote-contains-double-quote"
];

(* splitCommandLine: multiple quoted tokens *)
VerificationTest[
	splitCommandLine["--first \"John\" --last 'Doe'"],
	{"--first", "John", "--last", "Doe"},
	TestID -> "splitCommandLine-multiple-quoted-tokens"
];

(* splitCommandLine: empty string returns empty list *)
VerificationTest[
	splitCommandLine[""],
	{},
	TestID -> "splitCommandLine-empty-string"
];

(* splitCommandLine: quoted string with leading/trailing spaces inside quotes preserved *)
VerificationTest[
	splitCommandLine["--val \" spaced \""],
	{"--val", " spaced "},
	TestID -> "splitCommandLine-inner-spaces-preserved"
];

(* Parse: string command line with double-quoted flag value *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> "myscript --name \"John Doe\""
	]["name"],
	"John Doe",
	TestID -> "Parse-string-cmdline-double-quoted-value"
];

(* Parse: string command line with single-quoted flag value *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> "myscript --name 'Jane Doe'"
	]["name"],
	"Jane Doe",
	TestID -> "Parse-string-cmdline-single-quoted-value"
];

(* Parse: quoted value is treated as a single argument, not split further *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> "myscript --path \"/home/user/my documents\""
	]["path"],
	"/home/user/my documents",
	TestID -> "Parse-quoted-path-with-spaces"
];

(* Parse: mix of quoted and unquoted flags *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> "myscript --host localhost --label \"my server\""
		]},
		{r["host"], r["label"]}
	],
	{"localhost", "my server"},
	TestID -> "Parse-mixed-quoted-and-unquoted-flags"
];

(* Parse: quoted value alongside ArgumentSpecification with _String pattern *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> "myscript --output \"result file.csv\"",
		"ArgumentSpecification" -> {"output" -> _String}
	]["output"],
	"result file.csv",
	TestID -> "Parse-quoted-value-with-string-spec"
];

(* FlagQ: flag present in string command line with quoted values *)
VerificationTest[
	CommandLineTools["FlagQ", "output",
		"CommandLine" -> "myscript --output \"my file.csv\""
	],
	True,
	TestID -> "FlagQ-string-cmdline-quoted-value-flag-present"
];


(* ================================================================== *)
(* Section 12 - --flag='quoted value' and --flag="quoted value" forms  *)
(* ================================================================== *)

(* splitCommandLine: --flag='value with spaces' produces two tokens *)
VerificationTest[
	splitCommandLine["--name='John Doe'"],
	{"--name", "John Doe"},
	TestID -> "splitCommandLine-equals-single-quoted-value"
];

(* splitCommandLine: --flag="value with spaces" produces two tokens *)
VerificationTest[
	splitCommandLine["--name=\"John Doe\""],
	{"--name", "John Doe"},
	TestID -> "splitCommandLine-equals-double-quoted-value"
];

(* splitCommandLine: --flag=unquoted still splits on = *)
VerificationTest[
	splitCommandLine["--port=8080"],
	{"--port", "8080"},
	TestID -> "splitCommandLine-equals-unquoted-value"
];

(* Parse: --flag='value with spaces' as string command line *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> "myscript --name='John Doe'"
	]["name"],
	"John Doe",
	TestID -> "Parse-equals-single-quoted-value"
];

(* Parse: --flag="value with spaces" as string command line *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> "myscript --name=\"John Doe\""
	]["name"],
	"John Doe",
	TestID -> "Parse-equals-double-quoted-value"
];

(* Parse: mix of --flag=value and --flag='spaced value' *)
VerificationTest[
	With[{r = CommandLineTools["Parse",
			"CommandLine" -> "myscript --port=9090 --label='my server'"
		]},
		{r["port"], r["label"]}
	],
	{"9090", "my server"},
	TestID -> "Parse-equals-mixed-quoted-and-plain"
];

(* Parse: --flag='value' with ArgumentSpecification *)
VerificationTest[
	CommandLineTools["Parse",
		"CommandLine" -> "myscript --output='result file.csv'",
		"ArgumentSpecification" -> {"output" -> _String}
	]["output"],
	"result file.csv",
	TestID -> "Parse-equals-quoted-value-with-spec"
];

(* FlagQ: flag with equals-quoted form is detected *)
VerificationTest[
	CommandLineTools["FlagQ", "name",
		"CommandLine" -> "myscript --name='John Doe'"
	],
	True,
	TestID -> "FlagQ-equals-single-quoted-flag-present"
];
