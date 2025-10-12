# CLITools

This resource function exposes various utilities for aiding in the creation of
Wolfram Language command‑line scripts: parsing flags/arguments, detecting flags,
building interactive REPL‑style interfaces, and structured logging.

## Interfaces
This section defines interface patterns referred to in the Methods section.

```wl
stringFormP =
	Alternatives[
		_String,
		_StringExpression,
		_RegularExpression,
		_Alternatives,
		_Repeated
	];
```

## Methods

- ### Parse
The parse method will work differently depending on the "ArgumentSpecification"
Option. By default, it is set to `None`, and there are no pre-defined argument
names or patterns.

```wl
{
	<|
		"Form" :> CLITools["Parse"] /; (
			OptionValue[CLITools, "ArgumentSpecification"] === None
		),
		"Return" :> Alternatives[
			<|( option_String -> argument_ )...|>,
			fail_Failure
		]
	|>,
	<|
		"Form"   :> CLITools["Parse", stringForm: stringFormP] /; (
			OptionValue[CLITools, "ArgumentSpecification"] === None
		),
		"Return" :> Alternative[
			<|
				Repeated[
					key_?StringMatchQ[stringForm] -> argument_,
					{2, Infinity}
				]
			|>,
			argument_,
			fail_Failure
		]
	|>
}
```

However, the `Option` "ArgumentSpecification" can be set to a `List` that
describes expected flags (long and/or short), their value pattern, and an
optional default. When provided, parsing is validated and defaults are filled
in automatically.

### ArgumentSpecification Forms

Each element in the specification list can be one of:

1. `"name"` or `{long, short}` — A boolean flag (default `False`). Passing the
	 flag toggles it to `True`. Re‑passing a boolean flag with no explicit value
	 toggles the stored value (internally, encountering a lone boolean flag
	 flips the default if a default boolean was declared).
2. `"name" -> pattern` or `{long, short} -> pattern` — A flag whose value is
	 required and must match `pattern` (matched after converting the string via
	 `ToExpression` using `ResourceFunction["ToExpressionMatched"]`).
3. `"name" -> pattern -> default` or `{long, short} -> pattern -> default` — A
	 flag with a value that, if omitted, is initialized to `default`.

Patterns are standard Wolfram Language patterns (`_Integer`, `_String`, etc.).

Example specification:

```wl
spec = {
	"verbose",                            (* boolean, default False *)
	{"output", "o"} -> _String -> "stdout.txt",
	"count" -> _Integer -> 1,
	{"threshold", "t"} -> _Real,
	"dryRun"                                (* boolean *)
};
```

### Parsing Semantics with a Specification

Input command line (illustrative):

```wl
CLITools["Parse",
	"CommandLine" -> {"script.wls", "--verbose", "--count=5", "-ot", "results.txt",
		"--threshold", "0.25"},
	"ArgumentSpecification" -> spec
]
```

Steps / behavior:

* Tokens before the first option (script name etc.) are ignored.
* Long options may appear as `--name value` or `--name=value`.
* Short options can be clustered: `-ot` is expanded to `-o -t` (with the last
	short flag allowed to consume an attached value if supplied as `-t0.25` or by
	following token / delimiter forms; see limitations below).
* Boolean flags without explicit value invert (toggle) their default. If no
	default is provided they become `True` when seen the first time.
* Values containing the argument delimiter (default `","`) are split into a
	list, otherwise left atomic.
* After successful parse, unspecified flags that have defaults are inserted.

Example result (schematic):

```wl
<|
	"verbose" -> True,
	"output"  -> "results.txt",
	"count"   -> 5,
	"threshold" -> 0.25,
	"dryRun" -> False
|>
```

Failures return a `Failure[ ... ]` object (e.g. unknown flag or pattern
validation mismatch when `StrictParse -> True`).

### StrictParse

`"StrictParse" -> True` (default) causes unknown flags or pattern mismatches to
produce a `Failure`. Setting it to `False` relaxes validation (unknown flags are
still collected but not rejected; pattern conversion is more permissive).

### Value Conversion & Pattern Matching

When a pattern is specified (non‑boolean), the raw string (or each element of a
delimited value list) is converted via `ResourceFunction["ToExpressionMatched"]`
and then validated against the pattern. A mismatch raises a `Failure`.

### Multi-Value Arguments

If an argument value contains the argument delimiter (default `","`), it is
split. A single resulting element is automatically unwrapped to an atom.

Example:

```wl
CLITools["Parse",
	"CommandLine" -> {"script.wls", "--tags=alpha,beta,gamma"},
	"ArgumentSpecification" -> {"tags" -> _String}
]
(* <| "tags" -> {"alpha", "beta", "gamma"} |> *)
```

If you prefer the raw string, choose a different delimiter or set
`"ArgumentDelimiter" -> "\0"` (an unused character) to effectively disable
splitting.

### Undefined Specification Mode Recap

If `"ArgumentSpecification" -> None`, parsing is permissive and returns either:

* Association of arbitrary flags encountered, each mapped to either `True` or a
	parsed value (string or list), or
* Extracted argument for a specific form if using `CLITools["Parse", form]`.

### Additional Examples

```wl
(* Simple permissive parse *)
CLITools["Parse", "CommandLine" -> {"script.wls", "--foo=bar", "-x", "--list", "a,b"}]
(* <| "foo" -> "bar", "x" -> True, "list" -> {"a", "b"} |> *)

(* Specified parse with validation *)
CLITools["Parse",
	"CommandLine" -> {"script.wls", "--count=10", "-v"},
	"ArgumentSpecification" -> {"count" -> _Integer -> 1, "v"}
]
(* <| "count" -> 10, "v" -> True |> *)
```


- ### FlagQ

Determines whether a flag matching a pattern exists in the current command
line. Accepts a single `stringFormP` (string, string expression, regular
expression, alternatives, or repeated). Returns `True|False`.

```wl
{
	<|
		"Form"   :> CLITools["FlagQ", form: stringFormP],
		"Return" :> True | False
	|>
}
```

Examples:

```wl
CLITools["FlagQ", "verbose", "CommandLine" -> {"script.wls", "--verbose"}]
(* True *)

CLITools["FlagQ", "quiet", "CommandLine" -> {"script.wls", "--verbose"}]
(* False *)
```

Notes:
* Matching allows 0–2 leading dashes internally; you typically supply the bare
	name without dashes.
* To search for a family of flags: `CLITools["FlagQ", RegularExpression["verb.*"]]`.

- ### Interface

Provides a lightweight REPL for custom command execution. Two forms:

1. `CLITools["Interface"]` — Uses default help and built‑ins only.
2. `CLITools["Interface", <| pattern :> action, ... |>]` — Adds custom commands.

Built‑in commands:

* `help | h | ?` — Print help message (from `"HelpMessage"` option).
* `exit | quit | q` — Terminate the loop.
* `eval | e` — Enter an input prompt to evaluate an expression.

Custom commands are matched by simple string equality against user input.

Pattern description:

```wl
{
	<|
		"Form"   :> CLITools["Interface", cmds_Association],
		"Return" :> Null | Failure[__]
	|>
}
```

Example:

```wl
CLITools["Interface",
	<|
		"ping" :> Print["pong"],
		"time" :> Print[DateString[]]
	|>,
	"InterfacePrompt" -> "demo>"
];
```

Terminate with `q`.

- ### Log

Structured logging with colorized stdout (via `ANSITools`) and optional file
append. Two primary invocation styles:

1. Direct: `CLITools["Log", type, msg, opts]`
2. Curried: `CLITools["Log", type][msg]` (returns a logging function for the
	 chosen level).

`type` ∈ `{ "INFO", "WARN", "SUCCESS", "ERROR" }`.

Interface pattern:

```wl
{
	<|
		"Form"   :> CLITools["Log", ("INFO"|"WARN"|"SUCCESS"|"ERROR"), msg_String],
		"Return" :> Success[__] | Failure[__]
	|>
}
```

Example:

```wl
CLITools["Log", "INFO", "Starting task"];
withError = CLITools["Log", "ERROR"];
withError@"Could not open file";
```

Resulting `Success` contains the file path and metadata when file logging is
enabled.

### Logging Options

* `"LogToFile" -> True|False` — Enable/disable file append.
* `"LogToStdOut" -> True|False` — Enable/disable console printing.
* `"LogDirectory" -> path_String` — Directory (created if missing). Files are
	daily: `YYYYMMDD.log`.

---

## Options Summary

| Option | Default | Purpose |
| ------ | ------- | ------- |
| `"CommandLine"` | `Automatic` | Source list/string of CLI tokens (falls back to `$CommandLine` or `$ScriptCommandLine`). |
| `"InterfacePrompt"` | `"wls-cli>"` | Prompt string for `Interface`. |
| `"OptionDelimiter"` | `" " \| "="` | Characters used to split option from value (e.g. `--count=5`). |
| `"ArgumentDelimiter"` | `","` | Splits multi-value arguments (e.g. `a,b,c`). |
| `"HelpMessage"` | `Automatic` | Printed by `help` inside interface. |
| `"LogToFile"` | `True` | Enable log file writing. |
| `"LogToStdOut"` | `True` | Enable colored console log output. |
| `"LogDirectory"` | `"./logs/wls-cli"` | Directory for log files. |
| `"ArgumentSpecification"` | `None` | Enable validated parsing when set to a spec list. |
| `"StrictParse"` | `True` | Reject unknown flags / bad values. |

Notes:
* Set a single custom delimiter for values that should remain intact.
* When providing a string as `"CommandLine"`, it is first tokenized by space
	then each token is split by the option delimiter.

## Putting It All Together

```wl
spec = {
	"verbose",
	{"output","o"} -> _String -> "stdout.txt",
	"count" -> _Integer -> 1
};

args = CLITools["Parse",
	"CommandLine" -> {"tool.wls", "--verbose", "--count=5", "-o", "results.log"},
	"ArgumentSpecification" -> spec
];

If[FailureQ[args], Return[args]];

If[TrueQ@args["verbose"], CLITools["Log", "INFO", "Verbose mode enabled"]];
CLITools["Log", "SUCCESS", "Counting to " <> ToString@args["count"]];
```

Sample output (stdout):

```
[INFO]: Verbose mode enabled
[SUCCESS]: Counting to 5
```

## Future Enhancements (Ideas)

* Auto‑generated usage text from `ArgumentSpecification`.
* Support for positional (non‑flag) arguments section.

---

## Changelog

Initial version of README completed with full method and option documentation.

