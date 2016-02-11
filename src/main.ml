open Types

let filenameR = {|File "(.+)"|}
let lineR = {|File .+, line (\d+)|}
let chars1R = {|File .+, characters (\d+)|}
let chars2R = {|File .+, characters .+-(\d+)|}

(* helpers for getting the first (presumably only) match in a string *)
let get_match_n n pat str = Pcre.get_substring (Pcre.exec ~pat:pat str) n
let get_match = get_match_n 1
let get_match_maybe pat str =
  try Some (Pcre.get_substring (Pcre.exec ~pat:pat str) 1)
  with Not_found -> None
(* helper for turning Not_found exception into an optional *)
let exec ~pat str = try Some (Pcre.exec ~pat str) with Not_found -> None

let split sep str = Pcre.split ~pat:sep str

(* agnostic extractors, turning err string into proper data structures *)
(* TODO: don't make these raise error *)

(* TODO: differing portion data structure a-la diff table *)
let type_MismatchTypeArguments err _ =
  let allR = {|Error: The constructor ([\w\.]*) *expects[\s]*(\d+) *argument\(s\),\s*but is applied here to (\d+) argument\(s\)|} in
  let typeConstructor = get_match_n 1 allR err in
  let expectedCount = int_of_string @@ get_match_n 2 allR err in
  let actualCount = int_of_string @@ get_match_n 3 allR err in
  Type_MismatchTypeArguments {
    typeConstructor = typeConstructor;
    expectedCount = expectedCount;
    actualCount = actualCount;
  }

let type_UnboundValue err _ =
  let unboundValueR = {|Error: Unbound value ([\w\.]*)|} in
  let unboundValue = get_match unboundValueR err in
  (* TODO: there might be more than one suggestion *)
  let suggestionR = {|Error: Unbound value [\w\.]*[\s\S]Hint: Did you mean (.+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
  Type_UnboundValue {
    unboundValue = unboundValue;
    suggestion = suggestion;
  }

let type_SignatureMismatch err fileInfo = raise Not_found
let type_SignatureItemMissing err fileInfo = raise Not_found

let type_UnboundModule err _ =
  let unboundModuleR = {|Error: Unbound module ([\w\.]*)|} in
  let unboundModule = get_match unboundModuleR err in
  let suggestionR = {|Error: Unbound module [\w\.]*[\s\S]Hint: Did you mean (.+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
  Type_UnboundModule {
    unboundModule = unboundModule;
    suggestion = suggestion;
  }

(* need: if there's a hint, show which record type it is *)
let type_UnboundRecordField err _ =
  let recordFieldR = {|Error: Unbound record field (.+)|} in
  let recordField = get_match recordFieldR err in
  let suggestionR = {|Hint: Did you mean (.+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
    Type_UnboundRecordField {
      recordField = recordField;
      suggestion = suggestion
    }

let type_UnboundConstructor err fileInfo = raise Not_found

let type_UnboundTypeConstructor err _ =
  let constructorR = {|Error: Unbound type constructor (.+)|} in
  let constructor = get_match constructorR err in
  let suggestionR = {|Hint: Did you mean (.+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
    Type_UnboundTypeConstructor {
      namespacedConstructor = constructor;
      suggestion = suggestion
    }

(* need: number of arguments actually applied to it, and what they are *)
(* need: number of args the function asks, and what types they are *)
let type_AppliedTooMany err _ =
  let functionTypeR = {|This function has type (.+)\n +It is applied to too many arguments; maybe you forgot a `;'.|} in
  let functionType = get_match functionTypeR err in
  (* the func type 'a -> (int -> 'b) -> string has 2 arguments *)
  (* strip out false positive -> from nested function types passed as param *)
  let nestedFunctionTypeR = {|\(.+\)|} in
  let cleaned = Pcre.replace ~pat:nestedFunctionTypeR ~templ:"bla" functionType in
  let expectedArgCount = List.length (split "->" cleaned) - 1 in
    Type_AppliedTooMany {
      functionType = functionType;
      expectedArgCount = expectedArgCount;
    }

let type_RecordFieldNotInExpression err fileInfo = raise Not_found
let type_RecordFieldError err fileInfo = raise Not_found
let type_FieldNotBelong err fileInfo = raise Not_found

(* need: where the original expected comes from  *)
let type_IncompatibleType err _ =
  (* the type actual and expected might be on their own line *)
  let actualR = {|Error: This expression has type([\s\S]*?)but an expression was expected of type|} in
  let expectedR = {|Error: This expression has type[\s\S]*?but an expression was expected of type([\s\S]*?)$|} in
  let actual = get_match actualR err |> BatString.trim in
  let expected = get_match expectedR err |> BatString.trim in
    Type_IncompatibleType {
      actual = actual;
      expected = expected;
    }

let type_NotAFunction err _ =
  let actualR = {|This expression has type (.+)\n +This is not a function; it cannot be applied.|} in
  let actual = get_match actualR err in
    Type_NotAFunction {
      actual = actual;
    }

(* TODO: apparently syntax error can be followed by more indications *)
(* need: way, way more information, I can't even *)
let file_SyntaxError err fileInfo =
  let filename = get_match filenameR err in
  let line = int_of_string (get_match lineR err) in
  let chars1 = int_of_string (get_match chars1R err) in
  let chars2 = int_of_string (get_match chars2R err) in

  let allR = {|Error: Syntax error|} in
  let fileLines = Batteries.List.of_enum (BatFile.lines_of filename) in
   (* some syntax errors will make the output point at the last line + 1, char
   0-0, to indicate e.g. "there's something missing at the end here". Ofc that
   isn't a valid range to point to in a reporter. See syntax error test 3 *)
  let passedBoundary = List.length fileLines = line - 1 in
  (* raise the same error than if we failed to match *)
  if not (Pcre.pmatch ~pat:allR err) then raise Not_found
  else
    let correctedLineNum = if passedBoundary then line - 1 else line in
    let correctedChars2 = if passedBoundary then chars2 + 1 else chars2 in
    let hintR = {|Error: Syntax error: (.+)|} in
    let hint = get_match_maybe hintR err in
    File_SyntaxError {
      (* fileInfo = {
        content = Batteries.List.of_enum (BatFile.lines_of filename);
        name = filename;
        line = correctedLineNum;
        cols = (chars1, correctedChars2);
      }; *)
      hint = hint;
      offendingString = BatString.slice
        ~first:chars1
        ~last:correctedChars2
        (BatList.at fileLines (correctedLineNum - 1));
    }

let build_InconsistentAssumptions err fileInfo = raise Not_found
let warning_UnusedVariable err fileInfo = raise Not_found

(* need: what the variant is. If it's e.g. a list, instead of saying "doesn't
cover all the cases of the variant" we could say "doesn't cover all the possible
length of the list" *)
let warning_PatternNotExhaustive err fileInfo =
  (* let filename = get_match filenameR err in
  let line = int_of_string (get_match lineR err) in
  let chars1 = int_of_string (get_match chars1R err) in *)

  (* let chars2 = int_of_string (get_match chars2R err) in *)

  let unmatchedR = {|Warning 8: this pattern-matching is not exhaustive.\nHere is an example of a value that is not matched:\n(.+)|} in
  let unmatchedRaw = get_match unmatchedR err in
  (* ocaml has a bug/feature where it'll show that the range of the error for
  the variant is from the first letter of "match bla with ..." to the end of the
  whole pattern block, **as if the code was written on a single line**. This
  might be because their file line reporting got too rigid and can't show you
  line range. But for us it's pretty useless so we limit it to just highlight
  the word match *)
  let unmatched = if (BatString.get unmatchedRaw 0) = '(' then
    (* format was (Variant1|Variant2|Variant3). We strip the surrounding parens *)
    unmatchedRaw
    |> BatString.lchop
    |> BatString.rchop
    |> split {|\||}
  else
    [unmatchedRaw]
  in

  Warning_PatternNotExhaustive {
    (* fileInfo = {
      content = Batteries.List.of_enum (BatFile.lines_of filename);
      name = filename;
      line = line;
      (* TODO: change this. Isn't necessarily the word match that's highlighted *)
      cols = (chars1, chars1 + 5);
    }; *)
    unmatched = unmatched;
    warningCode = 8;
  }

let warning_PatternUnused err fileInfo = raise Not_found

(* need: offending optional argument name from AST *)
(* need: offending function name *)
let warning_OptionalArgumentNotErased err _ =
  let filename = get_match filenameR err in
  let line = int_of_string (get_match lineR err) in
  let chars1 = int_of_string (get_match chars1R err) in
  let chars2 = int_of_string (get_match chars2R err) in
  (* Hardcoding 16 for now. We might one day switch to use the variant from
  https://github.com/ocaml/ocaml/blob/901c67559469acc58935e1cc0ced253469a8c77a/utils/warnings.ml#L20 *)
  let allR = {|Warning 16: this optional argument cannot be erased\.|} in
  let fileLines = Batteries.List.of_enum (BatFile.lines_of filename) in
  let fileLine = BatList.at fileLines (line - 1) in
  let _ = get_match_n 0 allR err in
    Warning_OptionalArgumentNotErased {
      warningCode = 16;
      argumentName = BatString.slice ~first:chars1 ~last:chars2 fileLine;
    }

(* need: list of legal characters *)
let file_IllegalCharacter err _ =
  let characterR = {|Error: Illegal character \((.+)\)|} in
  let character = get_match characterR err in
    File_IllegalCharacter {
      character = character;
    }

let warning_CatchAll err _ =
  (* let allR = {|Warning (\d+): ([\s\S]+)|} in *)
  (* let warningCode = int_of_string (get_match_n 1 allR err) in *)
  (* let message = get_match_n 2 allR err in *)
  Warning_CatchAll "hi"
  (* Warning_CatchAll {
    fileInfo = {
      content = Batteries.List.of_enum (BatFile.lines_of filename);
      name = filename;
      line = line;
      cols = (chars1, chars2);
    };
    warningCode = warningCode;
    message = message;
  } *)

(* let unparsableButWithFileInfo err fileInfo =
  let filename = get_match filenameR err in
  let line = int_of_string (get_match lineR err) in
  let chars1 = int_of_string (get_match chars1R err) in
  let chars2 = int_of_string (get_match chars2R err) in

  let errorR = {|Error: ([\s\S]+)|} in
  let error = get_match errorR err in
  UnparsableButWithFileInfo {
    fileInfo = {
      content = Batteries.List.of_enum (BatFile.lines_of filename);
      name = filename;
      line = line;
      cols = (chars1, chars2);
    };
    error = error;
  } *)

let unparsable err fileInfo = Unparsable err

let hasErrorOrWarningR = Pcre.regexp
  ~flags:[Pcre.(`MULTILINE)]
  (* the all-caps ERROR is left by oasis when compilation fails bc of artifacts
  left in project folders *)
  {|^(Error|ERROR|Warning \d+): |}

let rec splitInto ~chunckSize (l: 'a list): 'a list list =
  if BatList.length l <= chunckSize || chunckSize = 0 then [l]
  else (BatList.take chunckSize l) :: (splitInto ~chunckSize (BatList.drop chunckSize l))

(* the compiler output might point to an error that spans across many lines;
however, instead of indicating from (startRow, startColumn) to (endRow,
endColumn), it'll indicate (startRow, startColumn, endColumn) where endColumn
might belong to a different row! We normalize and find the row here *)

(* the compiler line number is 1-indexed, and col number is 0-indexed but the
endColumn for an error goes past the last "expected" endColumn, e.g. if it's
`typ a = string`
instead of saying it's from 0 to 2, it shows as 0 to 3. This is also kinda
expected, since you get easy column count through 3 - 0 *)

(* we'll use 0-indexed. It's a reporter (printer)'s job to normalize to
1-indexed if it desires so *)
let normalizeCompilerLineColsToRange ~fileLines ~lineRaw ~col1Raw ~col2Raw =
(* accept strings to constraint usage to parse directly from raw data *)
  let startRow = (int_of_string lineRaw) - 1 in
  (* some error msgs don't have column numbers; we normal them to 0 here *)
  let col1 = BatOption.map_default int_of_string 0 col1Raw in
  let col2 = BatOption.map_default int_of_string 0 col2Raw in
  let currRow = ref startRow in
  let totalCharsRemaining = ref (col2 - col1) in
  let currCol = ref col1 in
  (* crawling back to imperative programming begging for forgiveness *)
  while !totalCharsRemaining > 0 do
    let currLine = BatList.at fileLines !currRow in
    (* no need for an extra - 1 here bc of now col1 is given; see comments above *)
    let remainingCharsCountOnLine = (BatString.length currLine) - !currCol in
    (* TODO: -1 here? +1 here? *)
    totalCharsRemaining := !totalCharsRemaining - remainingCharsCountOnLine;
    if !totalCharsRemaining > 0 then (
      currRow := !currRow + 1;
      currCol := 0
    ) else
      (* break *)
      currCol := !currCol + remainingCharsCountOnLine
  done;
  (* (startRow, startColumn), (endRow, endColumn) *)
  ((startRow, col1), (!currRow, !currCol))

(* has the side-effect of reading the file *)
let extractFromFileMatch fileMatch: (fileInfo * Atom.Range.t * string) =
  Pcre.(
    match fileMatch with
    | [Delim _; Group (_, fileName); Group (_, lineNum); col1; col2; Text text] ->
      let cachedContent = Batteries.List.of_enum (BatFile.lines_of fileName) in
      let (col1Raw, col2Raw) = match (col1, col2) with
        | (Group (_, c1), Group (_, c2)) -> (Some c1, Some c2)
        | _ -> (None, None)
      in
      (
        {path = fileName; cachedContent = cachedContent},
        (normalizeCompilerLineColsToRange
          ~fileLines:cachedContent
          ~lineRaw:lineNum
          ~col1Raw:col1Raw
          ~col2Raw:col2Raw
        ),
        BatString.trim text
      )
    | _ ->
      BatList.iter (fun x ->
        match x with
        | Delim a -> print_endline @@ "Delim " ^ a
        | Group (_, a) -> print_endline @@ "Group " ^ a
        | Text a -> print_endline @@ "Text " ^ a
        | NoGroup -> print_endline @@ "NoGroup"
      ) fileMatch;
      raise (invalid_arg "Couldn't extract error")
  )

let syntaxErr = {|File "tests/file_SyntaxError/file_SyntaxError_3.ml", line 2, characters 0-0:
Error: Syntax error: ')' expected
File "tests/file_SyntaxError/file_SyntaxError_3.ml", line 1, characters 8-9:
Error: This '(' might be unmatched
|}

let badFileErr = {|File "tests/1_bad_file_name/1_bad_file_name_1.ml", line 1:
Warning 24: bad source file name: "1_bad_file_name_1" is not a valid module name.
|}

let noErr = {||}

let badBuildOutput1 = {|ocaml setup.ml -build
SANITIZE: a total of 4 files that should probably not be in your source tree
  has been found. A script shell file
  "/Users/chenglou/Documents/Github/ocaml-better-errors/_build/sanitize.sh"
  is being created. Check this script and run it to remove unwanted files or
  use other options (such as defining hygiene exceptions or using the
  -no-hygiene option).
IMPORTANT: I cannot work with leftover compiled files.
ERROR: Leftover OCaml compilation files:
  File 1_bad_file_name_1.cmo in tests/1_bad_file_name has suffix .cmo
  File 1_bad_file_name_1.cmi in tests/1_bad_file_name has suffix .cmi
ERROR: Leftover OCaml compilation files:
  File noError_1.cmo in tests/noError has suffix .cmo
  File noError_1.cmi in tests/noError has suffix .cmi
Exiting due to hygiene violations.
E: Failure("Command ''/Users/chenglou/.opam/4.02.3/bin/ocamlbuild' src/main.byte tests/test.byte src/table.byte -use-ocamlfind -tag debug' terminated with error code 1")
make: *** [build] Error 1
|}

let badBuildOutput2 = {|ocaml setup.ml -build
/Users/chenglou/.opam/4.02.3/bin/ocamlfind ocamlc -c -g -annot -bin-annot -package Pcre -package Batteries -package ANSITerminal -I src -o src/main.cmo src/main.ml
+ /Users/chenglou/.opam/4.02.3/bin/ocamlfind ocamlc -c -g -annot -bin-annot -package Pcre -package Batteries -package ANSITerminal -I src -o src/main.cmo src/main.ml
File "src/main.ml", line 439, characters 10-19:
Error: Unbound value fileMatch
Hint: Did you mean fileMatc?
Command exited with code 2.
|}

let doThis (err): result =
  (* try
    let err = BatPervasives.input_all stdin in *)
    (* TODO: this isn't good enough bc other compilation failure might pass e.g.
    artifact *)
    let fileR = Pcre.regexp
      ~flags:[Pcre.(`MULTILINE)]
      {|^File "([\s\S]+?)", line (\d+)(?:, characters (\d+)-(\d+))?:$|}
    in
    if not (Pcre.pmatch ~rex:hasErrorOrWarningR err) then NoErrorNorWarning err
    else
      let errorContent = BatString.trim err
        |> Pcre.full_split ~rex:fileR
        (* First few rows might be random output info *)
        |> BatList.drop_while (function Pcre.Text _ -> true | _ -> false)
      in
      if BatList.length errorContent = 0 then Unparsable err
      else (
        (* print_int @@ BatList.length errorContent;
        BatList.iteri (fun i x ->
          print_int i;
          print_endline "";
          Pcre.(
            match x with
            | Delim a -> print_endline @@ "Delim " ^ a
            | Group (_, a) -> print_endline @@ "Group " ^ a
            | Text a -> print_endline @@ "Text " ^ a
            | NoGroup -> print_endline @@ "NoGroup"
          )
        ) errorContent; *)

        let files =
          BatString.trim err
          |> Pcre.full_split ~rex:fileR
          |> BatList.drop_while (function Pcre.Text _ -> true | _ -> false)
          (* we match 6 items, so the whole list will always be a multiple of 6 *)
          |> splitInto ~chunckSize:6
          (* TODO: this might be wrong, bc it assumes "FIle:" appears at first
          row. First few rows might be random output info *)
          |> BatList.map extractFromFileMatch
        in
        let errorOrWarningR = Pcre.regexp
          ~flags:[Pcre.(`MULTILINE)]
          {|^(?:(Error)|(Warning) \d+): |}
        in
        let errorParsers: (string -> fileInfo -> error) list = [
          type_MismatchTypeArguments;
          type_UnboundValue;
          type_SignatureMismatch;
          type_SignatureItemMissing;
          type_UnboundModule;
          type_UnboundRecordField;
          type_UnboundConstructor;
          type_UnboundTypeConstructor;
          type_AppliedTooMany;
          type_RecordFieldNotInExpression;
          type_RecordFieldError;
          type_FieldNotBelong;
          type_IncompatibleType;
          type_NotAFunction;
          file_SyntaxError;
          build_InconsistentAssumptions;
          file_IllegalCharacter;
        ]
        in
        let warningParsers: (string -> fileInfo -> warningType) list = [
          warning_UnusedVariable;
          warning_PatternNotExhaustive;
          warning_PatternUnused;
          warning_OptionalArgumentNotErased;
          (* TODO: don't put a catchall here. already commented out for now *)
          (* warning_CatchAll; *)
        ]
        in
        let filesAndErrorsAndWarnings: fileAndErrorsAndWarnings list =
        files |> BatList.map (fun (fileInfo, range, text) ->
          let errorsAndWarnings =
            Pcre.full_split ~rex:errorOrWarningR text
            (* we match 4 items, so the whole list will always be a multiple of 4 *)
            |> splitInto ~chunckSize:4
          in
          let errors = errorsAndWarnings |> Pcre.(BatList.filter_map (function
            | [Delim err; Group _; NoGroup; Text text] -> Some (err ^ text)
            | _ -> None
          ))
          in
          let warnings = errorsAndWarnings |> Pcre.(BatList.filter_map (function
            | [Delim warning; NoGroup; Group _; Text text] -> Some (warning ^ text)
            | _ -> None
          ))
          in
          let errs =
            errors
            |> BatList.map (fun errorRaw ->
                let result =
                  try
                    BatList.find_map (fun errorParser ->
                      try Some (errorParser errorRaw fileInfo)
                      with _ -> None)
                    errorParsers
                  with Not_found -> Error_CatchAll "hurr"
                in
                {
                  range = range;
                  parsedContent = result;
                }
            )
          in
          let warns =
            warnings
            |> BatList.map (fun warningRaw ->
              let result = {
                (* TODO: extract this... *)
                code = 10;
                warningType =
                  try
                    BatList.find_map (fun warningParser ->
                      try Some (warningParser warningRaw fileInfo)
                      with _ -> None)
                    warningParsers
                  with Not_found -> Warning_CatchAll "hurr"
              }
              in {
                range = range;
                parsedContent = result;
              }
            )
          in {fileInfo = fileInfo; errors = errs; warnings = warns}
        )
        in
        ErrorsAndWarnings filesAndErrorsAndWarnings
      )
  (* with BatIO.No_more_input -> () *)

let () =
  Reporter.print @@ doThis syntaxErr;
  print_endline "=======1===========";
  Reporter.print @@ doThis badFileErr;
  print_endline "=======2===========";
  Reporter.print @@ doThis noErr;
  print_endline "=======3===========";
  Reporter.print @@ doThis badBuildOutput2;
  print_endline "=======4===========";
  Reporter.print @@ doThis badBuildOutput1;
  (* print_endline "=======5===========" *)

(* entry point, for convenience purposes for now. Theoretically the parser and
the reporters are decoupled *)
(* let () =
  try
    let err = BatPervasives.input_all stdin in
    let errLines = split "\n" err in
      let matched = BatList.find_map (fun f ->
        try Some (f err errLines) with _ -> None
      ) parsers in Reporter.print matched
  with BatIO.No_more_input -> () *)
