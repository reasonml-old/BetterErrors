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
let type_MismatchTypeArguments err _ _ =
  let allR = {|The constructor ([\w\.]*) *expects[\s]*(\d+) *argument\(s\),\s*but is applied here to (\d+) argument\(s\)|} in
  let typeConstructor = get_match_n 1 allR err in
  let expectedCount = int_of_string @@ get_match_n 2 allR err in
  let actualCount = int_of_string @@ get_match_n 3 allR err in
  Type_MismatchTypeArguments {
    typeConstructor = typeConstructor;
    expectedCount = expectedCount;
    actualCount = actualCount;
  }

let type_UnboundValue err _ _ =
  let unboundValueR = {|Unbound value ([\w\.]*)|} in
  let unboundValue = get_match unboundValueR err in
  (* TODO: there might be more than one suggestion *)
  let suggestionR = {|Unbound value [\w\.]*[\s\S]Hint: Did you mean (.+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
  Type_UnboundValue {
    unboundValue = unboundValue;
    suggestion = suggestion;
  }

let type_SignatureMismatch err fileInfo = raise Not_found
let type_SignatureItemMissing err fileInfo = raise Not_found

let type_UnboundModule err _ _ =
  let unboundModuleR = {|Unbound module ([\w\.]*)|} in
  let unboundModule = get_match unboundModuleR err in
  let suggestionR = {|Unbound module [\w\.]*[\s\S]Hint: Did you mean (.+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
  Type_UnboundModule {
    unboundModule = unboundModule;
    suggestion = suggestion;
  }

(* need: if there's a hint, show which record type it is *)
let type_UnboundRecordField err _ _ =
  let recordFieldR = {|Unbound record field (.+)|} in
  let recordField = get_match recordFieldR err in
  let suggestionR = {|Hint: Did you mean (.+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
    Type_UnboundRecordField {
      recordField = recordField;
      suggestion = suggestion
    }

let type_UnboundConstructor err fileInfo = raise Not_found

let type_UnboundTypeConstructor err _ _ =
  let constructorR = {|Unbound type constructor (.+)|} in
  let constructor = get_match constructorR err in
  let suggestionR = {|Hint: Did you mean (.+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
    Type_UnboundTypeConstructor {
      namespacedConstructor = constructor;
      suggestion = suggestion
    }

(* need: number of arguments actually applied to it, and what they are *)
(* need: number of args the function asks, and what types they are *)
let type_AppliedTooMany err _ _ =
  let functionTypeR = {|This function has type (.+)\n +It is applied to too many arguments; maybe you forgot a `;'.|} in
  let functionType = get_match functionTypeR err in
  (* the func type 'a -> (int -> 'b) -> string has 2 arguments *)
  (* strip out false positive -> from nested function types passed as param *)
  let nestedFunctionTypeR = {|\(.+\)|} in
  let cleaned = Pcre.replace ~pat:nestedFunctionTypeR ~templ:"bla" functionType in
  let expectedArgCount = BatList.length (split "->" cleaned) - 1 in
    Type_AppliedTooMany {
      functionType = functionType;
      expectedArgCount = expectedArgCount;
    }

let type_RecordFieldNotInExpression err fileInfo range = raise Not_found
let type_RecordFieldError err fileInfo range = raise Not_found
let type_FieldNotBelong err fileInfo range = raise Not_found

(* need: where the original expected comes from  *)
let type_IncompatibleType err _ range =
  (* the type actual and expected might be on their own line *)
  let actualR = {|This expression has type([\s\S]*?)but an expression was expected of type|} in
  let expectedR = {|This expression has type[\s\S]*?but an expression was expected of type([\s\S]*?)$|} in
  let actual = get_match actualR err |> BatString.trim in
  let expected = get_match expectedR err |> BatString.trim in
    Type_IncompatibleType {
      actual = actual;
      expected = expected;
    }

let type_NotAFunction err _ range =
  let actualR = {|This expression has type (.+)\n +This is not a function; it cannot be applied.|} in
  let actual = get_match actualR err in
    Type_NotAFunction {
      actual = actual;
    }

(* TODO: apparently syntax error can be followed by more indications *)
(* need: way, way more information, I can't even *)
let file_SyntaxError err fileInfo range =
  let allR = {|Syntax error|} in
  (* raise the same error than if we failed to match *)
  if not (Pcre.pmatch ~pat:allR err) then
    raise Not_found
  else
    let hintR = {|Syntax error: (.+)|} in
    let hint = get_match_maybe hintR err in
    (* assuming on the same row *)
    let ((startRow, startColumn), (_, endColumn)) = range in
    File_SyntaxError {
      hint = hint;
      offendingString = BatString.slice
        ~first:startColumn
        ~last:endColumn
        (BatList.at fileInfo.cachedContent startRow);
    }

let build_InconsistentAssumptions err fileInfo range = raise Not_found
let warning_UnusedVariable code err fileInfo range = raise Not_found

(* need: what the variant is. If it's e.g. a list, instead of saying "doesn't
cover all the cases of the variant" we could say "doesn't cover all the possible
length of the list" *)
let warning_PatternNotExhaustive code err _ _ =
  let unmatchedR = {|this pattern-matching is not exhaustive.\nHere is an example of a value that is not matched:\n([\s\S]+)|} in
  let unmatchedRaw = get_match unmatchedR err in
  let unmatched = if (BatString.get unmatchedRaw 0) = '(' then
    (* format was (Variant1|Variant2|Variant3). We strip the surrounding parens *)
    unmatchedRaw
    |> BatString.lchop
    |> BatString.rchop
    |> split {|\|[\s]*|}
  else
    [unmatchedRaw]
  in
  Warning_PatternNotExhaustive {
    unmatched = unmatched;
  }

let warning_PatternUnused code err fileInfo range = raise Not_found

(* need: offending optional argument name from AST *)
(* need: offending function name *)
let warning_OptionalArgumentNotErased code err fileInfo range =
  (* assume error on one line *)
  let ((startRow, startColumn), (_, endColumn)) = range in
  (* Hardcoding 16 for now. We might one day switch to use the variant from
  https://github.com/ocaml/ocaml/blob/901c67559469acc58935e1cc0ced253469a8c77a/utils/warnings.ml#L20 *)
  let allR = {|this optional argument cannot be erased\.|} in
  let fileLine = BatList.at fileInfo.cachedContent startRow in
  let _ = get_match_n 0 allR err in
  Warning_OptionalArgumentNotErased {
    argumentName = BatString.slice ~first:startColumn ~last:endColumn fileLine;
  }

(* need: list of legal characters *)
let file_IllegalCharacter err _ _ =
  let characterR = {|Illegal character \((.+)\)|} in
  let character = get_match characterR err in
    File_IllegalCharacter {
      character = character;
    }

let unparsable err fileInfo range = Unparsable err

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
  let line = (int_of_string lineRaw) in
  let fileLength = BatList.length fileLines in
  let isOCamlBeingBadAndPointingToALineBeyondFileLength = line > fileLength in
  let (col1, col2) = if isOCamlBeingBadAndPointingToALineBeyondFileLength then
    let lastDamnReachableSpotInTheFile =
      BatString.length @@ BatList.at fileLines (fileLength - 1)
    in (lastDamnReachableSpotInTheFile - 1, lastDamnReachableSpotInTheFile)
  else
    match (col1Raw, col2Raw) with
    | (Some a, Some b) -> (int_of_string a, int_of_string b)
    (* some error msgs don't have column numbers; we normal them to 0 here *)
    | _ -> (0, 0)
  in
  let startRow = if isOCamlBeingBadAndPointingToALineBeyondFileLength then
    fileLength - 1
  else
    line - 1
  in
  let currentLine = BatList.at fileLines startRow in
  let numberOfCharsBetweenStartAndEndColumn = col2 - col1 in
  let numberOfCharsLeftToCoverOnStartingRow =
    (* +1 bc ocaml looooves to count new line as a char below when the error
    spans multiple lines*)
    (BatString.length currentLine) - col1 + 1
  in
  if numberOfCharsBetweenStartAndEndColumn <= numberOfCharsLeftToCoverOnStartingRow then
    ((startRow, col1), (startRow, col2))
  else
    let howManyCharsLeftToCoverOnSubsequentLines =
      ref (numberOfCharsBetweenStartAndEndColumn - numberOfCharsLeftToCoverOnStartingRow)
    in
    let suddenlyFunctionalProgrammingOutOfNowhere =
      fileLines
      |> BatList.drop (startRow + 1)
      |> BatList.map BatString.length
      |> BatList.take_while (fun numberOfCharsOnThisLine ->
        if !howManyCharsLeftToCoverOnSubsequentLines > numberOfCharsOnThisLine then
          (howManyCharsLeftToCoverOnSubsequentLines :=
            !howManyCharsLeftToCoverOnSubsequentLines - numberOfCharsOnThisLine - 1;
          true)
        else false)
    in
    let howManyMoreRowsCoveredSinceStartRow =
      1 + BatList.length suddenlyFunctionalProgrammingOutOfNowhere
    in
    ((startRow, col1),
    (startRow + howManyMoreRowsCoveredSinceStartRow, !howManyCharsLeftToCoverOnSubsequentLines))

(* has the side-effect of reading the file *)
let extractFromFileMatch fileMatch: (fileInfo * Atom.Range.t * string) =
  Pcre.(
    match fileMatch with
    | [Delim _; Group (_, fileName); Group (_, lineNum); col1; col2; Text text] ->
      let cachedContent = BatList.of_enum (BatFile.lines_of fileName) in
      let (col1Raw, col2Raw) = match (col1, col2) with
        | (Group (_, c1), Group (_, c2)) -> (Some c1, Some c2)
        | _ -> (None, None)
      in
      (
        {filePath = fileName; cachedContent = cachedContent},
        (normalizeCompilerLineColsToRange
          ~fileLines:cachedContent
          ~lineRaw:lineNum
          ~col1Raw:col1Raw
          ~col2Raw:col2Raw
        ),
        (* important, otherwise leaves random blank lines that defies some of
        our regex logic, maybe *)
        BatString.trim text
      )
    | _ -> raise (invalid_arg "Couldn't extract error")
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

let printFullSplitResult = BatList.iteri (fun i x ->
  print_int i;
  print_endline "";
  Pcre.(
    match x with
    | Delim a -> print_endline @@ "Delim " ^ a
    | Group (_, a) -> print_endline @@ "Group " ^ a
    | Text a -> print_endline @@ "Text " ^ a
    | NoGroup -> print_endline @@ "NoGroup"
  )
)

let doThis (err): result =
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
      let files =
        BatString.trim err
        |> Pcre.full_split ~rex:fileR
        (* First few rows might be random output info *)
        |> BatList.drop_while (function Pcre.Text _ -> true | _ -> false)
        (* we match 6 items, so the whole list will always be a multiple of 6 *)
        |> splitInto ~chunckSize:6
        |> BatList.map extractFromFileMatch
      in
      let errorOrWarningR = Pcre.regexp
        ~flags:[Pcre.(`MULTILINE)]
        {|^(?:(?:Error)|(?:Warning) (\d+)): |}
      in
      let errorParsers = [
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
      (* TODO: better logic using these codes *)
      let warningParsers = [
        warning_UnusedVariable;
        warning_PatternNotExhaustive;
        warning_PatternUnused;
        warning_OptionalArgumentNotErased;
      ]
      in
      let filesAndErrorsAndWarnings: fileAndErrorsAndWarnings list =
      files |> BatList.map (fun (fileInfo, range, text) ->
        let errorsAndWarnings =
          Pcre.full_split ~rex:errorOrWarningR text
          (* we match 4 items, so the whole list will always be a multiple of 4 *)
          |> splitInto ~chunckSize:3
        in
        (* taking advantage of the distinct shape of error/warning to separate
        them *)
        let errors = errorsAndWarnings |> Pcre.(BatList.filter_map (function
          | [Delim _; NoGroup; Text text] -> Some text
          | _ -> None
        ))
        in
        let warnings = errorsAndWarnings |> Pcre.(BatList.filter_map (function
          | [Delim _; Group (_, code); Text text] ->
              Some (int_of_string code, text)
          | _ -> None
        ))
        in
        let errs =
          errors
          |> BatList.map (fun errorRaw ->
              let result =
                try
                  BatList.find_map (fun errorParser ->
                    try Some (errorParser errorRaw fileInfo range)
                    with _ -> None)
                  errorParsers
                with Not_found -> Error_CatchAll errorRaw
              in
              {
                range = range;
                parsedContent = result;
              }
          )
        in
        let warns =
          warnings
          |> BatList.map (fun (code, warningRaw) ->
            let result = {
              code = code;
              warningType =
                try
                  BatList.find_map (fun warningParser ->
                    try Some (warningParser code warningRaw fileInfo range)
                    with _ -> None)
                  warningParsers
                with Not_found -> Warning_CatchAll warningRaw
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

(* let () =
  Reporter.print @@ doThis syntaxErr;
  print_endline "=======1===========";
  Reporter.print @@ doThis badFileErr;
  print_endline "=======2===========";
  Reporter.print @@ doThis noErr;
  print_endline "=======3===========";
  Reporter.print @@ doThis badBuildOutput2;
  print_endline "=======4===========";
  Reporter.print @@ doThis badBuildOutput1;
  print_endline "=======5===========" *)

(* entry point, for convenience purposes for now. Theoretically the parser and
the reporters are decoupled *)
let () =
  try
    let err = BatPervasives.input_all stdin in
    Reporter.print @@ doThis err;
  with BatIO.No_more_input -> ()
