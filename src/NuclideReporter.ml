open Types
open Atom

let reporterName = "Merlin"

let makeFullPath fileInfo = (BatSys.getcwd ()) ^ "/" ^ fileInfo.path

let makeFileDiagnosticMessage typee errorMessage ?tip fileInfo range =
  let fullErrorMessage = errorMessage ^ (match tip with
    | None -> ""
    | Some h -> "\n" ^ h) in
  let open NuclideDiagnostic.Message in
    FileDiagnosticMessage {
      scope = `file;
      providerName = reporterName;
      typee = typee;
      filePath = makeFullPath fileInfo;
      text = Some fullErrorMessage;
      html = None;
      range = Some range;
      trace = None;
    }

let printAssumingErrorsAndWarnings l = l |> BatList.map (fun {fileInfo; errors; warnings} ->
  let newErrors = errors |> BatList.map (fun {range; parsedContent} -> match parsedContent with
    | Type_MismatchTypeArguments {typeConstructor; expectedCount; actualCount} ->
      let errorMessage = Printf.sprintf "This needs to be applied to %d argument(s), we found %d." expectedCount actualCount in
      makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage fileInfo range
    | Type_IncompatibleType {actual; expected} ->
      let errorMessage = Printf.sprintf "This is %s, wanted %s instead." actual expected in
      makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage fileInfo range
    | Type_NotAFunction {actual} ->
      let errorMessage = Printf.sprintf "This is %s. You seem to have called it as a function." actual in
      let tip = "Careful with the spaces and the parentheses, and whatever's in-between!" in
      makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage ~tip fileInfo range
    | Type_AppliedTooMany {functionType; expectedArgCount} ->
      let errorMessage = Printf.sprintf "This function has type %s.\nIt accepts only %d arguments. You gave more." functionType expectedArgCount in
      let tip = "Maybe you forgot a `;` somewhere?" in
      makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage ~tip fileInfo range
    | File_SyntaxError {offendingString; hint} ->
      let errorMessage = (match hint with
        | Some a -> "The syntax is wrong: " ^ a
        | None -> "The syntax is wrong.") in
      begin match offendingString with
        | ";" -> let tip = "Semicolon is an infix symbol used *between* expressions that return `unit` (aka \"nothing\")." in
          makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage ~tip fileInfo range
        | "else" -> let tip =
          "Did you happen to have put a semicolon on the line before else?" ^
          " Also, `then` accepts a single expression. If you've put many, wrap them in parentheses." in
          makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage ~tip fileInfo range
        | _ -> makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage fileInfo range
      end
    | File_IllegalCharacter {character} ->
      let errorMessage = Printf.sprintf "The character `%s` is illegal. EVERY CHARACTER THAT'S NOT AMERICAN IS ILLEGAL!" character in
      makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage fileInfo range
    | Type_UnboundTypeConstructor {namespacedConstructor; suggestion} ->
      let errorMessage = Printf.sprintf "The type constructor %s can't be found." namespacedConstructor in
      begin match suggestion with
        | None -> makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage fileInfo range
        | Some h -> makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage ~tip:(Printf.sprintf "Hint: did you mean `%s`?" h) fileInfo range
      end
    | Type_UnboundValue {unboundValue; suggestion} ->
      let errorMessage = begin match suggestion with
        | None -> Printf.sprintf "`%s` can't be found. Could it be a typo?" unboundValue
        | Some h -> Printf.sprintf "`%s` can't be found. Did you mean `%s`?" unboundValue h
      end in
      makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage fileInfo range
    | Type_UnboundRecordField {recordField; suggestion} ->
      let errorMessage = begin match suggestion with
        | None -> Printf.sprintf "Field `%s` can't be found in any record type." recordField
        | Some h -> Printf.sprintf "Field `%s` can't be found in any record type. Did you mean `%s`?" recordField h
      end in
      makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage fileInfo range
    | Type_UnboundModule {unboundModule} ->
      let errorMessage = Printf.sprintf "Module `%s` not found in included libraries." unboundModule in
      let pckName = BatString.lowercase unboundModule in
      let tip =
        "Hint: your build rules might be missing a link. If you're using: \n" ^
        " - Oasis: make sure you have `"^ pckName ^"` under `BuildDepends` in your _oasis file.\n" ^
        " - ocamlbuild: make sure you have `-pkgs "^ pckName ^"` in your build command.\n" ^
        " - ocamlc | ocamlopt: make sure you have `-I +"^ pckName ^"` in your build command before the source files.\n" ^
        " - ocamlfind: make sure you have `-package "^ pckName ^" -linkpkg` in your build command.\n" in
      makeFileDiagnosticMessage NuclideDiagnostic.Error errorMessage ~tip fileInfo range
    | _ -> makeFileDiagnosticMessage NuclideDiagnostic.Error "No error message for this type of error." fileInfo range
  ) in
  let newWarnings = warnings |> BatList.map (fun {range; parsedContent = {code; warningType}} -> match warningType with
    | Warning_CatchAll message ->
      let errorMessage = Printf.sprintf "Warning: %s" message in
      makeFileDiagnosticMessage NuclideDiagnostic.Warning errorMessage fileInfo range
    | Warning_PatternNotExhaustive {unmatched; warningCode} ->
      let errorMessage = Printf.sprintf "Warning %d: this match doesn't cover all possible values of the variant.\n" warningCode in
      let tip = (match unmatched with
        | [oneVariant] -> Printf.sprintf "The case `%s` is not matched" oneVariant
        | many ->
          "These cases are not matched:\n" ^
          (BatList.fold_left (fun acc x -> acc ^ "- `"^ x ^"`\n") "" many)) in
      makeFileDiagnosticMessage NuclideDiagnostic.Warning errorMessage ~tip fileInfo range
    | Warning_OptionalArgumentNotErased {warningCode; argumentName} ->
      let errorMessage = Printf.sprintf "Warning %d: %s is an optional argument at last position; calling the function by omitting %s might be confused with currying.\n" warningCode argumentName argumentName in
      let tip = "The rule: an optional argument is erased as soon as the 1st positional (i.e. neither labeled nor optional) argument defined after it is passed in." in
      makeFileDiagnosticMessage NuclideDiagnostic.Warning errorMessage ~tip fileInfo range
    | _ -> makeFileDiagnosticMessage NuclideDiagnostic.Warning "No message for this type of warning." fileInfo range
  ) in
  BatList.concat [newErrors; newWarnings]
)

let print (content: result) = match content with
  (* handle the special cases first *)
  | NoErrorNorWarning err -> None
  (* What to do in this case? *)
  | Unparsable err -> None
  | ErrorsAndWarnings err -> Some (printAssumingErrorsAndWarnings err)
