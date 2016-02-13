open Types
open Atom

let diagnosticMessage typee content filePath range =
  let open NuclideDiagnostic.Message in
  (* no project wide error/warning for now? *)
  FileDiagnosticMessage {
    scope = `file;
    providerName = "Merlin";
    typee = typee;
    (* absolute path. Is this right? *)
    filePath = Filename.concat (BatSys.getcwd ()) filePath;
    text = Some content;
    html = None;
    range = Some range;
    trace = None;
  }

let toNuclideList errorsAndWarnings =
  BatList.map2 (fun decryptedContent original ->
    let open NuclideDiagnostic in
    match original with
    | Types.Error {filePath; range} ->
      diagnosticMessage
        Error
        decryptedContent
        filePath
        range
    | Types.Warning {filePath; range} ->
      diagnosticMessage
        Warning
        decryptedContent
        filePath
        range
  )
  (TerminalReporter.decryptAssumingErrorsAndWarnings errorsAndWarnings)
  errorsAndWarnings

(* FOR JORDAN. This is similar to Types.results and the rest *)
type nuclideResult =
  | NoErrorNorWarning of string
  | Unparsable of string
  | ErrorsAndWarnings of NuclideDiagnostic.Message.t list

let convert (content: result): nuclideResult =
  match content with
  | Types.NoErrorNorWarning content -> NoErrorNorWarning content
  | Types.Unparsable content -> Unparsable content
  | Types.ErrorsAndWarnings errorsAndWarnings ->
    ErrorsAndWarnings (toNuclideList errorsAndWarnings)
