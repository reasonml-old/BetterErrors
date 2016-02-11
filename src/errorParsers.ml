open Types

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

(* need: if it's e.g. a module function, which part is not found? Module?
Function? *)
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

(* need: list of legal characters *)
let file_IllegalCharacter err _ _ =
  let characterR = {|Illegal character \((.+)\)|} in
  let character = get_match characterR err in
  File_IllegalCharacter {
    character = character;
  }


let parsers = [
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
