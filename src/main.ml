open Types

let filenameR = {|File "(.+)"|}
let lineR = {|File .+, line (\d+)|}
let chars1R = {|File .+, characters (\d+)|}
let chars2R = {|File .+, characters .+-(\d+)|}

(* helper for getting the first (presumably only) match in a string *)
let get_match pat str = Pcre.get_substring (Pcre.exec ~pat:pat str) 1
(* helpers for turning Not_found exception into an optional *)
let exec ~rex str =
  try Some (Pcre.exec ~rex str) with Not_found -> None

let split sep str = Pcre.split ~pat:sep str

(* Perhaps this should be applied everywhere we see a type. *)
(* let splitEquivalentTypes typeStr =
   List.filter (fun typ -> (String.trim typ) != "") (split "=" typeStr)

let getConflictPairs incompatText =
  if !(Pcre.pmatch ~pat:"is not compatible with type"  incompatText) then None
  else
    let splitByIsNotCompatibleWith = split "is not compatible with type" incompatText in
    let conflicts = [] in
    let splitByType = List.map (fun text ->
      let splitByType = split {|\bType\s|} text in

    )
    let splitByType = splitByIsNotCompatibleWith.map(function(text) {
      let splitByType = text.split(/\bType\s/);
      splitByType && splitByType.forEach(function(byType){
        byType && byType.trim() && conflicts.push(byType.trim());
      });
    });
    if (conflicts.length % 2 !== 0) {
      throw new Error("Conflicts don't appear in pairs");
    }
    let conflictPairs = [];
    for (let i = 0; i < conflicts.length; i+=2) {
      conflictPairs.push({
        inferred: splitEquivalentTypes(conflicts[i]),
        expected: splitEquivalentTypes(conflicts[i+1])
      });
    }
    return conflictPairs;
 *)

(* agnostic extractors, turning err string into proper data structures *)
let type_MismatchTypeArguments err errLines =
  (* let filename = get_match filenameR err in
  let line = get_match lineR err in
  let chars1 = get_match chars1R err in
  let chars2 = get_match chars2R err in *)
  (* let regex = {|Error: The type constructor\s*([\w\.]*)\s*expects[\s]*(\d+)\s*argument\(s\),\s*but is here applied to\s*(\d+)\s*argument\(s\)|} in *)
    Type_MismatchTypeArguments {
      constructor = "asd";
      expectedCount = 1;
      observedCount = 2;
    }
    (* Type_MismatchTypeArguments (
      filename ^ " " ^ line ^ ":" ^ chars1 ^ "-" ^ chars2 ^ "\n" ^
      (List.nth fileLines ((int_of_string line) - 1)) ^ "\n" ^
      (String.make (int_of_string chars1) ' ') ^
      (String.make ((int_of_string chars2) - (int_of_string chars1)) '^') ^ "\n" ^
      String.concat "\n" (List.tl errLines)
    ) *)

let type_UnboundValue err errLines = raise Not_found
let type_SignatureMismatch err errLines = raise Not_found
let type_SignatureItemMissing err errLines = raise Not_found
let type_UnboundModule err errLines = raise Not_found
let type_UnboundRecordField err errLines = raise Not_found
let type_UnboundConstructor err errLines = raise Not_found
let type_UnboundTypeConstructor err errLines = raise Not_found

(* need: number of arguments actually applied to it, and what they are *)
(* need: number of args the function asks, and what types they are *)
let type_AppliedTooMany err errLines =
  let filename = get_match filenameR err in
  let line = int_of_string (get_match lineR err) in
  let chars1 = int_of_string (get_match chars1R err) in
  let chars2 = int_of_string (get_match chars2R err) in

  let functionTypeR = {|This function has type (.+)\n +It is applied to too many arguments; maybe you forgot a `;'.|} in
  let functionType = get_match functionTypeR err in
  (* the func type 'a -> (int -> 'b) -> string has 2 arguments *)
  (* strip out false positive -> from nested function types passed as param *)
  let nestedFunctionTypeR = {|\(.+\)|} in
  let cleaned = Pcre.replace ~pat:nestedFunctionTypeR ~templ:"bla" functionType in
  let expectedArgCount = List.length (split "->" cleaned) - 1 in
    Type_AppliedTooMany {
      fileInfo = {
        name = filename;
        line = line;
        cols = (chars1, chars2);
      };
      functionType = functionType;
      expectedArgCount = expectedArgCount;
    }

let type_RecordFieldNotInExpression err errLines = raise Not_found
let type_RecordFieldError err errLines = raise Not_found
let type_FieldNotBelong err errLines = raise Not_found

(* need: where the original expected comes from  *)
let type_IncompatibleType err errLines =
  let filename = get_match filenameR err in
  let line = int_of_string (get_match lineR err) in
  let chars1 = int_of_string (get_match chars1R err) in
  let chars2 = int_of_string (get_match chars2R err) in

  let actualR = {|This expression has type (.+) but an expression was expected of type|} in
  let expectedR = {|This expression has type .+ but an expression was expected of type\n +(.+)|} in
  let actual = get_match actualR err in
  let expected = get_match expectedR err in
    Type_IncompatibleType {
      fileInfo = {
        name = filename;
        line = line;
        cols = (chars1, chars2);
      };
      actual = actual;
      expected = expected;
    }

let type_NotAFunction err errLines =
  let filename = get_match filenameR err in
  let line = int_of_string (get_match lineR err) in
  let chars1 = int_of_string (get_match chars1R err) in
  let chars2 = int_of_string (get_match chars2R err) in

  let actualR = {|This expression has type (.+)\n +This is not a function; it cannot be applied.|} in
  let actual = get_match actualR err in
    Type_NotAFunction {
      fileInfo = {
        name = filename;
        line = line;
        cols = (chars1, chars2);
      };
      actual = actual;
    }

let file_SyntaxError err errLines = raise Not_found
let build_InconsistentAssumptions err errLines = raise Not_found
let warning_CatchAll err errLines = raise Not_found
let warning_UnusedVariable err errLines = raise Not_found

let unparsable err errLines = Unparsable err

let parsers = [
  (* type_MismatchTypeArguments; *)
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
  warning_CatchAll;
  warning_UnusedVariable;

  (* this should stay at last position. It's a catch-all that doesn't throw *)
  unparsable;
]

(* ------------------------ *)

let () =
  (* read stdin til end *)
  try
    let err = BatIO.nread BatIO.stdin 99999 in
    let errLines = split "\n" err in
      let matched = Batteries.List.find_map (fun f ->
        try Some (f err errLines) with Not_found -> None
      ) parsers in Reporter.print matched
  with Batteries.IO.No_more_input -> print_endline "All good!"
