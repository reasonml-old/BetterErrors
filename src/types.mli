(* type termKind = Expression | Pattern *)
type conflictPair = {inferred: string list; expected: string list}
type fileInfo = {
  name: string;
  line: int;
  cols: int * int;
}

type mismatchTypeArguments = {constructor: string; expectedCount: int; observedCount: int}
type unboundValue = {constructor: string; expectedCount: int; observedCount: int}
type signatureMismatch = {constructor: string; expectedCount: int; observedCount: int}
type signatureItemMissing = {constructor: string; expectedCount: int; observedCount: int}
type unboundModule = {constructor: string; expectedCount: int; observedCount: int}
type unboundRecordField = {constructor: string; expectedCount: int; observedCount: int}
type unboundConstructor = {constructor: string; expectedCount: int; observedCount: int}
type unboundTypeConstructor = {constructor: string; expectedCount: int; observedCount: int}
type appliedTooMany = {constructor: string; expectedCount: int; observedCount: int}
type recordFieldNotInExpression = {constructor: string; expectedCount: int; observedCount: int}
type recordFieldError = {constructor: string; expectedCount: int; observedCount: int}
type syntaxError = {constructor: string; expectedCount: int; observedCount: int}
type inconsistentAssumptions = {constructor: string; expectedCount: int; observedCount: int}
type catchAll = {constructor: string; expectedCount: int; observedCount: int}
type fieldNotBelong = {constructor: string; expectedCount: int; observedCount: int}

type incompatibleType = {
  (* termKind: termKind; *)
  fileInfo: fileInfo;
  inferred: string;
  expected: string;
  (* inferredEquivalentTypes: string list;
  expectedEquivalentTypes: string list; *)
  (* conflicts: conflictPair list; *)
  (* existentialMessage: string option; *)
}
type notAFunction = {
  fileInfo: fileInfo;
  inferred: string;
}

type message =
  | Type_MismatchTypeArguments of mismatchTypeArguments
  | Type_UnboundValue of string
  | Type_SignatureMismatch of string
  | Type_SignatureItemMissing of string
  | Type_UnboundModule of string
  | Type_UnboundRecordField of string
  | Type_UnboundConstructor of string
  | Type_UnboundTypeConstructor of string
  | Type_AppliedTooMany of string
  | Type_RecordFieldNotInExpression of string
  | Type_RecordFieldError of string
  | Type_FieldNotBelong of string

  | Type_IncompatibleType of incompatibleType
  | Type_NotAFunction of notAFunction

  | File_SyntaxError of string
  | Build_InconsistentAssumptions of string
  | Warning_CatchAll of string

  (* not in jordan's stuff *)
  | Warning_UnusedVariable of string
  | Unparsable of string
  (* | General_CatchAll of string *)
  (* | Project_Unknown of string *)
