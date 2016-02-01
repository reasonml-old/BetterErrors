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
type incompatibleType = {constructor: string; expectedCount: int; observedCount: int}
type notAFunction = {constructor: string; expectedCount: int; observedCount: int}

type message =
  | Unparsable of string
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
  | File_SyntaxError of string
  | Build_InconsistentAssumptions of string
  | Warning_CatchAll of string
  | Type_FieldNotBelong of string
  | Type_IncompatibleType of string
  | Type_NotAFunction of string

  | Warning_UnusedVariable of string
  (* | General_CatchAll of string *)
  (* | Project_Unknown of string *)
