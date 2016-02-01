open Types

let print msg = match msg with
  | Unparsable err -> print_endline "couldn't parse error, original:"; print_endline err
  | Type_MismatchTypeArguments err -> print_endline err.constructor
  | _ -> print_endline "huh"
