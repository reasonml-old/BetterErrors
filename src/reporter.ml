open Types

let print msg = match msg with
  | Unparsable err -> print_endline "couldn't parse error, original:"; print_endline err
  | TypeError err -> print_endline err
  | _ -> print_endline "huh"
