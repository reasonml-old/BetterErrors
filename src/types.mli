type message =
  | Unparsable of string
  | TypeError of string
  | FileError of string
  | BuildError of string
  | SyntaxError of string
  | Warning of string
