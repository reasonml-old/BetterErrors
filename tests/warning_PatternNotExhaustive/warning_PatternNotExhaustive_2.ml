type greetings =
  | Hello
  | Goodbye
  | Hola of string

let say a = match a with
| Hello -> ()
