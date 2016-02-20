type modifiersType = {
  reset: int * int;
  bold: int * int;
  dim: int * int;
  italic: int * int;
  underline: int * int;
  inverse: int * int;
  hidden: int * int;
  strikethrough: int * int;
}
let modifiers = {
  reset = (0, 0);
  (* 21 isn't widely supported and 22 does the same thing *)
  bold = (1, 22);
  dim = (2, 22);
  italic = (3, 23);
  underline = (4, 24);
  inverse = (7, 27);
  hidden = (8, 28);
  strikethrough = (9, 29);
}
type colorType = {
  black: int * int;
  red: int * int;
  green: int * int;
  yellow: int * int;
  blue: int * int;
  magenta: int * int;
  cyan: int * int;
  white: int * int;
  gray: int * int;
  grey: int * int;
}
let color = {
  black = (30, 39);
  red = (31, 39);
  green = (32, 39);
  yellow = (33, 39);
  blue = (34, 39);
  magenta = (35, 39);
  cyan = (36, 39);
  white = (37, 39);
  gray = (90, 39);
  grey = (90, 39);
}
type bgColor = {
  bgBlack: int * int;
  bgRed: int * int;
  bgGreen: int * int;
  bgYellow: int * int;
  bgBlue: int * int;
  bgMagenta: int * int;
  bgCyan: int * int;
  bgWhite: int * int;
}
let bgColor = {
  bgBlack = (40, 49);
  bgRed = (41, 49);
  bgGreen = (42, 49);
  bgYellow = (43, 49);
  bgBlue = (44, 49);
  bgMagenta = (45, 49);
  bgCyan = (46, 49);
  bgWhite = (47, 49);
}
(* let paint color string =

let () = print_endline @@ paint Red "hello"
let () = print_endline @@ paints [Red;] "hello"
 *)
 let () = print_endline @@
   {|\u001b[|} ^ (string_of_int (BatTuple.Tuple2.first color.red)) ^ "m" ^ "asd" ^
   {|\u001b[|} ^ (string_of_int (BatTuple.Tuple2.second color.red)) ^ "m"
