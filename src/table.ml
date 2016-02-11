type align =
  | Left
  | Right
  | Center

let ansiR = {|\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]|}
let getLength s = BatString.length (Pcre.replace ~pat:ansiR s)

let pad ~align ~totalWidth content =
  let freeSpace = totalWidth - (getLength content) in

  match align with
  | Left -> content ^ (BatString.make freeSpace ' ')
  | Right -> (BatString.make freeSpace ' ') ^ content
  | Center ->
    let leftSpace = freeSpace / 2 in
    (* if freeSpace is odd, give rightSpace one more space *)
    let rightSpace = freeSpace - leftSpace in
    (BatString.make leftSpace ' ') ^ content ^ (BatString.make rightSpace ' ')

let drawHorizontalLine ~style:(left, mid, right, horizontal) ~row ~maxes =
  if horizontal = "" then ""
  else
    BatList.map2 (fun cell maxWidth -> BatString.repeat horizontal maxWidth) row maxes
    |> BatList.interleave ~first:left ~last:(right ^ "\n") mid
    |> BatString.concat ""

let column l i = BatList.map (fun row -> getLength @@ BatList.at row i) l
(* -let onlyVertical =    ("┌", "┬", "┐", "├", "┼", "┤", "└", "┴", "┘", "", "│") *)
(* -let onlyHorizontal =  (" ", "─", " ", " ", "─", " ", " ", "─", " ", "─", " ")
-let compact =         ("", "", "", "", "", "", "", "", "", "", "")
-let onlyIner =        (" ", " ", " ", "├", "┼", "┤", " ", " ", " ", "─", "│") *)

type style = {
  top: string * string * string * string;
  middle: string * string * string * string;
  bottom: string * string * string * string;
  vertical: string * string * string;
}

let simple = {
  top = ("┌", "┬", "┐", "─");
  middle = ("├", "┼", "┤", "─");
  bottom = ("└", "┴", "┘", "─");
  vertical = ("│", "│", "│");
}
let double = {
  top = ("╔", "╦", "╗", "═");
  middle = ("╠", "╬", "╣", "═");
  bottom = ("╚", "╩", "╝", "═");
  vertical = ("║", "║", "║");
}
let dotted = {
  top = (" ", " ", " ", "┄");
  middle = (" ", " ", " ", "┄");
  bottom = (" ", " ", " ", "┄");
  vertical = ("┆", "┆", "┆");
}
let onlyVertical = {
  top = (" ", " ", " ", "");
  middle = ("│", "│", "│", "");
  bottom = (" ", " ", " ", "");
  vertical = ("│", "│", "│");
}
let onlyHorizontal = {
  top = (" ", "─", " ", "─");
  middle = (" ", "─", " ", "─");
  bottom = (" ", "─", " ", "─");
  vertical = (" ", " ", " ");
}
let compact = {
  top = ("", "", "", "");
  middle = ("", "", "", "");
  bottom = ("", "", "", "");
  vertical = ("", "", "");
}
let onlyInner = {
  top = (" ", " ", " ", " ");
  middle = (" ", "┼", " ", "─");
  bottom = (" ", " ", " ", " ");
  vertical = (" ", "│", " ");
}
let onlyOuter = {
  top = ("┌", "─", "┐", "─");
  middle = ("", "", "", "");
  bottom = ("└", "─", "┘", "─");
  vertical = ("│", " ", "│");
}
let testCode = {
  top = (" ", " ", " ", " ");
  middle = ("", "", "", "");
  bottom = (" ", " ", " ", " ");
  vertical = ("", "│", "");
}

let table ?(align=Left) ?(style=simple) ?(padding=1) lists =
  let {
    top = (tLeft, tMid, tRight, tBar);
    middle = (mLeft, mMid, mRight, mBar);
    bottom = (bLeft, bMid, bRight, bBar);
    vertical = (vLeft, vMid, vRight);
  } = style in
  let anyRow = BatList.hd lists in
  (* max columns width *)
  let maxes = BatList.init
    (BatList.length anyRow)
    (fun i -> padding * 2 + BatList.max (column lists i))
  in
  let paddingStr = BatString.make padding ' ' in
  (BatList.map (fun row ->
    BatList.map2 (fun cell maxWidth ->
      (* padding is added on top of totalWidth *)
      pad ~align ~totalWidth:maxWidth (paddingStr ^ cell ^ paddingStr)
    ) row maxes
    |> BatList.interleave ~first:vLeft ~last:(vRight ^ "\n") vMid
    |> BatString.concat ""
  ) lists)
  |> BatList.interleave
    ~first: (drawHorizontalLine ~style:(tLeft, tMid, tRight, tBar) ~row:anyRow ~maxes)
    ~last: (drawHorizontalLine ~style:(bLeft, bMid, bRight, bBar) ~row: anyRow ~maxes)
    (drawHorizontalLine ~style:(mLeft, mMid, mRight, mBar) ~row: anyRow ~maxes)
  |> BatString.concat ""

(* let () = print_endline @@ table [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:double ~align:Right [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:dotted ~align:Center [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:compact [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~padding:0 ~align:Center [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:onlyVertical [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:onlyHorizontal ~align:Right [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:onlyInner [["1"; "type bread ="]; ["2"; "  | Coconut of string"]; ["3"; "let morning = Coconut"]]
let () = print_endline @@ table ~style:onlyOuter [["1"; "type bread ="]; ["2"; "  | Coconut of string"]; ["3"; "let morning = Coconut"]]
let () = print_endline @@ table ~style:testCode [["1"; "type bread ="]; ["2"; "  | Coconut of string"]; ["3"; "let morning = Coconut"]] *)
