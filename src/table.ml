type align =
  | Left
  | Right
  | Center

let pad ~align ~totalWidth content =
  let freeSpace = totalWidth - (BatString.length content) in
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

let column l i = BatList.map (fun row -> BatString.length @@ BatList.nth row i) l

let simple =          ("┌", "┬", "┐", "├", "┼", "┤", "└", "┴", "┘", "─", "│")
let double =          ("╔", "╦", "╗", "╠", "╬", "╣", "╚", "╩", "╝", "═", "║")
let dotted =          (" ", " ", " ", " ", " ", " ", " ", " ", " ", "┄", "┆")
let onlyVertical =    ("┌", "┬", "┐", "├", "┼", "┤", "└", "┴", "┘", "", "│")
let onlyHorizontal =  (" ", "─", " ", " ", "─", " ", " ", "─", " ", "─", " ")
let compact =         ("", "", "", "", "", "", "", "", "", "", "")
let onlyIner =        (" ", " ", " ", "├", "┼", "┤", " ", " ", " ", "─", "│")

let table ?(align=Left) ?(style=simple) ?(padding=1) lists =
  let (topLeft, topMid, topRight, midLeft, midMid, midRight, bottomLeft,
    bottomMid, bottomRight, hor, ver) = style in

  let anyRow = BatList.hd lists in
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
    |> BatList.interleave ~first:ver ~last:(ver ^ "\n") ver
    |> BatString.concat ""
  ) lists)
  |> BatList.interleave
    ~first: (drawHorizontalLine ~style:(topLeft, topMid, topRight, hor) ~row:anyRow ~maxes)
    ~last: (drawHorizontalLine ~style:(bottomLeft, bottomMid, bottomRight, hor) ~row: anyRow ~maxes)
    (drawHorizontalLine ~style:(midLeft, midMid, midRight, hor) ~row: anyRow ~maxes)
  |> BatString.concat ""

let () = print_endline @@ table [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:double ~align:Right [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:dotted ~align:Center [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:compact [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~padding:0 ~align:Center [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:onlyVertical [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table ~style:onlyHorizontal ~align:Right [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
(* let () = print_endline @@ table ~style:onlyIner [["1"; "213ad"; "3";]; ["4"; "5"; "6"]] *)
