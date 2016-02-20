type align =
  | Left
  | Right
  | Center

let ansiR = {|\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]|}
let stripAnsi s = Pcre.replace ~pat:ansiR s

type style =
  | Compact
  | Normal
  | Bold
  | Double

(* style overriding precedence: Compact < Normal < Bold <> Double *)
(* bold and double can't mix side-by-side. Style will be resolved arbitrarily
(but deterministically) *)
let dominantStyle a b =
  match (a, b) with
  | (Compact, x) -> x
  | (Normal, Compact) -> Normal
  | (Normal, x) -> x
  | (Bold, Compact) -> Bold
  | (Bold, Normal) -> Bold
  | (Bold, x) -> x
  | (Double, _) -> Double

(*
        0	1	2	3	4	5	6	7	8	9	A	B	C	D	E	F
U+250x	─	━	│	┃	┄	┅	┆	┇	┈	┉	┊	┋	┌	┍	┎	┏
U+251x	┐	┑	┒	┓	└	┕	┖	┗	┘	┙	┚	┛	├	┝	┞	┟
U+252x	┠	┡	┢	┣	┤	┥	┦	┧	┨	┩	┪	┫	┬	┭	┮	┯
U+253x	┰	┱	┲	┳	┴	┵	┶	┷	┸	┹	┺	┻	┼	┽	┾	┿
U+254x	╀	╁	╂	╃	╄	╅	╆	╇	╈	╉	╊	╋	╌	╍	╎	╏
U+255x	═	║	╒	╓	╔	╕	╖	╗	╘	╙	╚	╛	╜	╝	╞	╟
U+256x	╠	╡	╢	╣	╤	╥	╦	╧	╨	╩	╪	╫	╬	╭	╮	╯
U+257x	╰	╱	╲	╳	╴	╵	╶	╷	╸	╹	╺	╻	╼	╽	╾	╿
 *)

(* I need my ocaml 4.03 inline record please *)
type topLeft = {h: style; v: style}
type topMiddle = {hLeft: style; hRight: style; v: style}
type topRight = {h: style; v: style}
type middleLeft = {vTop: style; vBottom: style; h: style}
type middleMiddle = {vTop: style; vBottom: style; hLeft: style; hRight: style}
type middleRight = {vTop: style; vBottom: style; h: style}
type bottomLeft = {h: style; v: style}
type bottomMiddle = {hLeft: style; hRight: style; v: style}
type bottomRight = {h: style; v: style}

type joint =
  | TopLeft of topLeft
  | TopMiddle of topMiddle
  | TopRight of topRight
  | MiddleLeft of middleLeft
  | MiddleMiddle of middleMiddle
  | MiddleRight of middleRight
  | BottomLeft of bottomLeft
  | BottomMiddle of bottomMiddle
  | BottomRight of bottomRight
  | Horizontal of style
  | Vertical of style

type position =
  | TopLeft
  | TopMiddle
  | TopRight
  | MiddleLeft
  | MiddleMiddle
  | MiddleRight
  | BottomLeft
  | BottomMiddle
  | BottomRight
  | Horizontal
  | Vertical

type specialConfig = (int * style) list

let findValue key assocList =
  try Some (BatList.find (fun (k) -> k = key) assocList)
  with Not_found -> None

type cellPosition =
  | TopLeft
  | TopMiddle
  | TopRight
  | MiddleLeft
  | MiddleRight
  | BottomLeft
  | BottomMiddle
  | BottomRight

type kek = {
  topRight: style;
  bottomLeft: style;
  bottomRight: style;
}

type extraStyle =
  | FirstRow of style
  | LastRow of style
  | FirstColumn of style
  | LastColumn of style
  | Row of int * style
  | Column of int * style
  | Cell of int * int * style

let allCompact = BatList.for_all (function Compact -> true | _ -> false)
let column table index = BatList.map (fun row -> BatList.at row index) table
let interleaveF ~first ~last ~middleF list :string list =
  let result = ref [] in
  for i = (BatList.length list) - 2 to 0 do
    result := (middleF (BatList.at list i) (BatList.at list (i + 1)) i (i + 1)) :: !result
  done;
  [first] @ !result @ [last]

type 'a muahaha = {
  topLeft: 'a option;
  topMiddle: 'a option;
  topRight: 'a option;
  middleLeft: 'a option;
  middleRight: 'a option;
  bottomLeft: 'a option;
  bottomMiddle: 'a option;
  bottomRight: 'a option;
}

let pad ~align ~totalWidth content =
  let freeSpace = totalWidth - (BatString.length (stripAnsi content)) in
  match align with
  | Left -> content ^ (BatString.make freeSpace ' ')
  | Right -> (BatString.make freeSpace ' ') ^ content
  | Center ->
    let leftSpace = freeSpace / 2 in
    (* if freeSpace is odd, give rightSpace one more space *)
    let rightSpace = freeSpace - leftSpace in
    (BatString.make leftSpace ' ') ^ content ^ (BatString.make rightSpace ' ')

let at2 listList i j = BatList.at (BatList.at listList i) j

let powerMap2D f grid =
  let at2Maybe listList i j =
    try Some (BatList.at (BatList.at listList i) j)
    with Invalid_argument _ -> None
  in
  BatList.mapi (fun i row ->
    BatList.mapi (fun j cell ->
      f cell {
        topLeft = at2Maybe grid (i - 1) (j - 1);
        topMiddle = at2Maybe grid (i - 1) j;
        topRight = at2Maybe grid (i - 1) (j + 1);
        middleLeft = at2Maybe grid i (j - 1);
        middleRight = at2Maybe grid i (j + 1);
        bottomLeft = at2Maybe grid (i + 1) (j - 1);
        bottomMiddle = at2Maybe grid (i + 1) j;
        bottomRight = at2Maybe grid (i + 1) (j + 1);
      }
    ) row
  ) grid

let table2
?(style=Normal)
?(extraStyles)
?(align=Left)
?(padding=1)
?(flip=false)
(data: string list list) =
  let tableWidth = BatList.length (BatList.at data 0) in
  let tableHeight = BatList.length data in

  let styles = powerMap2D (fun cell _ -> style) data in
  let styles = BatList.fold_left (fun styles' extraStyle ->
    match extraStyle with
    | FirstRow s ->
      BatList.modify_at 0 (fun _ -> BatList.make tableWidth s) styles'
    | LastRow s ->
      BatList.modify_at (tableHeight - 1) (fun _ -> BatList.make tableWidth s) styles'
    | FirstColumn s ->
      BatList.map (fun row ->
        BatList.modify_at 0 (fun _ -> s) row
      ) styles'
    | LastColumn s ->
      BatList.map (fun row ->
        BatList.modify_at (tableWidth - 1) (fun _ -> s) row
      ) styles'
    | Row (r, s) ->
      BatList.modify_at r (fun _ -> BatList.make tableWidth s) styles'
    | Column (c, s) ->
      BatList.map (fun row ->
        BatList.modify_at c (fun _ -> s) row
      ) styles'
    | Cell (r, c, s) ->
      BatList.modify_at r (fun r2 ->
        BatList.modify_at c (fun _ -> s) r2
      ) styles'
  ) styles (BatPervasives.(|?) extraStyles [])
  in
  let anyRow = BatList.hd data in
  let maxColumnsWidth = BatList.init
    (BatList.length anyRow)
    (fun i -> padding * 2 + BatList.max (BatList.map BatString.length (column data i)))
  in

  let paddingStr = BatString.make padding ' ' in
  (* first horizontal line *)
  let result = if (allCompact (BatList.at styles 0)) then ref ""
  else
    let upperLeftJoint = match at2 styles 0 0 with
    | Compact -> " "
    | Normal -> "┌"
    | Bold -> "┏"
    | Double -> "╔"
    in
    let firstRow = ref "" in
    for j = 0 to tableWidth - 1 do
      let maxColumnWidth = BatList.at maxColumnsWidth j in
      let char = match at2 styles 0 j with
      | Compact -> " "
      | Normal -> "─"
      | Bold -> "━"
      | Double -> "═"
      in
      firstRow := !firstRow ^ (BatString.repeat char maxColumnWidth);
      if j = tableWidth - 1 then
        let upperRightJoint = match at2 styles 0 (tableWidth - 1) with
        | Compact -> " "
        | Normal -> "┐"
        | Bold -> "┓"
        | Double -> "╗"
        in
        firstRow := !firstRow ^ upperRightJoint
      else
        let upperMiddleJoint =
        match (at2 styles 0 j, at2 styles 0 (j + 1)) with
        | (Compact, Compact) ->  " "
        | (Compact, Normal) -> "┌"
        | (Compact, Bold) -> "┏"
        | (Compact, Double) -> "╔"

        | (Normal, Compact) -> "┐"
        | (Normal, Normal) -> "┬"
        | (Normal, Bold) -> "┲"
        | (Normal, Double) -> "╥" (* no good one *)

        | (Bold, Compact) -> "┓"
        | (Bold, Normal) -> "┱"
        | (Bold, Bold) -> "┳"
        | (Bold, Double) -> "┳" (* no good one *)

        | (Double, Compact) -> "╗"
        | (Double, Normal) -> "╥" (* no good one *)
        | (Double, Bold) -> "╥" (* no good one *)
        | (Double, Double) -> "╦"
        in
        firstRow := !firstRow ^ upperMiddleJoint
    done;
    ref (upperLeftJoint ^ !firstRow ^ "\n")
  in

  (* middle (main) section *)
  for i = 0 to tableHeight - 1 do
    (* each row *)
    let leftMostVerticalBar = match at2 styles i 0 with
    | Compact -> if allCompact (column styles 0) then "" else " "
    | Normal -> "│"
    | Bold -> "┃"
    | Double -> "║"
    in
    let rowText = ref leftMostVerticalBar in

    let dontDrawHorizontalBar = i < tableHeight - 1
      && allCompact (BatList.at styles i)
      && allCompact (BatList.at styles (i + 1))
    in
    let leftMostVerticalBarH = if i = tableHeight - 1 || dontDrawHorizontalBar then ""
    else
      match (at2 styles i 0, at2 styles (i + 1) 0) with
      | (Compact, Compact) ->  " " (* already know we need to draw the horizontal bar *)
      | (Compact, Normal) -> "┌"
      | (Compact, Bold) -> "┏"
      | (Compact, Double) -> "╔"

      | (Normal, Compact) -> "└"
      | (Normal, Normal) -> "├"
      | (Normal, Bold) -> "┢"
      | (Normal, Double) -> "╞" (* no good one *)

      | (Bold, Compact) -> "┗"
      | (Bold, Normal) -> "┞"
      | (Bold, Bold) -> "┣"
      | (Bold, Double) -> "┣" (* no good one *)

      | (Double, Compact) -> "╚"
      | (Double, Normal) -> "╞" (* no good one *)
      | (Double, Bold) -> "╞" (* no good one *)
      | (Double, Double) -> "╠"
    in
    let horizontalBarRow = ref leftMostVerticalBarH in
    for j = 0 to tableWidth - 1 do
      (* each cell *)
      let maxColumnWidth = BatList.at maxColumnsWidth j in
      let cellText = at2 data i j in
      let cellText' =
        pad ~align ~totalWidth:maxColumnWidth (paddingStr ^ cellText ^ paddingStr)
      in
      let dominant = if j = tableWidth - 1 then
        at2 styles i j
      else
        dominantStyle (at2 styles i j) (at2 styles i (j + 1))
      in
      let barOnTheRight = match dominant with
      | Compact -> if allCompact (column styles j) then "" else " "
      | Normal -> "│"
      | Bold -> "┃"
      | Double -> "║"
      in
      rowText := !rowText ^ cellText' ^ barOnTheRight;

      let barH = if i = tableHeight - 1 || dontDrawHorizontalBar then ""
      else
        let char = match dominantStyle (at2 styles i j) (at2 styles (i + 1) j) with
        | Compact -> " " (* already know we need to draw the horizontal bar *)
        | Normal -> "─"
        | Bold -> "━"
        | Double -> "═"
        in
        BatString.repeat char maxColumnWidth
      in
      let barOnTheRightH = if i = tableHeight - 1 then ""
      else
        if j = tableWidth - 1 then
          match (at2 styles i j, at2 styles (i + 1) j) with
          | (Compact, Compact) -> if allCompact (column styles j) then "" else " "
          | (Compact, Normal) -> "┐"
          | (Compact, Bold) -> "┓"
          | (Compact, Double) -> "╗"

          | (Normal, Compact) -> "┘"
          | (Normal, Normal) -> "┤"
          | (Normal, Bold) -> "┪"
          | (Normal, Double) -> "╡" (* no good one *)

          | (Bold, Compact) -> "┛"
          | (Bold, Normal) -> "┩"
          | (Bold, Bold) -> "┫"
          | (Bold, Double) -> "┫" (* no good one *)

          | (Double, Compact) -> "╝"
          | (Double, Normal) -> "╡" (* no good one *)
          | (Double, Bold) -> "╡" (* no good one *)
          | (Double, Double) -> "╣"
        else
          (* clockwise, starting from top left *)
          match (at2 styles i j, at2 styles i (j + 1), at2 styles (i + 1) (j + 1), at2 styles (i + 1) j) with
          | _ -> "┼"
      in
      if not dontDrawHorizontalBar then
        horizontalBarRow := !horizontalBarRow ^ barH ^ barOnTheRightH
    done;
    result := !result ^ !rowText ^ (
      if i = tableHeight - 1 && allCompact (BatList.at styles (tableHeight - 1)) then ""
      else if dontDrawHorizontalBar then "\n"
      else if i = tableHeight - 1 then "\n" ^ !horizontalBarRow
      else "\n" ^ !horizontalBarRow ^ "\n"
    )
  done;

  (* last horizontal line *)
  result := !result ^
  if (allCompact (BatList.at styles (tableHeight - 1))) then ""
  else (
    let lowerLeftJoint = match at2 styles (tableHeight - 1) 0 with
    | Compact -> " "
    | Normal -> "└"
    | Bold -> "┗"
    | Double -> "╚"
    in
    let lastRow = ref "" in
    for j = 0 to tableWidth - 1 do
      let maxColumnWidth = BatList.at maxColumnsWidth j in
      let char = match at2 styles (tableHeight - 1) j with
      | Compact -> " "
      | Normal -> "─"
      | Bold -> "━"
      | Double -> "═"
      in
      lastRow := !lastRow ^ (BatString.repeat char maxColumnWidth);
      if j = tableWidth - 1 then
        let lowerRightJoint = match at2 styles (tableHeight - 1) (tableWidth - 1) with
        | Compact -> " "
        | Normal -> "┘"
        | Bold -> "┛"
        | Double -> "╝"
        in
        lastRow := !lastRow ^ lowerRightJoint
      else
        let lowerMiddleJoint =
        match (at2 styles (tableHeight - 1) j, at2 styles (tableHeight - 1) (j + 1)) with
        | (Compact, Compact) ->  " "
        | (Compact, Normal) -> "└"
        | (Compact, Bold) -> "┗"
        | (Compact, Double) -> "╚"

        | (Normal, Compact) -> "┘"
        | (Normal, Normal) -> "┴"
        | (Normal, Bold) -> "┺"
        | (Normal, Double) -> "╨" (* no good one *)

        | (Bold, Compact) -> "┛"
        | (Bold, Normal) -> "┹"
        | (Bold, Bold) -> "┻"
        | (Bold, Double) -> "┻" (* no good one *)

        | (Double, Compact) -> "╝"
        | (Double, Normal) -> "╨" (* no good one *)
        | (Double, Bold) -> "╨" (* no good one *)
        | (Double, Double) -> "╩"
        in
        lastRow := !lastRow ^ lowerMiddleJoint
    done;
    lowerLeftJoint ^ !lastRow);
  !result


let () = print_endline @@ table2 [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table2 ~style:Double ~align:Right [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table2 ~style:Compact ~align:Center [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table2 ~style:Compact ~align:Center ~padding:0 [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table2 ~style:Bold [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table2 ~align:Center ~extraStyles:[FirstRow Bold; FirstColumn Double] [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table2 ~align:Center ~extraStyles:[FirstColumn Double; FirstRow Bold] [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table2 ~align:Center ~extraStyles:[FirstColumn Double] [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline @@ table2 ~align:Center ~extraStyles:[FirstRow Bold] [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]

let () = assert ("\n" ^ table2 [["1"; "213ad"; "3";]; ["4"; "5"; "6"]] = {|
┌───┬───────┬───┐
│ 1 │ 213ad │ 3 │
├───┼───────┼───┤
│ 4 │ 5     │ 6 │
└───┴───────┴───┘|})
(* let () = assert ("\n" ^ table2 ~style:Double ~align:Right [["1"; "213ad"; "3";]; ["4"; "5"; "6"]] = {|
╔═══╦═══════╦═══╗
║ 1 ║ 213ad ║ 3 ║
╠═══╬═══════╬═══╣
║ 4 ║     5 ║ 6 ║
╚═══╩═══════╩═══╝|}) *)
let () = assert (table2 ~style:Compact ~align:Center [["1"; "213ad"; "3";]; ["4"; "5"; "6"]] =
" 1  213ad  3 " ^ "\n" ^
" 4    5    6 ")
let () = assert (table2 ~style:Compact ~align:Center ~padding:0 [["1"; "213ad"; "3";]; ["4"; "5"; "6"]] =
"1213ad3" ^ "\n" ^
"4  5  6")


(* let () = print_endline @@ table ~style:onlyVertical [["1"; "213ad"; "3";]; ["4"; "5"; "6"]] *)
(* let () = print_endline @@ table ~style:onlyHorizontal ~align:Right [["1"; "213ad"; "3";]; ["4"; "5"; "6"]] *)
(* let () = print_endline @@ table ~style:onlyInner [["1"; "type bread ="]; ["2"; "  | Coconut of string"]; ["3"; "let morning = Coconut"]] *)
(* let () = print_endline @@ table ~style:onlyOuter [["1"; "type bread ="]; ["2"; "  | Coconut of string"]; ["3"; "let morning = Coconut"]] *)
(* let () = print_endline @@ table ~style:testCode [["1"; "type bread ="]; ["2"; "  | Coconut of string"]; ["3"; "let morning = Coconut"]] *)
