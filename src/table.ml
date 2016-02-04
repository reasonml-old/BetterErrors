type align =
  | Left
  | Right
  | Center

let pad ?(align=Left) content ~total =
  let freeSpace = total - (BatString.length content) in
  match align with
  | Left ->
    content ^ (BatString.make freeSpace ' ')
  | Right ->
    (BatString.make freeSpace ' ') ^ content
  | Center ->
    let leftSpace = freeSpace / 2 in
    (* if freeSpace is odd, give rightSpace one more space *)
    let rightSpace = freeSpace - leftSpace in
    (BatString.make leftSpace ' ') ^ content ^ (BatString.make rightSpace ' ')

let table l =
  let columnLength = BatList.length (BatList.hd l) in
  let maxes = BatList.init columnLength (fun i ->
    let column = BatList.map (fun row ->
       BatString.length @@ BatList.nth row i
    ) l in
    BatList.max column
  ) in

  print_string "+";
  for i = 0 to (BatList.length @@ BatList.hd l) - 1 do
    let maxWidth = BatList.nth maxes i in
    print_string @@ (BatString.make (maxWidth + 2) '-') ^ "+"
  done;
  print_endline "";

  BatList.iter (fun row ->
    print_string "|";
    BatList.iter2 (fun cell maxWidth ->
      print_string @@ (pad ~align:Left cell ~total:(maxWidth + 2)) ^ "|"
    ) row maxes;

    print_string "\n+";
    BatList.iter2 (fun cell maxWidth ->
      print_string @@ (BatString.make (maxWidth + 2) '-') ^ "+"
    ) row maxes;
    print_endline ""
  ) l


let () = table [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]

(*
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
*)
