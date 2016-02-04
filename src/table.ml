type align =
  | Left
  | Right
  | Center

let pad ~align ~totalWidth content =
  let freeSpace = totalWidth - (BatString.length content) in
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

let print_horizontal_border ~joint ~hBorder ~row ~maxes =
  print_string joint;
  BatList.iter2 (fun cell maxWidth ->
    print_string @@ (BatString.repeat hBorder maxWidth) ^ joint
  ) row maxes;
  print_endline ""

let table ?(align=Left) ?(hBorder=Some "-") ?(vBorder=Some "|") ?(padding=1) l =
  let columnLength = BatList.length (BatList.hd l) in
  let maxes = BatList.init columnLength (fun i ->
    let column = BatList.map (fun row ->
       BatString.length @@ BatList.nth row i
    ) l in
    padding * 2 + BatList.max column
  ) in

  let vBorder2 = BatPervasives.(|?) vBorder "" in
  let joint = match vBorder with
  | None -> ""
  | Some _ -> "+"
  in
  (* top border *)
  (match hBorder with
  | None -> ()
  | Some ch -> print_horizontal_border ~joint ~hBorder:ch ~row:(BatList.hd l) ~maxes);

  let paddingStr = BatString.make padding ' ' in
  BatList.iter (fun row ->
    print_string vBorder2;
    BatList.iter2 (fun cell maxWidth ->
      print_string @@
        (* padding is added on top of totalWidth *)
        (pad ~align ~totalWidth:maxWidth (paddingStr ^ cell ^ paddingStr)) ^ vBorder2
    ) row maxes;

    print_endline "";
    (match hBorder with
    | None -> ()
    | Some ch -> print_horizontal_border ~joint ~hBorder:ch ~row ~maxes)
  ) l

let () = table [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline "\n"
let () = table ~hBorder:(Some "=") ~vBorder:(Some "║") ~align:Right [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline "\n"
let () = table ~padding:0 ~align:Center [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline "\n"
let () = table ~hBorder:None ~align:Center [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline "\n"
let () = table ~vBorder:None [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
let () = print_endline "\n"
let () = table ~vBorder:None ~hBorder:None ~align:Right [["1"; "213ad"; "3";]; ["4"; "5"; "6"]]
