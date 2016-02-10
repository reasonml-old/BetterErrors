module Range = struct
  (* (startRow, startColumn), (endRow, endColumn) *)
  type t = (int * int) * (int * int)
  let emptyRange = ((0, 0), (0, 0))
end
