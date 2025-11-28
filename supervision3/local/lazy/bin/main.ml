let pairwiseLtLO (x, y: string * int) (x', y': string * int): bool =
  (x < x') && (y < y')

let () =
  Printf.printf "\nPairwise Less Than Lower Order (\"a\", 1) (\"b\", 2): %b\n" (pairwiseLtLO ("a", 1) ("b", 2));
