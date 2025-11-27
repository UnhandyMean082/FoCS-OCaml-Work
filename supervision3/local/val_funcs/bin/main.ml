let pairwiseLtLO (x, y: string * int) (x', y': string * int): bool =
  (x < x') && (y < y')

let lexicographicLtLO (x, y: string * int) (x', y': string * int): bool =
  (x < x') || (x = x' && y < y')

let pairwiseLtHO (x, y: 'a * 'b) (x', y': 'a * 'b) (lt: 'a -> 'a -> bool): bool =
  (lt x x') && (lt y y')

let lexicographicLtHO (x, y: 'a * 'b) (x', y': 'a * 'b) (eq: 'a -> 'a -> bool) (lt: 'a -> 'a -> bool): bool =
  (lt x x') || (eq x x' && lt y y')

let rec map2 (f: 'a -> 'b) (lst: 'a list list): 'b list list =
  match lst with
  | [] -> []
  | x :: xs -> (List.map f x) :: (map2 f xs)

let () =
  Printf.printf "\nPairwise Less Than Lower Order (\"a\", 1) (\"b\", 2): %b\n" (pairwiseLtLO ("a", 1) ("b", 2));
  Printf.printf "Pairwise Less Than Lower Order (\"a\", 2) (\"b\", 1): %b\n\n" (pairwiseLtLO ("a", 2) ("b", 1));
  Printf.printf "Lexicographic Less Than Lower Order (\"a\", 1) (\"a\", 2): %b\n" (lexicographicLtLO ("a", 1) ("a", 2));
  Printf.printf "Lexicographic Less Than Lower Order (\"a\", 2) (\"b\", 1): %b\n\n" (lexicographicLtLO ("a", 2) ("b", 1));
  Printf.printf "Pairwise Less Than Higher Order (3, 4) (5, 6) (<): %b\n" (pairwiseLtHO (3, 4) (5, 6) (<));
  Printf.printf "Pairwise Less Than Higher Order (3, 6) (5, 4) (<): %b\n\n" (pairwiseLtHO (3, 6) (5, 4) (<));
  Printf.printf "Lexicographic Less Than Higher Order (3, 4) (3, 5) (=) (<): %b\n" (lexicographicLtHO (3, 4) (3, 5) (=) (<));
  Printf.printf "Lexicographic Less Than Higher Order (4, 3) (3, 5) (=) (<): %b\n\n" (lexicographicLtHO (4, 3) (3, 5) (=) (<));
