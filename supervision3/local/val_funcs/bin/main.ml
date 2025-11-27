let pairwiseLtLO (x, y: string * int) (x', y': string * int): bool =
  (x < x') && (y < y')

let lexicographicLtLO (x, y: string * int) (x', y': string * int): bool =
  (x < x') || (x = x' && y < y')

let pairwiseLtHO (x, y: 'a * 'b) (x', y': 'a * 'b) (lt: 'a -> 'a -> bool): bool =
  (lt x x') && (lt y y')

let lexicographicLtHO (x, y: 'a * 'b) (x', y': 'a * 'b) (eq: 'a -> 'a -> bool) (lt: 'a -> 'a -> bool): bool =
  (lt x x') || (eq x x' && lt y y')

let rec map2_inner (f: 'a -> 'b) (lst: 'a list list) (acc: 'b list): 'b list list =
  match lst with
  | [] -> []
  | x :: xs ->
      match x with
      | [] -> List.rev acc :: map2_inner f xs []
      | y :: ys -> map2_inner f (ys :: xs) ((f y) :: acc)

let map2 (f: 'a -> 'b) (lst: 'a list list): 'b list list =
  map2_inner f lst []

type 'a option = None | Some of 'a

let map_option (f: 'a -> 'b): 'a option -> 'b option = function
  | None -> None
  | Some x -> Some (f x)

let () =
  Printf.printf "\nPairwise Less Than Lower Order (\"a\", 1) (\"b\", 2): %b\n" (pairwiseLtLO ("a", 1) ("b", 2));
  Printf.printf "Pairwise Less Than Lower Order (\"a\", 2) (\"b\", 1): %b\n\n" (pairwiseLtLO ("a", 2) ("b", 1));
  Printf.printf "Lexicographic Less Than Lower Order (\"a\", 1) (\"a\", 2): %b\n" (lexicographicLtLO ("a", 1) ("a", 2));
  Printf.printf "Lexicographic Less Than Lower Order (\"a\", 2) (\"b\", 1): %b\n\n" (lexicographicLtLO ("a", 2) ("b", 1));
  Printf.printf "Pairwise Less Than Higher Order (3, 4) (5, 6) (<): %b\n" (pairwiseLtHO (3, 4) (5, 6) (<));
  Printf.printf "Pairwise Less Than Higher Order (3, 6) (5, 4) (<): %b\n\n" (pairwiseLtHO (3, 6) (5, 4) (<));
  Printf.printf "Lexicographic Less Than Higher Order (3, 4) (3, 5) (=) (<): %b\n" (lexicographicLtHO (3, 4) (3, 5) (=) (<));
  Printf.printf "Lexicographic Less Than Higher Order (4, 3) (3, 5) (=) (<): %b\n\n" (lexicographicLtHO (4, 3) (3, 5) (=) (<));
  let lst = [[1; 2; 3]; [4; 5]; [6]] in
  let mapped_lst = map2 (fun x -> x * 2) lst in
  Printf.printf "Mapped List:\n[";
  List.iter (fun sublist ->
    Printf.printf "[";
    List.iter (fun item -> Printf.printf "%d; " item) sublist;
    Printf.printf "]; "
  ) mapped_lst;
  Printf.printf "\n";
  let opt1 = Some 10 in
  let mapped_opt1 = map_option (fun x -> x + 5) opt1 in
  match mapped_opt1 with
  | Some v -> Printf.printf "\nMapped Option 1: Some %d\n" v
  | None -> Printf.printf "\nMapped Option 1: None\n";
  let opt2 = None in
  let mapped_opt2 = map_option (fun x -> x + 5) opt2 in
  match mapped_opt2 with
  | Some v -> Printf.printf "Mapped Option 2: Some %d\n" v
  | None -> Printf.printf "Mapped Option 2: None\n";
