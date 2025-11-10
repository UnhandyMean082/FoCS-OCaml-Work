type 'a tree =
  | Lf
  | Br of int * 'a tree * 'a tree

let rec ftree (k: int): int -> 'a tree = function
  | 0 -> Lf
  | n -> Br(k, ftree (2 * k) (n - 1), ftree (2 * k + 1) (n - 1))

let tree_sum (t: 'a tree): int =
  let rec loop (acc: int): 'a tree list -> int = function
    | [] -> acc
    | x :: xs -> 
      match x with
      | Lf -> loop acc xs
      | Br(v, l, r) -> loop (acc + v) (l :: r :: xs)
  in
  loop 0 [t]

let tree_product (t: 'a tree): int =
  let rec loop (acc: int): 'a tree list -> int = function
    | [] -> acc
    | x :: xs -> 
      match x with
      | Lf -> loop acc xs
      | Br(v, l, r) -> loop (acc * v) (l :: r :: xs)
  in
  loop 1 [t]

let tree_fold (f: int -> int -> int) (start: int) (t: 'a tree): int =
  let rec loop (f: int -> int -> int) (acc: int): 'a tree list -> int = function
    | [] -> acc
    | x :: xs -> 
      match x with
      | Lf -> loop f acc xs
      | Br(v, l, r) -> loop f (f acc v) (l :: r :: xs)
  in
  loop f start [t]

let () =
  Printf.printf "Tree sum: %d\n" (tree_sum (ftree 1 4));
  Printf.printf "Tree product: %d\n" (tree_product (ftree 1 4));
  Printf.printf "Tree fold (sum): %d\n" (tree_fold ( + ) 0 (ftree 1 4));
  Printf.printf "Tree fold (product): %d\n" (tree_fold ( * ) 1 (ftree 1 4))
