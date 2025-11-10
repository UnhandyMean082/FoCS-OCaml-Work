type 'a tree =
  | Lf
  | Br of (string * 'a) * 'a tree * 'a tree

exception Collision
let ins (k: string) (v: 'a) (tree: 'a tree): 'a tree =
  let rec loop (k: string) (v: 'a): 'a tree -> 'a tree = function
    | Lf -> Br ((k, v), Lf, Lf)
    | Br ((a, x), t1, t2) ->
      if k < a then
        Br ((a, x), loop k v t1, t2)
      else if a < k then
        Br ((a, x), t1, loop k v t2)
      else
        raise Collision
  in
  try
  loop k v tree
  with Collision -> 
    let _ = Printf.printf "There was a collision.\n\n" in tree

let del (k: string) (tree: 'a tree): 'a tree =
  let rec loop (k: string): 'a tree -> 'a tree = function
    | Lf -> Lf
    | Br ((a, x), t1, t2) ->
      if k < a then
        Br ((a, x), loop k t1, t2)
      else if a < k then
        Br ((a, x), t1, loop k t2)
      else
        match (t1, t2) with
        | (Lf, _) -> t2
        | (_, Lf) -> t1
        | _ ->
          let rec min_elt = function
            | Lf -> failwith "Unexpected Lf"
            | Br ((b, y), Lf, _) -> (b, y)
            | Br (_, l, _) -> min_elt l
          in
          let (b, y) = min_elt t2 in
          Br ((b, y), t1, loop b t2)
  in
  loop k tree

let () =
  let t1 = ins "b" 2 Lf in
  let t2 = ins "a" 1 t1 in
  let t3 = ins "c" 3 t2 in
  let t4 = ins "b" 4 t3 in
  Printf.printf "The list after inserting the first elements:\n";
  let rec print_tree = function
    | Lf -> ()
    | Br ((k, v), left, right) ->
      print_tree left;
      Printf.printf "%s: %d\n" k v;
      print_tree right
  in
  print_tree t4;
  let t5 = del "b" t4 in
  Printf.printf "\nThe list after deleting 'b':\n";
  print_tree t5
