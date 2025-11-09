module Set: sig
type 'a t
val empty: 'a t
val singleton: 'a -> 'a t
val union: 'a t -> 'a t -> 'a t
val inter: 'a t -> 'a t -> 'a t
val to_list: 'a t -> 'a list
val to_set: 'a list -> 'a t
end = struct

type 'a t = 'a list

let empty = []

let singleton x = [x]

let rec member x = function
  | [] -> false
  | y :: l ->
    if x = y then true
    else member x l

let union xs ys =
  let rec loop xs ys acc =
    match xs with
    | [] ->
      if ys = [] then acc
      else loop ys [] acc
    | x :: xs ->
        if member x acc then
          loop xs ys acc
        else
          loop xs ys (x :: acc)
  in
  loop xs ys []

let inter xs ys =
  let rec loop xs ys acc =
    match xs with
    | [] -> List.rev acc
    | x :: xs ->
        if member x ys then
          loop xs ys (x :: acc)
        else
          loop xs ys acc
  in
  loop xs ys []

let to_list set = set

let to_set lst = lst

end

let rec zip xs ys =
  match (xs, ys) with
  | (x :: xs, y :: ys) -> (x, y) :: zip xs ys
  | _ -> []

let rec unzip = function
  | [] -> ([], [])
  | (x, y) :: pairs ->
      let xs, ys = unzip pairs in
      (x :: xs, y :: ys)

let () =
  Printf.printf "Zipping [1;2;3] and ['a';'b';'c']:\n";
  let zipped = zip [1;2;3] ['a';'b';'c'] in
  List.iter (fun (x, y) -> Printf.printf "(%d, %c) " x y) zipped;
  Printf.printf "\n";
  Printf.printf "Unzipping [(1,'a'); (2,'b'); (3,'c')]:\n";
  let xs, ys = unzip [(1,'a'); (2,'b'); (3,'c')] in
  Printf.printf "First list: ";
  List.iter (Printf.printf "%d ") xs;
  Printf.printf "\nSecond list: ";
  List.iter (Printf.printf "%c ") ys;
  Printf.printf "\n\n";
  Printf.printf "Set creation:\n";
  let s0 = Set.empty in
  Printf.printf "Empty set has %d elements.\n" (List.length (Set.to_list s0));
  let s1 = Set.singleton 1 in
  let s2 = Set.singleton 2 in
  let s3 = Set.union s1 s2 in
  Printf.printf "Union of singleton sets {1} and {2}:\n";
  List.iter (Printf.printf "%d ") (Set.to_list s3);
  Printf.printf "\n";
  let s4 = Set.inter (Set.to_set [1;2;3]) (Set.to_set [2;3;4]) in
  Printf.printf "Intersection of [1;2;3] and [2;3;4]:\n";
  List.iter (Printf.printf "%d ") (Set.to_list s4);
  Printf.printf "\n"
