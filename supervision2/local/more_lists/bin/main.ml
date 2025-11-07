let rec zip xs ys =
  match (xs, ys) with
  | (x :: xs, y :: ys) -> (x, y) :: zip xs ys
  | _ -> []

let rec unzip = function
  | [] -> ([], [])
  | (x, y) :: pairs ->
      let xs, ys = unzip pairs in
      (x :: xs, y :: ys)

let rec member x = function
  | [] -> false
  | y :: l ->
    if x = y then true
    else member x l

let rec inter xs ys acc =
  match xs with
  | [] -> List.rev acc
  | x :: xs ->
      if member x ys then
        inter xs ys (x :: acc)
      else
        inter xs ys acc

let rec set_union xs ys acc =
  match xs with
  | [] ->
    if ys = [] then acc
    else set_union ys [] acc
  | x :: xs ->
      if member x acc then
        set_union xs ys acc
      else
        set_union xs ys (x :: acc)

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
  Printf.printf "Intersection of [1;2;3;4] and [3;4;5;6]:\n";
  let intersection = inter [1;2;3;4] [3;4;5;6] [] in
  List.iter (Printf.printf "%d ") intersection;
  Printf.printf "\n\n";
  Printf.printf "Union of [1;2;3] and [3;4;5]:\n";
  let union = set_union [1;2;3] [3;4;5] [] in
  List.iter (Printf.printf "%d ") union;
  Printf.printf "\n"
