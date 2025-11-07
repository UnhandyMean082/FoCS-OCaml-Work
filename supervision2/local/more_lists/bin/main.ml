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
