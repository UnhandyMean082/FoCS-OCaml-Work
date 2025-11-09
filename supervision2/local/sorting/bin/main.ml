let bubble (lst: int list): int list =
  let rec pass (lst: int list) (swap: bool) (newlist: int list) (acc: int list): int list =
    match lst with
    | [] -> []
    | [x] -> if swap then pass (List.rev newlist) false [] (x :: acc) else (List.rev (x :: newlist)) @ acc
    | x :: y :: xs ->
        if x > y then
          pass (x :: xs) true (y :: newlist) acc
        else
          pass (y :: xs) swap (x :: newlist) acc
  in
  pass lst false [] []

let () =
  Printf.printf "Sorting []:\n";
  let s1 = bubble [] in
  if s1 = [] then Printf.printf "[]" else List.iter (fun x -> Printf.printf "%d " x) s1;
  Printf.printf "\n";
  Printf.printf "Sorting [1;2;3;4;5]:\n";
  let s2 = bubble [1;2;3;4;5] in
  List.iter (fun x -> Printf.printf "%d " x) s2;
  Printf.printf "\n";
  Printf.printf "Sorting [5;2;1;4;3]:\n";
  let s3 = bubble [5;2;1;4;3] in
  List.iter (fun x -> Printf.printf "%d " x) s3;
  Printf.printf "\n";
