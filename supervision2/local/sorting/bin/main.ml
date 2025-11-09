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

let selection (lst: int list): int list =
  let rec min (lst: int list) (c: int) (run: int list) (acc: int list): int * int list =
    match lst with
    | [] -> (c, (List.tl (List.rev run)) @ acc)
    | x :: xs -> if x < c then min xs x [x] (run @ acc) else min xs c (x :: run) acc
  in
  let rec loop (lst: int list) (acc: int list): int list =
    match lst with
    | [] -> List.rev acc
    | _ ->
        let (m, rest): int * int list = min lst max_int [] [] in
        loop rest (m :: acc)
  in
  loop lst []

let () =
  Printf.printf "Bubble sorting []:\n";
  let s1 = bubble [] in
  if s1 = [] then Printf.printf "[]" else List.iter (fun x -> Printf.printf "%d " x) s1;
  Printf.printf "\n";
  Printf.printf "Bubble sorting [1;2;3;4;5]:\n";
  let s2 = bubble [1;2;3;4;5] in
  List.iter (fun x -> Printf.printf "%d " x) s2;
  Printf.printf "\n";
  Printf.printf "Bubble sorting [5;2;1;4;3]:\n";
  let s3 = bubble [5;2;1;4;3] in
  List.iter (fun x -> Printf.printf "%d " x) s3;
  Printf.printf "\n";
  Printf.printf "Selection sorting []:\n";
  let s4 = selection [] in
  if s1 = [] then Printf.printf "[]" else List.iter (fun x -> Printf.printf "%d " x) s4;
  Printf.printf "\n";
  Printf.printf "Selection sorting [1;2;3;4;5]:\n";
  let s5 = selection [1;2;3;4;5] in
  List.iter (fun x -> Printf.printf "%d " x) s5;
  Printf.printf "\n";
  Printf.printf "Selection sorting [5;2;1;4;3]:\n";
  let s6 = selection [5;2;1;4;3] in
  List.iter (fun x -> Printf.printf "%d " x) s6;
  Printf.printf "\n"
