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
