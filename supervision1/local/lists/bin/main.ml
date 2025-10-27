(* Following 2 functions find the sum of a list's elements *)
let rec rec_sum (lst: int list) =
  match lst with
  | [] -> 0
  | x :: xs -> x + rec_sum xs

let iter_sum (lst: int list) =
  let rec loop (l: int list) (total: int) =
    match l with
    | [] -> total
    | x :: xs -> loop xs (total + x)
  in
  loop lst 0

(* Following 3 functions find the last element of a list *)
let rec rec_last (lst: 'a list) = 
  match lst with
  | [] -> failwith "Empty list has no last element"
  | x :: xs -> match xs with
    | [] -> x
    | _ -> rec_last xs

let rev_last (lst: 'a list) =
  match List.rev lst with
  | [] -> failwith "Empty list has no last element"
  | x :: _ -> x

let ind_last (lst: 'a list) =
  List.nth lst (List.length lst - 1)

(* Following function returns all even position elements of a list *)
let rec even_vals (lst: 'a list) =
  match lst with
  | [] -> []
  | [_] -> []
  | _ :: x :: xs -> x :: even_vals xs

(* Following function returns the list of tails *)
let rec tails (lst: 'a list) =
  match lst with
  | [] -> [[]]
  | _ :: xs -> lst :: tails xs

(* Following functions illustrate the benefits of sharing implementations *)
let sum (lst: int list) =
  let rec loop (lst: int list) (total: int) =
    match lst with
    | [] -> total
    | x :: xs -> loop xs (total + x)
  in
  loop lst 0

let mult (lst: int list) =
  let rec loop (lst: int list) (total: int) =
    match lst with
    | [] -> total
    | x :: xs -> loop xs (total * x)
  in
  loop lst 1

let rec fold (lst: int list) (op: int -> int -> int) (total: int) =
  match lst with
  | [] -> total
  | x :: xs -> fold xs op (op total x)

(* Test cases *)
let () =
  Printf.printf "%d\n%d\n" (rec_sum [1; 2; 3; 4; 5; 6; 7]) (iter_sum [1; 2; 3; 4; 5; 6; 7]);
  Printf.printf "%d\n%d\n%d\n" (rec_last [1; 2; 3; 4; 5; 6; 7]) (rev_last [1; 2; 3; 4; 5; 6; 7]) (ind_last [1; 2; 3; 4; 5; 6; 7]);
  Printf.printf "%s\n" (even_vals ["a"; "b"; "c"; "d"] |> String.concat ", ");
  Printf.printf "%s\n" (tails [1; 2; 3] |> List.map (fun lst -> "[" ^ (String.concat "; " (List.map string_of_int lst)) ^ "]") |> String.concat ", ");
  Printf.printf "%d\n%d\n%d\n%d\n" (sum [1; 2; 3; 4]) (mult [1; 2; 3; 4]) (fold [1; 2; 3; 4] ( + ) 0) (fold [1; 2; 3; 4] ( * ) 1)
