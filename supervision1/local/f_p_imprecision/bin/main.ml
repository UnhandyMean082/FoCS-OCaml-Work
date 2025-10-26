let mult (x: float) (n: int) =
  let rec loop (x: float) (n: int) (total: float) =
    if n <= 0 then total
    else loop (x) (n - 1) (total +. x)
  in
  loop x n 0.0

let golden (n: int) =
  let start = (1. +. sqrt 5.) /. 2.
  in
  let rec loop (n: int) (total: float) =
    if n <= 0 then total
    else loop (n - 1) (1. /. (total -. 1.))
  in
  loop n start

let () =
  Printf.printf "%f\n" (mult 0.1 1000000);
  Printf.printf "%f\n" (golden 50)
