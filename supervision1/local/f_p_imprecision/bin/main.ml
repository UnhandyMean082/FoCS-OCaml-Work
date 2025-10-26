let mult (x: float) (n: int) =
  let rec loop (x: float) (n: int) (total: float) =
    if n <= 0 then total
    else loop (x) (n - 1) (total +. x)
  in
  loop x n 0.0

let () =
  Printf.printf "%f\n" (mult 0.1 1000000)
