let triangle n: int =
  let rec loop (n: int) (total: int) = 
    if n = 0 then total
    else loop (n - 1) (total + n)
  in
  loop (n + 1) 0

let power (x: float) (n: int) =
  let rec loop (x: float) (n: int) (total: float) =
    if n = 0 then total
    else if n mod 2 = 0 then loop (x *. x) (n / 2) total
    else loop x (n - 1) (total *. x)
  in
  loop x n 1.0

let () =
  Printf.printf "%i\n" (triangle 10);
  Printf.printf "%.1f\n" (power 2.0 7)