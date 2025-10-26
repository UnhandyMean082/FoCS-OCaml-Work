let triangle n: int =
  let rec loop (n: int) (total: int) = 
    if n = 0 then total
    else loop (n - 1) (total + n)
  in
  loop (n + 1) 0

let () =
  Printf.printf "%i\n" (triangle 10)