type 'a list =
  | Nil
  | Cons of 'a * 'a list

type 'a utree =
  | Lf
  | Br of 'a * 'a utree

let () =
  Printf.printf "Hello, world!\n"
