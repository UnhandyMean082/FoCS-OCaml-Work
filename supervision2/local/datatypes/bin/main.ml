type 'a list =
  | Nil
  | Cons of 'a * 'a list

type 'a utree =
  | Lf
  | Br of 'a * 'a utree

type arithmetic =
  | Const of float
  | Var of string
  | Neg of arithmetic
  | Add of arithmetic * arithmetic
  | Mul of arithmetic * arithmetic

let () =
  Printf.printf "Hello, world!\n"
