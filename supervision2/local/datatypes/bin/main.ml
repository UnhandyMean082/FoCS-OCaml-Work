type 'a list =
  | Nil
  | Cons of 'a * 'a list

type 'a utree =
  | Lf
  | Br of 'a * 'a utree

type expr =
  | Const of float
  | Var of string
  | Neg of expr
  | Add of expr * expr
  | Mul of expr * expr

type 'a option =
  | None
  | Some of 'a

let rec eval: expr -> float = function
  | Const c -> c
  | Var x -> failwith (Printf.sprintf "Variable %s not defined" x)
  | Neg x -> -. (eval x)
  | Add (x, y) -> eval x +. eval y
  | Mul (x, y) -> eval x *. eval y

let () =
  let _: 'a list = Cons (1, Cons (2, Nil)) in
  let _: 'a utree = Br (1, Lf) in
  let _: 'a option = None in
  let _: 'a option = Some 42 in
  Printf.printf "Evaluation of constant: %.1f\n"
    (eval (Const 3.5));
  Printf.printf "Evaluation of negation: %.1f\n"
    (eval (Neg (Const 5.0)));
  Printf.printf "Evaluation of multiplication: %.1f\n"
    (eval (Mul (Const 6.0, Const 7.0)));
  Printf.printf "Evaluation of addition: %.1f\n"
    (eval (Add (Const 1.5, Const 2.5)));
  Printf.printf "Evaluation of complex expression: %.1f\n"
    (eval (Add (Mul (Const 2.0, Const 3.0), Neg (Const 4.0))));
  Printf.printf "Error case (undefined variable):\n";
  (try
     let _ = eval (Add (Var "x", Const 2.0)) in
     ()
   with Failure msg ->
     Printf.printf "%s\n" msg)
