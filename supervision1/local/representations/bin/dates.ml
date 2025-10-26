open Representations 
open FourDigit

let my_year: year = make 2025

let () =
  Printf.printf "The current year is: %d\n" (get my_year);
  Printf.printf "2049 Less than equal to 1950: %b\n1950 Less than equal to 2049: %b\n" (is_lteq (make 2049) (make 1950)) (is_lteq (make 1950) (make 2049));
  Printf.printf "Adding 5 years to 2025 gives: %d\n" (get (add my_year 5))