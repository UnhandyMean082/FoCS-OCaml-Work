type year = int

exception YearOutOfBoundsException

let make x =
  if 1950 <= x && x <= 2049 then x
  else raise YearOutOfBoundsException

let get x =
  if 1950 <= x && x <= 2049 then x
  else raise YearOutOfBoundsException

let is_lteq a b =
  if get a <= get b then true else false

let add a b =
  make (get a + b)
