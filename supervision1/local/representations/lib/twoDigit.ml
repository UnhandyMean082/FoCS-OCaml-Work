type year = int

exception YearOutOfBoundsException

let make x =
  if 1950 <= x && x <= 2049 then
    if x < 2000 then x - 1900 else x - 2000
  else raise YearOutOfBoundsException

let get x =
  if 0 <= x && x <= 99 then
    if x <= 49 then x + 2000 else x + 1900
  else raise YearOutOfBoundsException

let is_lteq a b =
  if get a <= get b then true else false

let add a b =
  make (get a + b)
