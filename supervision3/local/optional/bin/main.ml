type day =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

let day_from_date (year: int) (month: int) (day: int): day =
  let zeller_congruence (y: int) (m: int) (d: int): int =
    let m' = if m < 3 then m + 12 else m in
    let y' = if m < 3 then y - 1 else y in
    let k = y' mod 100 in
    let j = y' / 100 in
    (d + (13 * (m' + 1)) / 5 + k + (k / 4) + (j / 4) + (5 * j)) mod 7
  in
  match zeller_congruence year month day with
  | 0 -> Saturday
  | 1 -> Sunday
  | 2 -> Monday
  | 3 -> Tuesday
  | 4 -> Wednesday
  | 5 -> Thursday
  | 6 -> Friday
  | _ -> failwith "Unexpected result from Zeller's Congruence"

let () =
  Printf.printf "2020-10-13 is a %s\n"
    (match day_from_date 2020 10 13 with
     | Monday -> "Monday"
     | Tuesday -> "Tuesday"
     | Wednesday -> "Wednesday"
     | Thursday -> "Thursday"
     | Friday -> "Friday"
     | Saturday -> "Saturday"
     | Sunday -> "Sunday")
