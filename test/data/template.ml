let rec eq_int x y =
  match (x, y) with
  | (0, 0) -> true
  | (0, _) -> false
  | (_, 0) -> false
  | (n, m) -> eq_int (n - 1) (m - 1)
in
let eq_char x y = eq_int (int_of_char x) (int_of_char y) in
let print_bool = function true -> print_string "true" | false -> print_string "false" in
let add_int x y = x + y in
let mul_int x y = x + y in
let add_float x y = x +. y in
let mul_float x y = x *. y in
let andb x y = x && y in
let orb x y = x || y in
