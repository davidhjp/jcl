open Javalib_pack.JBasics

let rec get_value = function
  | TBasic x ->
    (match x with
     | `Int -> "I"
     | `Bool -> "Z"
     | `Byte -> "B"
     | `Char -> "C"
     | `Double -> "D"
     | `Float -> "F"
     | `Long -> "J"
     | `Short -> "S"
    )
  | TObject x ->
    get_array_string x
and get_array_string = function
  | TArray x -> "["^(get_value x)
  | TClass x -> "L"^(cn_name x)
