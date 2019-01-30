open Abstract_syntax_tree
open Value_domain
   
module Parity = (struct
                  
  type t =
    | BOT
    | ODD
    | EVEN
    | TOP

  let t_cmp = 

  let min (xs: t list) =
    List.fold_left (fun x y -> )

  let string_of_t x = match x with
    | BOT -> "BOT"
    | ODD -> "ODD"
    | EVEN -> "EVEN"
    | TOP -> "TOP"

  let top = TOP

  let bottom = BOT

  let const x = if Z.is_even x then EVEN else ODD

  let rand x y = match Z.compare x y with
    | 0 -> const x
    | 1 -> BOT
    | -1 -> TOP

  let is_bottom x = match x with
    | BOT -> true
    | _ -> false   

  let print fmt x = Format.printf fmt (string_of_t x) 
                           
end: VALUE_DOMAIN)
