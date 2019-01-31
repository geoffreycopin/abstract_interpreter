open Abstract_syntax_tree
open Value_domain
   
module Parity = (struct
                  
  type t = BOT | ODD | EVEN | TOP

  let is_even x = match x with
    | EVEN -> true
    | _ -> false
    
  let string_of_t x = match x with
    | BOT -> "⊥"
    | ODD -> "odd"
    | EVEN -> "even"
    | TOP -> "⊤"

  let top = TOP

  let bottom = BOT

  let const x = if Z.is_even x then EVEN else ODD

  let rand x y =
    let c = Z.compare x y in
    if c == 0 then const x
    else if c < 0 then TOP
    else BOT

  let is_bottom x = match x with
    | BOT -> true
    | _ -> false

  let is_even x = match x with
    | EVEN -> true
    | _ -> false

  let is_odd x = match x with
    | ODD -> true
    | _ -> false

  let print fmt x = Format.fprintf fmt "%s" (string_of_t x)

  let join x y = match x, y with
    | BOT, a | a, BOT -> a
    | EVEN, EVEN | ODD, ODD -> x
    | TOP, _ | _, TOP -> TOP
    | _ -> BOT

  let meet x y = match x, y with
    | TOP, a | a, TOP -> a
    | EVEN, EVEN | ODD, ODD -> x
    | BOT, _ | _, BOT -> BOT
    | _ -> TOP

  let compare x y op = match x, y with
    | BOT, _ | _, BOT -> (BOT, BOT)
    | _ -> (x, y)

  let widen _ y = y

  let subset x y = match x, y with
    | _, TOP | EVEN, EVEN | ODD, ODD | BOT, EVEN | BOT, ODD -> true
    | _ -> false

  let add x y = match x, y with
    | BOT, _ | _, BOT -> BOT
    | TOP, _ | _, TOP -> TOP
    | EVEN, EVEN | ODD, ODD -> EVEN
    | _ -> ODD

  let mul x y = match x, y with
    | BOT, _ | _ , BOT -> BOT
    | EVEN, _ | _, EVEN -> EVEN
    | TOP, _ | _, TOP -> TOP
    | _ -> ODD

  let div x y = match x, y with
    | BOT, _ | _, BOT -> BOT
    | TOP, _ | _, TOP -> TOP
    | ODD, _ | _, ODD -> ODD
    | EVEN, EVEN -> EVEN

  let binary x y op = match op with
    | AST_PLUS -> add x y
    | AST_MINUS -> add x y
    | AST_MULTIPLY -> mul x y
    | AST_DIVIDE -> div x y

  let unary x _ = x

  let bwd_unary x op r = r

  let bwd_binary x y op r = (x, y)
                           
end: VALUE_DOMAIN)
