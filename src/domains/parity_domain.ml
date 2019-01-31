open Abstract_syntax_tree
open Value_domain
   
module Parity = (struct
                  
  type t =
    | BOT
    | ODD
    | EVEN
    | TOP

  let compare_parity x y = match x, y with
    | BOT, BOT | TOP, TOP -> 0
    | BOT, _ -> -1
    | _, BOT -> 1
    | TOP, _ -> 1
    | _ -> 0

  let min_parity (xs: t list) =
    List.fold_left (fun x y -> if compare_parity x y < 0 then x else y) TOP xs

  let max_parity (xs: t list) =
    List.fold_left (fun x y -> if compare_parity x y > 0 then x else y) BOT xs

  let string_of_t x = match x with
    | BOT -> "BOT"
    | ODD -> "ODD"
    | EVEN -> "EVEN"
    | TOP -> "TOP"

  let top = TOP

  let bottom = BOT

  let const x = if Z.is_even x then EVEN else ODD

  let rand x y =
    let c = Z.compare x y in
    if c == 0 then const x
    else if c < 0 then BOT
    else TOP

  let is_bottom x = match x with
    | BOT -> true
    | _ -> false   

  let print fmt x = Format.fprintf fmt "%s" (string_of_t x)

  let join x y = match x, y with
    | BOT, a | a, BOT -> a
    | a, b when compare_parity a b == 0 -> a
    | TOP, _ | _, TOP -> TOP
    | _ -> BOT

  let meet x y = match x, y with
    | TOP, a | a, TOP -> a
    | a, b when compare_parity a b == 0 -> a
    | BOT, _ | _, BOT -> BOT
    | _ -> TOP

  (* let eq x y = match x, y with
    | TOP, a | a, TOP -> (a, a)
    | _ -> (BOT, BOT)

  let neq x y = match x, y with
    | TOP, EVEN | ODD, TOP -> (ODD, EVEN)
    | EVEN, TOP | TOP, ODD -> (EVEN, ODD)
    | _ -> (x, y)

  let lt x y = match x, y with
    | TOP, _ | _, TOP | BOT, _ | _, BOT -> (x, y)
    | _ -> (BOT, BOT)

  let leq x y =
    let x_lt, y_lt = lt x y in
    let x_eq, y_eq = eq x y in
    (join x_lt x_eq, join y_lt x_eq)

  let gt x y = match x, y with
    | BOT, _ | _, BOT -> (BOT, BOT) *)

  let compare x y op = match x, y with
    | BOT, _ | _, BOT -> (BOT, BOT)
    | _ -> (x, y)

  let widen _ y = y

  let subset x y = y == TOP

  let add x y = match x, y with
    | BOT, _ | _, BOT -> BOT
    | TOP, _ | _, TOP -> TOP
    | x, y when compare_parity x y == 0 -> EVEN
    | _ -> ODD

  let mul x y = match x, y with
    | BOT, _ | _ , BOT -> BOT
    | TOP, _ | _, TOP -> TOP
    | EVEN, _ | _, EVEN -> EVEN
    | _ -> ODD

  let binary x y op = match op with
    | AST_PLUS -> add x y
    | AST_MINUS -> add x y
    | AST_MULTIPLY -> mul x y
    | AST_DIVIDE -> mul x y

  let unary x _ = x

  let bwd_unary x op r = r

  let bwd_binary x y op r = (x, y)
                           
end: VALUE_DOMAIN)
