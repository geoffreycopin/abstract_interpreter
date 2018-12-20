open Abstract_syntax_tree
open Value_domain

type bound =
  | Int of Z.t
  | PINF
  | MINF

let print_bound fmt a = match a with
  | Int i -> Format.fprintf fmt "%s" (Z.to_string i)
  | PINF -> Format.fprintf fmt "+∞"
  | MINF -> Format.fprintf fmt "-∞"

let bound_neg (a:bound) : bound = match a with
  | MINF -> PINF
  | PINF -> MINF
  | Int i -> Int (Z.neg i)

let bound_add (a:bound) (b:bound) = match a, b with
  | MINF, PINF | PINF, MINF -> invalid_arg "bound_add"
  | MINF, _ | _, MINF -> MINF
  | PINF, _ | _, PINF -> PINF
  | Int i, Int j -> Int (Z.add i j)

let bound_cmp (a:bound) (b:bound) = match a, b with
  | MINF, MINF | PINF, PINF -> 0
  | MINF, _ | _, PINF -> -1
  | _, MINF | PINF, _ -> 1
  | Int i, Int j -> Z.compare i j


module Intervals = (struct

  type t =
    | E
    | I of bound * bound

  let restore x = match x with
    | E -> E
    | I(low, up) -> if compare up low = -1 then reverse x else x

  let reverse x = match x with
    | E -> E
    | I(low, up) -> I(up, low)

  let print fmt x = match x with
    | E -> Format.fprintf fmt "⊥"
    | I (low, up) -> Format.fprintf fmt "[";
                  print_bound fmt low;
                  Format.fprintf fmt ";";
                  print_bound fmt up;
                  Format.fprintf fmt "]"

  let lift1 f x = match x with
    | E -> E
    | I(low, up) ->  restore I(f low, f up)

  let top = I (MINF, PINF)

  let bottom = E

  let const c = I (Int c, Int c)

  let rand x y = I (Int x, Int y)

  let unary x op = match op with
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> reverse (lift1 bound_neg x)
                           
end: VALUE_DOMAIN)
