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
    | BOT
    | Itv of bound * bound

  let print fmt x = match x with
    | BOT -> Format.fprintf fmt "⊥"
    | Itv(a, b) -> Format.fprintf fmt "[";
                   print_bound fmt a;
                   Format.fprintf fmt ";";
                   print_bound fmt b;
                   Format.fprintf fmt "]"


  let lift1 f x = match x with
    | BOT -> BOT
    | Itv(a, b) -> f a b

  let lift2 f x y = match x,y with
    | BOT,_ | _,BOT -> BOT
    | Itv(a,b), Itv(c, d) -> f a b c d

  let neg x =
    lift1 (fun a b -> Itv(bound_neg b, bound_neg a)) x

  let subset (x:t) (y:t) : bool = match x,y with
    | BOT,_ -> true
    | _,BOT -> false
    | Itv(a, b), Itv(c, d) -> bound_cmp a c >= 0 && bound_cmp b d <= 0

  let top = Itv(MINF, PINF)

  let bottom = BOT

  let const c = Itv(Int c, Int c)

  let rand x y = Itv(Int x, Int y)

  let unary x op = match op with
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> neg x

  
                           
end: VALUE_DOMAIN)
