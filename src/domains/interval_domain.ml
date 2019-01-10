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

let bound_sub (a:bound) (b:bound) = match a, b with
  | MINF, PINF | PINF, MINF | PINF, PINF -> invalid_arg "bound_sub"
  | MINF, _ | _, MINF | _, PINF -> MINF
  | PINF, _ -> PINF
  | Int(a), Int(b) -> Int(Z.sub a b)

let bound_mul (a:bound) (b:bound) = match a, b with
  | Int(a), _ when a = Z.zero -> Int(Z.zero)
  | _, Int(a) when a = Z.zero -> Int(Z.zero)
  | MINF, MINF -> PINF
  | MINF, _ | _, MINF -> MINF
  | PINF, _ | _, PINF -> PINF
  | Int(a), Int(b) -> Int(Z.mul a b)

let bound_div (a:bound) (b:bound) = match a, b with
  | _, PINF | _, MINF -> Int(Z.zero)
  | PINF, Int(a) when a > Z.zero -> PINF
  | PINF, Int(a) when a < Z.zero -> MINF
  | MINF, Int(a) when a > Z.zero -> MINF
  | MINF, Int(a) when a < Z.zero -> PINF
  | PINF, Int(_) | MINF, Int(_) -> invalid_arg "bound_div"
  | Int(a), Int(b) -> Int(Z.div a b)

let bound_cmp (a:bound) (b:bound) = match a, b with
  | MINF, MINF | PINF, PINF -> 0
  | MINF, _ | _, PINF -> -1
  | _, MINF | PINF, _ -> 1
  | Int i, Int j -> Z.compare i j

let bound_min (l: bound list) =
  List.fold_left (fun a b -> if bound_cmp a b < 0 then a else b) PINF l

let bound_max (l: bound list) =
  List.fold_left (fun a b -> if bound_cmp a b > 0 then a else b) MINF l

let bound_succ a = match a with
  | MINF | PINF -> a
  | Int(x) -> Int(Z.succ x)

let bound_pred a = match a with
  | MINF | PINF -> a
  | Int(x) -> Int(Z.pred x)

module Intervals = (struct

  type t =
    | BOT
    | Itv of bound * bound

  let is_bottom x = match x with
    | BOT -> true
    | _ -> false

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

  let lift2 f x y = match x, y with
    | BOT,_ | _,BOT -> BOT
    | Itv(a,b), Itv(c, d) -> f a b c d

  let union x y = match x, y with
    | BOT, b -> b
    | a, BOT -> a
    | Itv(a, b), Itv(c, d) -> Itv(bound_min [a; c], bound_max [b; d])

  let inter x y =
    lift2 (fun a b c d ->
        if b < c || d < a then
          BOT
        else
          Itv(bound_max [a; c], bound_min [b; d])
      ) x y

  let subset (x:t) (y:t) : bool = match x,y with
    | BOT,_ -> true
    | _,BOT -> false
    | Itv(a, b), Itv(c, d) -> bound_cmp a c >= 0 && bound_cmp b d <= 0

  let disjoint (x:t) (y:t): bool = match inter x y with
    | BOT -> true
    | _ -> false

  let neg x =
    lift1 (fun a b -> Itv(bound_neg b, bound_neg a)) x

  let add x y =
    lift2 (fun a b c d -> Itv(bound_add a c, bound_add c d)) x y

  let sub x y =
    lift2 (fun a b c d -> Itv(bound_sub a d, bound_sub b c)) x y

  let mul x y =
    lift2 (fun a b c d ->
        let perms = [bound_mul a c; bound_mul a d; bound_mul b c; bound_mul b d] in
        let lo = bound_min perms in
        let hi = bound_max perms in
        Itv(hi, lo)
      ) x y

  let div' x y =
    lift2 (fun a b c d ->
        if c >= Int(Z.one) then
          let lo = bound_min [bound_div a c; bound_div a d] in
          let hi = bound_max [bound_div b c; bound_div b d] in
          Itv(lo, hi)
        else
          let lo = bound_min [bound_div b c; bound_div b d] in
          let hi = bound_max [bound_div a c; bound_div a d] in
          Itv(lo, hi)
      ) x y

  let div x y =
    let positive = div' x (inter y (Itv(Int(Z.one), PINF))) in
    let negative = div' x (inter y (Itv(MINF, Int(Z.minus_one)))) in
    union positive negative

  let eq a b = match a, b with
    | BOT, _ | _, BOT -> (BOT, BOT)
    | _ -> (inter a b, inter a b)

  let neq a b = match a, b with
    | BOT, _ | _, BOT | _ when disjoint a b -> (a, b)
    | Itv(a, b), Itv(c, d) when bound_cmp a c = 0 && bound_cmp b d = 0 -> (BOT, BOT)
    | Itv(a, b), Itv(c, d) when bound_cmp b d < 0 -> (Itv(a, bound_pred c), Itv(bound_succ b, d))
    | Itv(a, b), Itv(c, d) when bound_cmp a c > 0 -> (Itv(bound_succ d, b), Itv(c, bound_pred a))
    | _ -> (BOT, BOT)

  let gt a b = match a, b with
    | BOT, _ | _, BOT -> (a, b)
    | Itv(a', b'), Itv(c, d) when disjoint a b -> if bound_cmp b' c < 0
                                                then (BOT, b)
                                                else (a, BOT)
    | Itv(a, b), Itv(c, d) when bound_cmp b d > 0 -> (Itv(bound_succ d, b), BOT)
    | Itv(a, b), Itv(c, d) when bound_cmp b d < 0 -> (BOT, Itv(bound_succ b, d))
    | _ -> (BOT, BOT)

  let geq a b = let (a', b') = gt a b in
                let (a'', b'') = eq a b in
                (union a' a'', union b' b'')

  let top = Itv(MINF, PINF)

  let bottom = BOT

  let const c = Itv(Int c, Int c)

  let rand x y = Itv(Int x, Int y)

  let unary x op = match op with
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> neg x

  let binary x y op = match op with
    | AST_PLUS -> add x y
    | AST_MINUS -> sub x y
    | AST_MULTIPLY -> mul x y
    | AST_DIVIDE -> div x y

  let compare x y op =
    match op with
    | AST_EQUAL -> eq x y
    | AST_NOT_EQUAL -> neq x y
    | AST_GREATER_EQUAL -> geq x y
    | AST_GREATER -> gt x y
    | AST_LESS_EQUAL -> let y', x' = geq y x in x', y'
    | AST_LESS -> let y', x' = gt y x in x', y'
                           
end: VALUE_DOMAIN)
