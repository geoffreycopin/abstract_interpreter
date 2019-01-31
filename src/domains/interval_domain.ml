open Abstract_syntax_tree
open Value_domain

module Bound = struct
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
  | MINF, PINF | PINF, PINF -> invalid_arg "bound_sub"
  | MINF, _ | _, PINF -> MINF
  | PINF, _ | _, MINF -> PINF
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
  | MINF, PINF -> -1
  | PINF, MINF -> 1
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

let bound_neg a = match a with
  | PINF -> MINF
  | MINF -> PINF
  | Int(x) -> Int(Z.neg x)
end

module Intervals = (struct
  open Bound
                        
  type t =
    | BOT
    | Itv of bound * bound

  let is_const x = match x with
    | Itv(a, b) when bound_cmp a b == 0 -> true
    | _ -> false

  let itv_size a = match a with
    | BOT -> Int(Z.zero)
    | Itv(Int(a), Int(b)) when Z.compare a b < 0 -> Int(Z.abs (Z.sub a b))
    | _ -> MINF

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

  let remove_const itv const =
    lift1 (fun a b ->
        if bound_cmp a const > 0 || bound_cmp b const < 0 then
          itv
        else if is_const itv then
          BOT
        else
          let x = Itv(bound_succ a, b) in
          let y = Itv(a, bound_pred b) in
          if bound_cmp (itv_size x) (itv_size y) >= 0 then x else y
      ) itv

  let union x y = match x, y with
    | BOT, b -> b
    | a, BOT -> a
    | Itv(a, b), Itv(c, d) -> Itv(bound_min [a; c], bound_max [b; d])

  let inter x y =
    lift2 (fun a b c d ->
        if bound_cmp b c < 0 || bound_cmp d a < 0 then
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
    lift2 (fun a b c d -> Itv(bound_add a c, bound_add b d)) x y

  let sub x y =
    lift2 (fun a b c d ->
        let lo = bound_min [bound_sub a c; bound_sub a d] in
        let hi = bound_max [bound_sub b c; bound_sub b d] in
        Itv(lo, hi)
      ) x y

  let mul x y =
    lift2 (fun a b c d ->
        let perms = [bound_mul a c; bound_mul a d; bound_mul b c; bound_mul b d] in
        let lo = bound_min perms in
        let hi = bound_max perms in
        Itv(lo, hi)
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

  let join a b = union a b

  let meet a b = match a, b with
    | BOT, _ | _, BOT -> BOT
    | Itv(a, b), Itv(c, d) -> Itv(bound_max [a; c], bound_min [b; d])

  let widen a b =
    lift2 (fun a b c d ->
        let lo = if bound_cmp c a < 0 then MINF else bound_min [c; a] in
        let hi = if bound_cmp d b > 0 then PINF else bound_max [d; b] in
        Itv(lo, hi)) a b

  let eq a b = match a, b with
    | BOT, _ | _, BOT -> (BOT, BOT)
    | _ -> (inter a b, inter a b)

  let neq a b =
    match a, b with
    | Itv(a', b'), Itv(c, d) when is_const a && is_const b
                                  && bound_cmp a' d == 0 -> (BOT, BOT)
    | Itv(a', _), Itv(_, _) when is_const a -> (a, remove_const b a')
    | Itv(_, _), Itv(c, _) when is_const b -> (remove_const a c, b)
    | _ -> (a, b)

  let gt a b = match a, b with
    | BOT, _ | _, BOT -> (a, b)
    | Itv(a, b), Itv(c, d) when bound_cmp c b >= 0 -> (BOT, BOT)
    | Itv(a, b), Itv(c, d) -> let a_lo = bound_max [a; bound_succ c] in
                              let a_hi = b in
                              let b_lo = c in
                              let b_hi = bound_min [d; bound_pred b] in
                              (Itv(a_lo, a_hi), Itv(b_lo, b_hi))

  let geq a b = let (a', b') = gt a b in
                let (a'', b'') = eq a b in
                (union a' a'', union b' b'')

  let top = Itv(MINF, PINF)

  let bottom = BOT

  let const c = Itv(Int c, Int c)

  let rand x y = if Z.compare x y <= 0 then Itv(Int x, Int y) else BOT

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

  let bwd_unary_minus a b = 
    match a, b with
    | _, Itv(r, s) -> Itv(bound_neg s, bound_neg r)
    | _ -> BOT

  let bwd_unary x op r = match op with
    | AST_UNARY_PLUS -> r
    | AST_UNARY_MINUS -> bwd_unary_minus x r

  let bwd_binary_plus a b r = match a, b, r with
    | Itv(a, b), Itv(c, d), Itv(r, s) -> inter (Itv(a, b)) (Itv(bound_sub r d, bound_sub s c)),
                                         inter (Itv(c, d)) (Itv(bound_sub r b, bound_sub s a))
    | _ -> a, b

  let bwd_binary_minus a b r =  match a, b, r with
    | Itv(a, b), Itv(c, d), Itv(r, s) -> inter (Itv(a, b)) (Itv(bound_add r d, bound_add s c)),
                                         inter (Itv(c, d)) (Itv(bound_sub a s, bound_sub b r))
    | _ -> a, b

  let bwd_binary_mul a b r =  match a, b, r with
    | Itv(a, b), Itv(c, d), Itv(r, s) -> inter (Itv(a, b)) (Itv(bound_div r d, bound_div s c)),
                                         inter (Itv(c, d)) (Itv(bound_div r b, bound_div s a))
    | _ -> a, b

  let bwd_binary x y op r = match op with
    | AST_PLUS -> bwd_binary_plus x y r
    | AST_MINUS -> bwd_binary_minus x y r
    | AST_MULTIPLY -> bwd_binary_mul x y r
    | AST_DIVIDE -> x, y
                           
end: VALUE_DOMAIN)
