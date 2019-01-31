open Abstract_syntax_tree
open Value_domain
open Domain

module ReducedProduct (V1: VALUE_DOMAIN) (V2: VALUE_DOMAIN) = (struct

  type t = V1.t * V2.t

  let top = V1.top, V2.top       

  let bottom = V1.bottom, V2.bottom

  let is_bottom (x1, x2) = V1.is_bottom x1 || V2.is_bottom x2

  let const x = V1.const x, V2.const x

  let rand x y = V1.rand x y, V2.rand x y

  let print fmt (x1,x2) =
      V1.print fmt x1;
      Format.fprintf fmt " ∧ ";
      V2.print fmt x2

  let join (x1, x2) (y1, y2) = V1.join x1 y1, V2.join x2 y2

  let meet (x1, x2) (y1, y2) = V1.meet x1 y1, V2.meet x2 y2

  let subset (x1, x2) (y1, y2) = V1.subset x1 y1 && V2.subset x2 y2

  let widen (x1, x2) (y1, y2) = V1.widen x1 y1, V2.widen x2 y2

  let compare (x1, x2) (y1, y2) op =
    let (x1', y1') = V1.compare x1 y1 op in
    let (x2', y2') = V2.compare x2 y2 op in
    (x1', x2'), (y1', y2')

  let unary (x1, x2) op = V1.unary x1 op, V2.unary x2 op

  let bwd_unary (x1, x2) op (r1, r2) =
    V1.bwd_unary x1 op r1, V2.bwd_unary x2 op r2

  let binary (x1, x2) (y1, y2) op =
    V1.binary x1 y1 op, V2.binary x2 y2 op

  let bwd_binary (x1, x2) (y1, y2) op (r1, r2) =
    let (x1', y1') = V1.bwd_binary x1 y1 op r1 in
    let (x2', y2') = V2.bwd_binary x2 y2 op r2 in
    (x1', x2'), (y1', y2')
                
end: VALUE_DOMAIN)


module IntervalParity = (struct
  include ReducedProduct (Parity_domain.Parity) (Interval_domain.Intervals)
  let refine (x:Interval_domain.Intervals.t) y = x                                  
end: VALUE_DOMAIN)
