open Abstract_syntax_tree
open Value_domain
open Value_reduction
open Domain

module ReducedProduct (R: VALUE_REDUCTION) = (struct

  type t = R.A.t * R.B.t

  let top = R.A.top, R.B.top       

  let bottom = R.A.bottom, R.B.bottom

  let is_bottom (x1, x2) = R.A.is_bottom x1 || R.B.is_bottom x2

  let const x = R.A.const x, R.B.const x

  let rand x y = R.A.rand x y, R.B.rand x y

  let print fmt (x1,x2) =
      R.A.print fmt x1;
      Format.fprintf fmt " ∧ ";
      R.B.print fmt x2

  let join (x1, x2) (y1, y2) = R.A.join x1 y1, R.B.join x2 y2

  let meet (x1, x2) (y1, y2) = R.A.meet x1 y1, R.B.meet x2 y2

  let subset (x1, x2) (y1, y2) = R.A.subset x1 y1 && R.B.subset x2 y2

  let widen (x1, x2) (y1, y2) = R.A.widen x1 y1, R.B.widen x2 y2

  let compare (x1, x2) (y1, y2) op =
    let (x1', y1') = R.A.compare x1 y1 op in
    let (x2', y2') = R.B.compare x2 y2 op in
    (x1', x2'), (y1', y2')

  let unary (x1, x2) op = R.A.unary x1 op, R.B.unary x2 op

  let bwd_unary (x1, x2) op (r1, r2) =
    R.A.bwd_unary x1 op r1, R.B.bwd_unary x2 op r2

  let binary (x1, x2) (y1, y2) op =
    R.A.binary x1 y1 op, R.B.binary x2 y2 op

  let bwd_binary (x1, x2) (y1, y2) op (r1, r2) =
    let (x1', y1') = R.A.bwd_binary x1 y1 op r1 in
    let (x2', y2') = R.B.bwd_binary x2 y2 op r2 in
    (x1', x2'), (y1', y2')
                
end: VALUE_DOMAIN)
