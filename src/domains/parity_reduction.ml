open Value_reduction
open Interval_domain
open Parity_domain

module ParityReduction = (struct
  open Parity_domain.Parity
  module A = Parity_domain.Parity 
  module B = Interval_domain.Intervals

  (*let refine_bound a parity f = match a, parity with
    | Int(a), EVEN when Z.is_odd a -> Int(f a)
    | Int(a), ODD when Z.is_even a -> Int(f a) *)

  let reduce (p, y) = match y with
    | Itv(a, b) -> (p, y)
end: VALUE_REDUCTION)
