open Value_reduction
open Parity_domain
open Interval_domain

module ParityIntervalsReduction = (struct
  module A = Parity
  module B = Intervals
           
  type t = A.t * B.t

  let reduce x = x
         
end : VALUE_REDUCTION)
