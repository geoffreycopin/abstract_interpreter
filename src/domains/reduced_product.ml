open Abstract_syntax_tree
open Value_domain
open Domain

module Parity (V1: VALUE_DOMAIN) (V2: VALUE_DOMAIN) = (struct

  type t = V1.t * V2.t

  let top = V1.top, V2.top       

  let bottom = V1.bottom, V2.bottom
                
end: VALUE_DOMAIN)
