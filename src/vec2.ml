let apply f (ax, ay) (bx, by) = (f ax bx, f ay by)

let add a b = apply (+.) a b
let sub a b = apply (-.) a b

let scale s (x, y) = (s *. x, s *. y)

let dot (ax, ay) (bx, by) = ax *. bx +. ay *. by

let length v = sqrt @@ dot v v

let normalize v = scale (1. /. length v) v
