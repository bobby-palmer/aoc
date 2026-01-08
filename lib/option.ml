include Stdlib.Option

(** Returns b if a is None and [f a_inner b] if a is Some *)
let merge f a b =
  match a with
  | Some value -> Some (f value b)
  | None -> Some b
