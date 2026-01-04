include Stdlib.Seq

let range lower upper =
  Stdlib.Seq.unfold (fun current ->
    if current < upper then Some (current, current + 1)
    else None
  ) lower
