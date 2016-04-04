type 'a t

let always_unit _ = ()

let check v fn =
  fn v

let validate v =
  true

let create n ?(check=fun x -> check x always_unit) ?(validate=validate) f =
  ()

let use p f =
  ()
