type t =
  | Dummy of string
  | Normal of string * int * int * int * int


let dummy msg = Dummy(msg)


let make fname ln pos1 pos2 =
  Normal(fname, ln, pos1, ln, pos2)

let make_large fname ln1 pos1 ln2 pos2 =
  Normal(fname, ln1, pos1, ln2, pos2)