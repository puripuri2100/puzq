open Range

type t =
  | TmInt of (Range.t * int)
  | TmString of (Range.t * string)
  | TmList of t list
  | TmTuple of t list