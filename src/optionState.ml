open PuzzleType

type state = {
  mutable is_work : bool;
  mutable puzzle_type : PuzzleType.puzzle_type option;
}

let state = {
  is_work = true;
  puzzle_type = None;
}


let set_puzzle_type t = state.puzzle_type <- Some(t)

let get_puzzle_type () = state.puzzle_type