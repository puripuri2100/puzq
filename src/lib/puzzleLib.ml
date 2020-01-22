open Lexing

open Range
open TmType
open PuzzleType
open Msg

open Yajirin


exception PuzzleLib_Error

type puzzle_state = {
  mutable x_len : int;
  mutable y_len : int;
  mutable canvas : string array;
}


let puzzle_state = {
  x_len = 10;
  y_len = 10;
  canvas = [||];
}

let set_x_len n = puzzle_state.x_len <- n
let get_x_len () = puzzle_state.x_len


let set_y_len n = puzzle_state.y_len <- n
let get_y_len () = puzzle_state.y_len


let set_canvas ary = puzzle_state.canvas <- ary
let get_canvas () = puzzle_state.canvas



let parser str =
  let t =
    str |>  Lexing.from_string |> Parse.parse Lex.lex
  in
    t




let to_str_canvas str =
  let t = parser str in
  let to_string t =
    match t with
    | TmString(_,str) -> str
    | _ -> raise PuzzleLib_Error
  in
  let lst =
    match t with
    | TmList(lst) -> List.map to_string lst
    | _ -> raise PuzzleLib_Error
  in
    Array.of_list lst


let set_config puzzle_type =
  let x_len = get_x_len () in
  let y_len = get_y_len () in
  let canvas = get_canvas () in
  match puzzle_type with
  | None -> Yajirin.set_config x_len y_len canvas
  | Some(Yajirin) -> Yajirin.set_config x_len y_len canvas


let main puzzle_type t =
  match puzzle_type with
  | None -> Yajirin.main t
  | Some(Yajirin) -> Yajirin.main t
