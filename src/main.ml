open Arg
open String
open Str
open Printf
open Filename

open Range
open PuzzleType
open PuzzleLib
open OptionState
open Msg

let arg_version () =
  print_string "puzq version 0.0.1\n"


let arg_puzzle_type puzzle_type =
  let t =
    match puzzle_type with
    | "yajirin" -> Yajirin
    | _ -> Yajirin
  in
  OptionState.set_puzzle_type t


let arg_err _ = Printf.printf "%s" "err"


let arg_spec curdir =
  [
    ("-v",        Arg.Unit(arg_version)  , "Prints version");
    ("--version", Arg.Unit(arg_version)  , "Prints version");
    ("-p",       Arg.String(arg_puzzle_type), "puzzle type");
    ("--puzzle", Arg.String(arg_puzzle_type), "puzzle type");
  ]


let main =
  let curdir = Sys.getcwd () in
  let () = Arg.parse (arg_spec curdir) arg_puzzle_type "" in
  let puzzle_type = OptionState.get_puzzle_type () in
  let () = Msg.msg "横の長さ" in
  let () = Msg.get_int () |> PuzzleLib.set_x_len in
  let () = Msg.msg "縦の長さ" in
  let () = Msg.get_int () |> PuzzleLib.set_y_len in
  let () = Msg.msg "盤面" in
  let () = Msg.get_msg () |> PuzzleLib.to_str_canvas |> PuzzleLib.set_canvas in
  let () = PuzzleLib.set_config puzzle_type in
  let rec get_t () =
    let ans = Msg.get_msg () in
    try PuzzleLib.parser ans
    with _ -> let () = Msg.msg "やり直し" in get_t ()
  in
  let rec main_do () =
    let () = Msg.msg "操作>" in
    let (b1, b2, str) =
      (*b1は正答条件を満たしているかの判定 b2はその操作が正しいかの判定 strはその結果としての盤面*)
      PuzzleLib.main puzzle_type (get_t ())
    in
    let () = Msg.msg (string_of_bool b2) in
    let () = Msg.msg str in
    if b1 then
      Msg.msg "終わり"
    else
      main_do ()
  in
    main_do ()
