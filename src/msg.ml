open Printf

let msg str =
  Printf.printf "%s\n" str


let get_msg () =
  read_line ()

let get_int () =
 let s = read_line () in
  int_of_string s