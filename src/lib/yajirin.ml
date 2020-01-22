open Array
open String

open TmType
(*
矢印の方向
  ↑ : 1
  ← : 2
  ↓ : 3
  → : 4
*)

let fold_lefti f init lst =
  let rec sub i f init lst =
    match lst with
    | [] -> init
    | x :: xs -> sub (i + 1) f (f i init x) xs
  in
  sub 0 f init lst

let rec take i lst =
  match lst with
    | []      -> []
    | x :: xs -> (
        if i < 0 then
          []
        else
          x :: (take (i - 1) xs)
    )

let rec drop i lst =
  if i < 0 then
    lst
  else
    match lst with
    | []      -> []
    | x :: xs -> (
        if i < 1 then
          xs
        else
          drop (i - 1) xs
    )


let rec split_set n lst =
  if List.length lst <= 0 then
    []
  else
    let take_lst = take n lst in
    let drop_lst = drop n lst in
      take_lst :: (split_set n drop_lst)




exception Yajirin_Error

type t =
  | N (*何もないマス*)
  | Y of int * int (*“ヤジリ”数と方向*)
  | M (*黒マス*)
  | L of int * int (*線　方向と方向*)
  | W (*白確定マス*)


type yajirin_state = {
  mutable x_len : int;
  mutable y_len : int;
  mutable canvas : t array;
  mutable arrow_list : (int * (int list)) list;
}

let yajirin_state = {
  x_len = 10;
  y_len = 10;
  canvas = [||];
  arrow_list = [];
}


let set_x_len n = yajirin_state.x_len <- n
let get_x_len () = yajirin_state.x_len

let set_y_len n = yajirin_state.y_len <- n
let get_y_len () = yajirin_state.y_len

let set_canvas t_array = yajirin_state.canvas <- t_array
let get_canvas () = yajirin_state.canvas

let set_arrow_list lst = yajirin_state.arrow_list <- lst
let get_arrow_list () = yajirin_state.arrow_list

(*0スタート*)
let make_pos x_len x y = x_len * (y - 1) + x - 1

(*0スタート*)
let get_x_y x_len i =
  let x =
    let n = (i+1) mod x_len in
    if n = 0 then
      x_len
    else
      n
  in
  let y =
    if (i+1) mod x_len = 0 then
      i / x_len + 1
    else
      (i+1) / x_len + 1
    in
  (x, y)


let to_canvas str_array =
  let to_t str =
    let str_1 = String.sub str 0 1 in
    let str_2 = String.sub str 1 1 in
      match (str_1, str_2) with
      | (".", ".") -> N
      | (_, "^") -> Y(int_of_string str_1, 1)
      | (_, "<") -> Y(int_of_string str_1, 2)
      | (_, "v") -> Y(int_of_string str_1, 3)
      | (_, ">") -> Y(int_of_string str_1, 4)
      | _ -> raise Yajirin_Error
  in
    Array.map to_t str_array


let to_string canvas_array =
  let t_to_str t =
    match t with
    | N -> " 　"
    | W -> " ・"
    | Y(n, m) ->
      let str_m =
        match m with
        | 1 -> "↑"
        | 2 -> "←"
        | 3 -> "↓"
        | _ -> "→"
      in
      " " ^ string_of_int n ^ str_m
    | M -> " ■ "
    | L(n,m) ->
        match (n,m) with
        | (1,2) -> " ┘ "
        | (1,3) -> " │ "
        | (1,4) -> " └ "
        | (2,1) -> " ┘ "
        | (2,3) -> " ┐ "
        | (2,4) -> " ─ "
        | (3,1) -> " │ "
        | (3,2) -> " ┐ "
        | (3,4) -> " ┌ "
        | (4,1) -> " └ "
        | (4,2) -> " ─ "
        | _ (*(4,3)*) -> " ┌ "
  in
  let lst = Array.to_list canvas_array in
  let str_lst = List.map t_to_str lst in
  let x_len = get_x_len () in
  let join_n i s1 s2 =
    if i <= 0 then
      s2
    else
      s1 ^ "\n" ^ s2
  in
  let join i s1 s2 =
    if i = (x_len - 1) then
      s1 ^ "┃" ^ s2 ^ "┃"
    else
      s1 ^ "┃" ^ s2
  in
  let split_lst = split_set (x_len - 1) str_lst in
    List.map (fold_lefti join "") split_lst |> fold_lefti join_n ""


(*

  [|
    "0>"; ".."; ".."; "..";
    ".."; ".."; "1<"; "..";
    ".."; ".."; ".."; "..";
    ".."; ".."; ".."; "0<";
  |]

  ["0>"; ".."; ".."; ".."; ".."; ".."; "1<"; ".."; ".."; ".."; ".."; ".."; ".."; ".."; ".."; "0<"]

  [|
    Masu; Null; Null; Null;
    Null; Null; Masu; Null;
    Null; Null; Null; Null;
    Null; Null; Masu; Null;
  |]

  [|
     1; 2; 3; 4;
     5; 6; 7; 8;
     9;10;11;12;
    13;14;15;16;
  |]

*)

let get_val x_len canvas x y =
  let n = make_pos x_len x y in
  canvas.(n)


let is_null b =
  match b with
  | N -> true
  | _ -> false


let is_masu t =
  match t with
  | M -> true
  | _ -> false


let is_masu_and_masu t1 t2 =
  match (t1, t2) with
  | (M, M) -> true
  | _ -> false



(*Masuが隣り合ったらtrue*)
let is_wrong_masu x_len y_len canvas =
  let get_val_pos = get_val x_len canvas in
  let get_bool x y =
    let x_bool =
      try  is_masu_and_masu (get_val_pos x y) (get_val_pos (x + 1) y) (*右側*)
      with _ -> false(*単位元*)
    in
    let y_bool =
      try  is_masu_and_masu (get_val_pos x y) (get_val_pos x (y + 1)) (*下側*)
      with _ -> false(*単位元*)
    in
    x_bool || y_bool
  in
  (*
    黒マスでなかったら飛ばす
    黒マスだったら確認する↓
    まず右のマスが黒マスか、次に下が黒マスかを確かめる

    次のマスに移動するが、右端のときはyを一つ増やし、右端且つ下端のときはそこで終了
  *)
  let rec sub x y b =
    let new_b =
      if is_masu (get_val_pos x y) then
      (*黒マスである*)
        get_bool x y
      else
      (*黒マスでない*)
        false (*「または」での単位元*)
    in
    if x = x_len && y = y_len then
    (*右端且つ下端 ここで終わり*)
      b
    else
      if x = x_len then
        (*右端*)
        (sub 1 (y + 1)) (new_b || b)
      else
        (*普通*)
        (sub (x + 1) y) (new_b || b)
  in
    sub 1 1 false



(*ヤジリか黒マスか辺があるマスに線が行かないようにする*)
(*
  線が通るマスのみ見る
  線が出ている先の2つのマスの中身を見て、次のことをする
    * 辺だったら（x=1 v x=x_len v y=0 v y=y_len）true
    * 線でなかったらtrue
    * 線であったとしても、こちらのマスに向いていなかったらtrue
*)
let is_wrong_line x_len y_len canvas =
  let canvas_lst = Array.to_list canvas in
  let make_new_b x y i n m =
    let eq = (n = m) in (*nとmが一緒だったらtrue*)
    let check (new_x, new_y, p) =
      if (new_x <= 0 || new_y <= 0 || new_x > x_len || new_y > y_len) then
        (*この値が負であったら左端か上端に突き抜けているということなのでtrue*)
        (*この値が横幅や縦幅より大きいということは右端か下端に突き抜けているのでtrue*)
        true
      else
        let new_i = make_pos x_len new_x new_y in
        match canvas.(new_i) with
        | L(n,m) -> not (n = p || m = p) (*どちらかがpと同じだったら間違っていないわけなのでfalse*)
        | Y(_,_) -> true
        | M -> true
        | N -> false
        | W -> false
    in
    let is_l l =
      let t =
        match l with
        | 1 -> (x, y - 1, 3)
        | 2 -> (x - 1, y, 4)
        | 3 -> (x, y + 1, 1)
        | _ (*4*) -> (x + 1, y, 2)
      in
      check t
    in
      eq || (is_l n) || (is_l m)
  in
  let rec sub i lst b =
    let (x,y) = get_x_y x_len i in
    (*
    let () = Printf.printf "x %d\n" x in
    let () = Printf.printf "y %d\n" y in
    *)
    match lst with
    | [] -> b (*終わり*)
    | z :: zs ->
      match z with
      | L(n,m) ->
        let new_b = make_new_b x y i n m in
        sub (i + 1) zs (b || new_b)
      | _ -> sub (i + 1) zs b
  in
    sub 0 canvas_lst false



let get_arrow_range_lst x_len y_len canvas_ary =
  let canvas_lst = Array.to_list canvas_ary in
  let get_arrow_lst canvas_lst =
    let rec sub i lst1 lst2 =
      match lst2 with
      | [] -> lst1
      | Y(n,m) :: xs -> sub (i + 1) ((i,n,m) :: lst1) xs
      | x :: xs -> sub (i + 1) lst1 xs
    in
    sub 0 [] canvas_lst
  in
  let arrow_lst = get_arrow_lst canvas_lst in
  let rec sub lst =
    match lst with
    | [] -> []
    | x :: xs ->
      let (i,n,m) = x in (*何番目のマスか, 数字, 方向*)
      let (x,y) = get_x_y x_len i in
      let rec make_lst_p a b c =
        if a > c then
          []
        else
          a :: make_lst_p (a + b) b c
      in
      let rec make_lst_m a b c =
        if a < c then
          []
        else
          a :: make_lst_m (a - b) b c
      in
      let t =
        match m with
        (*(黒マスの個数,矢印上のマスのリスト)*)
        (*初期値 増やすもしくは減らす値 目標値*)
        | 1 -> (n,make_lst_m i x_len (x - 1))
        | 2 -> (n,make_lst_m i 1 (x_len * (y - 1)))
        | 3 -> (n,make_lst_p i x_len (x_len * (y_len - 1) + x))
        | _ (*4*) -> (n,make_lst_p i 1 (x_len * y - 1))
      in
      t :: (sub xs)
  in
  sub arrow_lst


let show (n,lst) =
  let str_n = string_of_int n in
  let join s1 s2 = s1 ^ ";" ^ s2 in
  let str_lst = lst |> List.map string_of_int |> List.fold_left join "" in
  "(" ^ str_n ^ ",[" ^ str_lst ^ "])"


(*黒マスの数が指定より多くなったらtrueを返す*)
let is_wrong_masu_vol x_len y_len pos canvas =
  let arrow_range_lst = get_arrow_list () in
  let filter_in_list pos =
    List.filter (fun (n,lst) -> List.exists ((=) pos) lst) arrow_range_lst
  in
  let in_list = filter_in_list pos in
  let check_vol (n,lst) =
    let rec vol i lst =
      match lst with
      | [] -> i
      | x :: xs ->
        match canvas.(x) with
        | M -> vol (i + 1) xs
        | _ -> vol i xs
    in
      n < (vol 0 lst)
  in
    List.exists check_vol in_list


(*Nullが一個でも存在したらtrue*)
let is_wrong_null canvas = Array.exists (fun t -> is_null t) canvas


(*線が一つのループになっていなかったらtrue*)
(*
無向単純グラフとして処理してみる
隣接リスト
チェックは「順々にたどっていき、元のマスに戻ったら処理を終了。記録しておいた“通ったマス”のリストと線が通るマスのリストが一致しなかったらtrueを返す（リストの長さだけで判別できるはず）」
*)
let is_wrong_roop x_len canvas =
  let to_graph ary =
    let lst = Array.to_list ary in
    let rec sub i lst =
      match lst with
      | [] -> []
      | x :: xs ->
        match x with
        | L(n,m) ->
          let fx =
            match (n,m) with
            (*数が大きい方を先に*)
            | (1,2) -> (i, i - x_len, i - 1)
            | (1,3) -> (i, i - x_len, i + x_len)
            | (1,4) -> (i, i - x_len, i + 1)
            | (2,1) -> (i, i - x_len, i - 1)
            | (2,3) -> (i, i - 1, i + x_len)
            | (2,4) -> (i, i - 1, i + 1)
            | (3,1) -> (i, i - x_len, i + x_len)
            | (3,2) -> (i, i - 1, i + x_len)
            | (3,4) -> (i, i + 1, i + x_len)
            | (4,1) -> (i, i - x_len, i + 1)
            | (4,2) -> (i, i - 1, i + 1)
            | _ (*(4,3)*) -> (i, i + 1, i + x_len)
          in
          fx :: sub (i + 1) xs
        | _ -> sub (i + 1) xs
    in
      sub 1 lst
  in
  let graph = to_graph canvas in
  let make_route_lst lst =
    let len = List.length graph in
    let first_num =
      List.hd lst |> (fun (i,_,_) -> i)
    in
    let f_m =
      List.hd lst |> (fun (_,_,m) -> m)
    in
    let rec sub i m lst =
      if first_num = m then
        [] (*最初に戻ったら終了*)
      else
        if i > len then
          [] (*graphの大きさを超えたら終了*)
        else
          let (_,_,m_2) = List.find (fun (j,_,_) -> m = j) lst in
            0 :: sub (i + 1) m_2 lst
    in
      sub 0 f_m lst
  in
  let route_lst =
    try make_route_lst graph
    with _ -> []
  in
  (*2つのリストの長さが等しかったら「ループがきちんと成立している」ということ*)
  not (List.length graph = List.length route_lst - 1)



let get_pos x_len t =
  let data_lst =
    match t with
    | TmTuple(lst) -> lst
    | _ -> raise Yajirin_Error
  in
  let get_int t =
    match t with
    | TmInt(_, n) -> n
    | _ -> raise Yajirin_Error
  in
  let get_t n = n |> List.nth data_lst |> get_int in
  let (x,y) = (get_t 0, get_t 1) in
  let pos = make_pos x_len x y in
    pos

let to_data t =
  let data_lst =
    match t with
    | TmTuple(lst) -> lst
    | _ -> raise Yajirin_Error
  in
  let get_int t =
    match t with
    | TmInt(_, n) -> n
    | _ -> raise Yajirin_Error
  in
  let get_t n = n |> List.nth data_lst |> get_int in
  let data =
    if List.length data_lst = 3 then
      (*黒マス登録*)
      match get_t 2 with
      | 0 -> W (*0は空白マス*)
      | _ -> M (*他の数は黒マス*)
    else
      (*線の登録*)
      L(get_t 2, get_t 3)
  in
    data

let operat pos t ary =
  let data = to_data t in
    ary.(pos) <- data

let return pos old_data ary =
    ary.(pos) <- old_data


let set_config x_len y_len str_array =
  let canvas = to_canvas str_array in
  let () = set_x_len x_len in
  let () = set_y_len y_len in
  let () = set_canvas canvas in
  let () = set_arrow_list (get_arrow_range_lst x_len y_len canvas) in
  ()


let main t =
  let x_len = get_x_len () in
  let y_len = get_y_len () in
  let canvas = get_canvas () in
  let pos = get_pos x_len t in
  let old_data = canvas.(pos) in
  let () = operat pos t canvas in (*値を書き換える*)
  let is_wrong =
    let masu = is_wrong_masu x_len y_len canvas in
    let line = is_wrong_line x_len y_len canvas in
    let masu_vol = is_wrong_masu_vol x_len y_len pos canvas in
    masu || line || masu_vol
  in
  let () = if is_wrong then return pos old_data canvas else () in (*失敗したときに巻き戻す*)
  let is_right = not is_wrong in
  let is_end =
    let null = is_wrong_null canvas in
    let roop = is_wrong_roop x_len canvas in
      not (null || roop)
  in
  let str = to_string canvas in
  (is_end, is_right, str)
