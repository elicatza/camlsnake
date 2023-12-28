open Raylib
open Vector

let bg = {r= 0x28; g= 0x28; b= 0x28; a= 0xff}
let fg = {r= 0xeb; g= 0xdb; b= 0xb2; a= 0xff}
let red = {r= 0xcc; g= 0x24; b= 0x1d; a= 0xff}
let green = {r= 0x98; g= 0x97; b= 0x1a; a= 0xff}

(** int32x2 *)
module Intx2 = struct
  type t = {x: int; y: int}

  let ( +^ ) a b = {x= a.x + b.x; y= a.y + b.y}

  let is_null a = if a.x = 0 && a.y = 0 then true else false

  let ( -^ ) a b = {x= a.x - b.x; y= a.y - b.y}

  let ( =^ ) a b = if a.x = b.x && a.y = b.y then true else false
end

type game = {width: int; height: int; rows: int; cols: int}

let d_up : Intx2.t = {x= 0; y= -1}
let d_down : Intx2.t = {x= 0; y= 1}
let d_left : Intx2.t = {x= -1; y= 0}
let d_right : Intx2.t = {x= 1; y= 0}

let key_up = 265
let key_down = 264
let key_left = 263
let key_right = 262

let thick = 1.

let draw_lines starts ends thick color =
  let rec aux starts ends counter =
    match (starts, ends) with
    | start :: start_rest, ending :: ending_rest ->
        let () = draw_line start ending thick color in
        aux start_rest ending_rest (counter + 1)
    | _ ->
        counter
  in
  aux starts ends 0

(** Starts at theta = 0, and goes counter clockwise. I.e. tr, tl, bl, br *)
let get_cornors game =
  let corner_tr = {x= float_of_int game.width; y= 0.} in
  let corner_tl = {x= 0.; y= 0.} in
  let corner_bl = {x= 0.; y= float_of_int game.height} in
  let corner_br = {x= float_of_int game.width; y= float_of_int game.height} in
  (corner_tr, corner_tl, corner_bl, corner_br)

let draw_grid_unit game unit_len color =
  let step_x = {x= unit_len; y= 0.} in
  let step_y = {x= 0.; y= unit_len} in
  let c_tr, c_tl, c_bl, c_br = get_cornors game in
  let rows =
    1 + draw_lines (range c_tl c_tr step_x) (range c_bl c_br step_x) thick color
  in
  let cols =
    1 + draw_lines (range c_tl c_bl step_y) (range c_tr c_br step_y) thick color
  in
  (rows, cols)

let get_dir prev_dir =
  match
    ( is_key_pressed key_up
    , is_key_pressed key_down
    , is_key_pressed key_left
    , is_key_pressed key_right )
  with
  | true, false, false, false -> d_up
  | false, true, false, false -> d_down
  | false, false, true, false -> d_left
  | false, false, false, true -> d_right
  | _ -> prev_dir

let cut = function [] -> [] | x :: xs -> xs

module Snake = struct
  type t =
    { head: Intx2.t
    ; tail: Intx2.t
    ; body: Intx2.t list
    ; dir: Intx2.t
    ; apple: Intx2.t
    ; unit_len: float }

  let init unit_len =
    { head= {x= 5; y= 5}
    ; tail= {x= 5; y= 7}
    ; body= [d_up; d_up]
    ; dir= d_up
    ; apple= {x= 2; y= 3}
    ; unit_len }

  let print s =
    Printf.printf "snake head [%d, %d] -+> [%d, %d]\n" s.head.x s.head.y s.dir.x
      s.dir.y

  let square_in_snake square snake =
    let rec aux (cur : Intx2.t) body =
      match body with
      | [] ->
          Intx2.( =^ ) square cur
      | x :: xs ->
          if Intx2.( =^ ) square cur then true else aux (Intx2.( +^ ) cur x) xs
    in
    aux snake.tail (List.append snake.body [snake.dir])

  let spawn_apple g snake =
    let rec aux () =
      let id = Random.int (g.rows * g.cols) in
      let x = id mod g.cols in
      let y = (id - x) / g.cols in
      let square : Intx2.t = {x; y} in
      match square_in_snake square snake with false -> square | true -> aux ()
    in
    aux ()

  let next g prev =
    let dir = get_dir prev.dir in
    let head = Intx2.( +^ ) prev.head dir in
    let unit_len = prev.unit_len in
    let apple = prev.apple in
    match Intx2.( =^ ) head apple with
    | false ->
        let body = List.append (cut prev.body) [dir] in
        let tail = Intx2.( +^ ) prev.tail (List.nth prev.body 0) in
        let new_s = {head; tail; body; dir; unit_len; apple} in
        new_s
    | true ->
        let body = List.append prev.body [dir] in
        let tail = prev.tail in
        let apple = spawn_apple g prev in
        let new_s = {head; tail; body; dir; unit_len; apple} in
        new_s
  (* Match new tile here to check for apple *)

  let update_dir s =
    let dir = get_dir s.dir in
    { head= s.head
    ; tail= s.tail
    ; body= s.body
    ; dir
    ; unit_len= s.unit_len
    ; apple= s.apple }

  let draw_snake snake color =
    let rec aux (cur : Intx2.t) list =
      match list with
      | [] ->
          draw_rectangle
            (int_of_float ((float_of_int cur.x *. snake.unit_len) +. thick))
            (int_of_float ((float_of_int cur.y *. snake.unit_len) +. thick))
            (int_of_float (snake.unit_len -. thick))
            (int_of_float (snake.unit_len -. thick))
            color
      | x :: xs ->
          let _ =
            draw_rectangle
              (int_of_float ((float_of_int cur.x *. snake.unit_len) +. thick))
              (int_of_float ((float_of_int cur.y *. snake.unit_len) +. thick))
              (int_of_float (snake.unit_len -. thick))
              (int_of_float (snake.unit_len -. thick))
              color
          in
          aux (Intx2.( +^ ) cur x) xs
    in
    aux snake.tail snake.body

  let draw_apple snake color =
    draw_rectangle
      (int_of_float ((float_of_int snake.apple.x *. snake.unit_len) +. thick))
      (int_of_float ((float_of_int snake.apple.y *. snake.unit_len) +. thick))
      (int_of_float (snake.unit_len -. thick))
      (int_of_float (snake.unit_len -. thick))
      color

  let is_colliding snake g =
    if
      snake.head.x < 0 || snake.head.y < 0 || snake.head.x >= g.cols
      || snake.head.y >= g.rows
    then true
    else
      let rec aux (cur : Intx2.t) body =
        match body with
        | [] ->
            Intx2.is_null cur
        | x :: xs ->
            if Intx2.is_null cur then true else aux (Intx2.( +^ ) cur x) xs
      in
      let rev_body = List.rev snake.body in
      aux (List.nth rev_body 0) (cut rev_body)
end

module Apple = struct end

let () =
  let _ = Random.init (unix_getentropy ()) in
  let width = 700 in
  let height = 700 in
  let rows =
    int_of_float (float_of_int height /. (float_of_int (min width height) /. 9.))
  in
  let cols =
    int_of_float (float_of_int width /. (float_of_int (min width height) /. 9.))
  in
  let g = {width; height; rows; cols} in
  let s = Snake.init (float_of_int (min width height) /. 9.) in
  let _ = init_window g.width g.height "Hello caml" in
  let _ = set_target_fps 60 in
  let rec loop snake frame =
    match window_should_close () with
    | false ->
        let snake =
          if frame mod 20 = 0 then Snake.next g snake
          else Snake.update_dir snake
        in
        if Snake.is_colliding snake g then
          Printf.printf "Snake collided!!\nYour score was: %d!\n"
            (List.length snake.body - 2)
        else
          let () = begin_drawing () in
          let () = clear_background bg in
          let _ =
            draw_grid_unit g (float_of_int (min g.width g.height) /. 9.) fg
          in
          let _ = Snake.draw_snake snake green in
          let _ = Snake.draw_apple snake red in
          let () = end_drawing () in
          loop snake (frame + 1)
    | true ->
        ()
  in
  let _ = loop s 0 in
  let _ = Raylib.close_window () in
  ()
