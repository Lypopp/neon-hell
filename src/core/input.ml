let key_table = Hashtbl.create 16
let action_pressed_table = Hashtbl.create 16
let action_just_released_table = Hashtbl.create 16
let action_just_pressed_table = Hashtbl.create 16

let has_key table s = Hashtbl.mem table s

let set_key s =
  if has_key action_just_pressed_table s then
    (Hashtbl.find action_just_pressed_table s) ();
  Hashtbl.replace key_table s ()

let unset_key s =
  if has_key action_just_released_table s then 
    (Hashtbl.find action_just_released_table s) ();
  Hashtbl.remove key_table s

let register action_table key action =
  Hashtbl.replace action_table key action

let rec get_inputs () =
  match Gfx.poll_event () with
      KeyDown s -> if not (has_key key_table s) then (set_key s; get_inputs ())
    | KeyUp s -> unset_key s; get_inputs ()
    | Quit -> exit 0
    | NoEvent -> ()
    | _ -> ()
let handle_input () =
  get_inputs ();
  Hashtbl.iter (fun key action_pressed ->
      if has_key key_table key then action_pressed ()) action_pressed_table

let () =
  register action_pressed_table "n" (fun () -> ignore (Block.create_random ()) );

  register action_just_pressed_table "q" (fun () -> Player.move_direction (-1.));
  register action_just_released_table "q" (fun () -> Player.move_direction 1.);

  register action_just_pressed_table "d" (fun () -> Player.move_direction 1.);
  register action_just_released_table "d" (fun () -> Player.move_direction (-1.))