open System_defs
open Component_defs
open Ecs


let init dt =
  Ecs.System.init_all dt;
  Some ()

let last_dt = ref 0.
let update dt =
  let () = Input.handle_input () in
  let delta = (dt -. !last_dt) /. 25. in
  Gfx.debug "%f\n%!" delta;
  Move_system.update delta;
  Collision_system.update delta;
  Draw_system.update delta;
  last_dt := dt;
  None

let (let@) f k = f k


let run () =
  let window_spec = 
    Format.sprintf "game_canvas:%dx%d:"
      Cst.window_width Cst.window_height
  in
  let window = Gfx.create  window_spec in
  let ctx = Gfx.get_context window in
  let () = Gfx.set_context_logical_size ctx 800 600 in
  let _walls = Block.walls () in
  let global = Global.{ window; ctx } in
  Global.set global;
  let@ () = Gfx.main_loop ~limit:false init in
  let@ () = Gfx.main_loop update in ()








