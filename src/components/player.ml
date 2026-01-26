open Ecs
open Component_defs
open System_defs


let create (x, y, v, txt, width, height, mass) =
  let e = new player () in
  e#texture#set txt;
  e#position#set Vector.{x=float x ; y = float y};
  e#velocity#set v;
  e#box#set Rect.{width;height};
  e#mass#set mass;
  e#forces#set Cst.g;
  e#friction#set Vector.{x = 0.5 ; y = 1.};
  Collision_system.(register (e:>t));
  Move_system.(register (e:>t));
  Draw_system.(register (e:>t));
  e


let player = create (100, 100, Vector.{x=0. ; y=0.}, Texture.red, 20, 50, 1.)

let move_direction d =
  player#forces#set Vector.(add {x = (d *. Cst.player_speed) ; y = 0.} player#forces#get)

let jump () =
  if Collision.is_on_floor player (Collision_system.get_seq ()) then
    player#velocity#set Vector.{x = player#velocity#get.x ; y = -.Cst.player_jump_speed}

let fast_falling d =
  player#forces#set Vector.(add {x = 0.; y = (d *. Cst.player_fast_falling_speed)} player#forces#get)