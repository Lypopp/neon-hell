open Ecs
open Component_defs
open System_defs


let create (x, y, v, txt, width, height, mass) =
  let e = new player () in
  e#texture#set txt;
  e#position#set Vector.{x=float x;y = float y};
  e#velocity#set v;
  e#box#set Rect.{width;height};
  e#mass#set mass;
  e#forces#set Cst.g;
  Collision_system.(register (e:>t));
  Move_system.(register (e:>t));
  Draw_system.(register (e:>t));
  e


let player = create (100, 100, Vector.{x=0. ; y=0.}, Texture.red, 20, 50, 0.2)

let move_direction d =
  player#forces#set Vector.(add (mult (d *. Cst.player_speed) {x = 1. ; y = 0.}) player#forces#get)