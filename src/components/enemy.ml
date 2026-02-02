open Ecs
open Component_defs
open System_defs

let resolve (v : Vector.t) (player : player) (reacter : tag) =
  player#velocity#set (Vector.mult 10. v);
  Gfx.debug "The player has been hit\n%!"

let create (x, y, v, txt, width, height, mass) =
  let e = new enemy () in
  e#texture#set txt;
  e#position#set Vector.{x=float x ; y = float y};
  e#velocity#set v;
  e#box#set Rect.{width;height};
  e#mass#set mass;
  e#forces#set Cst.g;
  e#friction#set Vector.{x = 0.5 ; y = 1.};
  e#tag#set (Enemy_tag false); 
  Collision_system.(register (e:>t));
  Move_system.(register (e:>t));
  Draw_system.(register (e:>t));
  e

let () = Gfx.debug "aaa\n%!"
let enemy = create (300, 300, Vector.{x=0. ; y=0.}, Texture.red, 20, 50, 1.)