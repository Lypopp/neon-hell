open Ecs
open Component_defs
open System_defs

let resolve (v : Vector.t) (player : player) (reacter : tag) =
  player#velocity#set (Vector.mult 2. v);
  Gfx.debug "The player has been hit\n%!"

let create (x, y, v, txt, width, height, mass) =
  let e = new player () in
  e#texture#set txt;
  e#position#set Vector.{x=float x ; y = float y};
  e#velocity#set v;
  e#box#set Rect.{width;height};
  e#mass#set mass;
  e#forces#set Cst.g;
  e#friction#set Vector.{x = 0.5 ; y = 1.};
  e#tag#set (Player_tag false);
  e#resolve#set (fun (v:Vector.t) (reacter:tag) -> resolve v e reacter);
  Collision_system.(register (e:>t));
  Move_system.(register (e:>t));
  Draw_system.(register (e:>t));
  e


let player = create (100, 100, Vector.{x=0. ; y=0.}, Texture.blue, 20, 50, 1.)


let move_direction d =
  player#forces#set Vector.(add {x = (d *. Cst.player_speed) ; y = 0.} player#forces#get)

let jump () =
  match player#tag#get with
    | Player_tag true ->
      player#tag#set (Player_tag false);
      player#velocity#set Vector.{x = player#velocity#get.x ; y = -.Cst.player_jump_speed}
    | _ -> ()

let fast_falling d =
  player#forces#set Vector.(add {x = 0.; y = (d *. Cst.player_fast_falling_speed)} player#forces#get)