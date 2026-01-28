open Ecs
open Component_defs

type t = movable

let init _ = ()

let update dt el =
  Seq.iter (fun (e:t) ->
    e#velocity#set Vector.(add (mult (dt /. e#mass#get) e#forces#get) e#velocity#get);
    let v = e#velocity#get in
    e#position#set Vector.(add (mult dt e#velocity#get) e#position#get);
    let fric = e#friction#get in
    e#velocity#set Vector.{x = v.x *. fric.x ; y = v.y *. fric.y};
    if Vector.norm v <= Cst.minimal_speed then
      e#velocity#set Vector.zero
  ) el
