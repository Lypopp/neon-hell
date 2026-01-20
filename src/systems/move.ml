open Ecs
open Component_defs

type t = movable

let init _ = ()

let update dt el =
  Seq.iter (fun (e:t) ->
    e#velocity#set Vector.(add (mult dt e#forces#get) e#velocity#get);
    let v = e#velocity#get in
    if Vector.norm v <= 0.1 then
      e#velocity#set Vector.zero
    else begin
      e#position#set Vector.(add (mult dt e#velocity#get) e#position#get);
      e#velocity#set Vector.{x = v.x *. Cst.friction ; y = v.y}
    end
  ) el
