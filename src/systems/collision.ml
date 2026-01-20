open Ecs
open Component_defs

type t = collidable

let e = 0.

let init _ = ()

let rec iter_pairs f s =
  match s () with
    Seq.Nil -> ()
  | Seq.Cons(e, s') ->
    Seq.iter (fun e' -> f e e') s';
    iter_pairs f s'


let update dt el =
  el
  |> iter_pairs (fun (e1:t) (e2:t) ->
  if e1#mass#get = infinity && e2#mass#get = infinity then
    ()
  else
    let p, r = Rect.mdiff e1#position#get e1#box#get e2#position#get e2#box#get in
    if not (Rect.has_origin p r) then
      ()
    else
      let n = Rect.penetration_vector p r in
      let v1 = e1#velocity#get in
      let v2 = e2#velocity#get in
      if e1#mass#get = infinity then begin
        e2#position#set (Vector.sub e2#position#get n)
      end else if e2#mass#get = infinity then begin
        e1#position#set (Vector.add e1#position#get n)
      end else begin
        let n1, n2 =
          if v1 = Vector.zero && v1 = v2 then begin
            0.5, 0.5
          end else begin
            let normv1 = Vector.norm v1 in
            let normv2 = Vector.norm v2 in
            normv1 /. (normv1 +. normv2), normv2 /. (normv1 +. normv2)
          end
        in
        e1#position#set (Vector.add e1#position#get (Vector.mult (n1) n));
        e2#position#set (Vector.sub e2#position#get (Vector.mult (n2) n))
      end;
      let nnorm = Vector.normalize n in
      let v = Vector.sub v1 v2 in
      let m1 = e1#mass#get in
      let m2 = e2#mass#get in
      let j =  ((-.(1. +. e)) *. (Vector.dot v nnorm)) /. ((1./.m1) +. (1./.m2)) in
      e1#velocity#set (Vector.add v1 (Vector.mult (j/.m1) nnorm));
      e2#velocity#set (Vector.sub v2 (Vector.mult (j/.m2) nnorm));
      ()

)
