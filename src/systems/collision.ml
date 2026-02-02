open Ecs
open Component_defs
open Rect

type t = collidable


let init _ = ()

let rec iter_pairs f s =
  match s () with
    Seq.Nil -> ()
  | Seq.Cons(e, s') ->
    Seq.iter (fun e' -> f e e') s';
    iter_pairs f s'

let physic_collision e1 e2 has_player =
  if e1#mass#get = infinity && e2#mass#get = infinity then
    ()
  else
    let p, r = Rect.mdiff e1#position#get e1#box#get e2#position#get e2#box#get in
    if Rect.has_origin p r then begin
      let n = Rect.penetration_vector p r in
      let () = 
        match has_player with
        | None -> ()
        | Some i -> 
          if i = 0 && n.y < 0. then
            e1#tag#set (Player_tag true)
          else if i = 1 && n.y > 0. then
            e2#tag#set (Player_tag true)
      in
      let v1 = e1#velocity#get in
      let v2 = e2#velocity#get in
      if e1#mass#get = infinity then begin
        e2#position#set (Vector.sub e2#position#get n)
      end else if e2#mass#get = infinity then begin
        e1#position#set (Vector.add e1#position#get n)
      end else begin
        let n1, n2 =
          if Vector.is_almost_zero v1 && Vector.is_almost_zero v2 then begin
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
      let j =  ((-.(1. +. Cst.elasticity)) *. (Vector.dot v nnorm)) /. ((1./.m1) +. (1./.m2)) in
      e1#velocity#set (Vector.add v1 (Vector.mult (j/.m1) nnorm));
      e2#velocity#set (Vector.sub v2 (Vector.mult (j/.m2) nnorm))
    end

let affect (e_starter:t) (e_affected:t) =
  let pos_starter, box_starter = e_starter#position#get, e_starter#box#get in
  let pos_affected, box_affected = e_affected#position#get, e_affected#box#get in
  let p, r = Rect.mdiff pos_starter box_starter pos_affected box_affected in
  if Rect.has_origin p r then begin
    let v = Vector.(normalize (add (sub pos_affected pos_starter) e_starter#velocity#get)) in
    e_affected#resolve#get v e_starter#tag#get
  end

let update dt el =
    el
    |> Seq.iter (fun (e:t) -> 
      match e#tag#get with
      | Player_tag true -> e#tag#set (Player_tag false)
      | _ -> ()
      );
  for i = 0 to 2 do 
    el
    |> iter_pairs (fun (e1:t) (e2:t) ->
      match (e1#tag#get, e2#tag#get) with
      | (Player_tag _, Enemy_tag _) -> affect e2 e1
      | (Enemy_tag _, Player_tag _) -> affect e1 e2
      | (Player_tag _, _) -> physic_collision e1 e2 (Some 0)
      | (_, Player_tag _)  -> physic_collision e1 e2 (Some 1)
      | (_, _) -> physic_collision e1 e2 None
      )
  done
