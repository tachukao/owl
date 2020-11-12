(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module Make (C : sig
  include Owl_algodiff_core_sig.Sig

  val reverse_add : t -> t -> t
end) =
struct
  open C

  (* core of reverse mode functions *)

  let reverse_reset x =
    let rec reset xs =
      match xs with
      | []     -> ()
      | x :: t ->
        (match x with
        | DR (_cp, aa, (_, register, _, _), fanout, _ai) ->
          aa := reset_zero !aa;
          fanout := succ !fanout;
          if !fanout = 1 then reset (register t) else reset t
        | _ -> reset t)
    in
    reset [ x ]


  let reverse_push =
    let rec push xs =
      match xs with
      | []          -> ()
      | (v, x) :: t ->
        (match x with
        | DR (cp, aa, (adjoint, _, _, _), fanout, _ai) ->
          aa := reverse_add !aa v;
          if !fanout = 1
          then push (adjoint cp aa t)
          else (
            fanout := pred !fanout;
            push t)
        | _ -> push t)
    in
    fun v x -> push [ v, x ]


  let reverse_prop v x =
    reverse_reset x;
    reverse_push v x
end
