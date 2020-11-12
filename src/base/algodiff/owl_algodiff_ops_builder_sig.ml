(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module type Sig = sig
  type elt

  type arr

  type t

  type op

  module type Siso = sig
    val label : string

    (* primal op: input is a scalar *)
    val op_s : elt -> t

    (* primal op: input is an arr *)
    val op_a : arr -> t

    (* forward-mode gradient *)
    val df : t -> t -> t -> t

    (* reverse-mode gradient *)
    val dr : t -> t -> t ref -> t
  end

  val build_siso : (module Siso) -> t -> t
  (** build single input single output operations *)

  module type Sipo = sig
    val label : string

    (* primal op: input is a scalar *)
    val op_s : elt -> t * t

    (* primal op: input is a arr *)
    val op_a : arr -> t * t

    (* forward-mode gradient *)
    val df : t -> t -> t -> t

    (* reverse-mode gradient *)
    val dr : t -> t -> t ref * t ref -> t ref * t ref -> t
  end

  val build_sipo : (module Sipo) -> t -> t * t
  (** build single input pair outputs operations *)

  module type Sito = sig
    val label : string

    (* primal op: input is a scalar *)
    val op_s : elt -> t * t * t

    (* primal op: input is arr *)
    val op_a : arr -> t * t * t

    (* forward-mode gradient *)
    val df : t -> t -> t -> t

    (* reverse-mode gradient *)
    val dr : t -> t -> t ref * t ref * t ref -> t ref * t ref * t ref -> t
  end

  val build_sito : (module Sito) -> t -> t * t * t
  (** build single input triple outputs operations *)

  module type Siao = sig
    val label : string

    (* primal op: input is scalar *)
    val op_s : elt -> t array

    (* primal op: input is arr *)
    val op_a : arr -> t array

    (* forward-mode gradient *)
    val df : t -> t -> t -> t

    (* reverse-mode gradient *)
    val dr : t -> t -> t ref array -> t ref array -> t
  end

  val build_siao : (module Siao) -> t -> t array
  (** build single input array output operations *)

  module type Piso = sig
    val label : string

    (* primal op: input is (scalar, scalar) *)
    val op_ss : elt -> elt -> t

    (* primal op: input is (scalar, arr) *)
    val op_sa : elt -> arr -> t

    (* primal op: input is (arr, scalar) *)
    val op_as : arr -> elt -> t

    (* primal op: input is (arr, arr) *)
    val op_aa : arr -> arr -> t

    (* forward-mode from first input only *)
    val df_1 : t -> t -> t -> t -> t

    (* forward-mode from second input only *)
    val df_2 : t -> t -> t -> t -> t

    (* forward-mode from both inputs *)
    val df_12 : t -> t -> t -> t -> t -> t

    (* reverse-mode for both inputs *)
    val dr_12 : t -> t -> t -> t ref -> t * t

    (* reverse-mode for first input only *)
    val dr_1 : t -> t -> t -> t ref -> t

    (* reverse-mode for second input only *)
    val dr_2 : t -> t -> t -> t ref -> t
  end

  val build_piso : (module Piso) -> t -> t -> t
  (** build pair inputs single output operations *)

  module type Aiso = sig
    val label : string

    (* primal ops *)
    val op : t array -> t

    (* forward-mode *)
    val df : int list -> t -> t array -> t array -> t

    (* reverse-mode *)
    val dr : int list -> t array -> t -> t ref -> t list
  end

  val build_aiso : (module Aiso) -> t array -> t
  (** build array input single output operations *)

  module type Aiao = sig
    val label : string

    (* primal ops *)
    val op : t array -> t array

    (* forward-mode *)
    val df : int list -> t array -> t array -> t array -> t array

    (* reverse-mode *)
    val dr : int list -> t array -> t ref array -> t ref array -> t list
  end

  val build_aiao : (module Aiao) -> t array -> t array
  (** build array input array output operations *)
end
