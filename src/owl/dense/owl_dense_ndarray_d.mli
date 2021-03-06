(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Bigarray

type elt = float

type arr = (float, float64_elt, c_layout) Genarray.t

include Owl_dense_ndarray_intf.Common with type elt := elt and type arr := arr

include Owl_dense_ndarray_intf.Real with type elt := elt and type arr := arr

include Owl_dense_ndarray_intf.NN with type arr := arr

include Owl_dense_ndarray_intf.Distribution with type arr := arr
