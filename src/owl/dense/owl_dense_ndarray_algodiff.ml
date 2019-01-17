(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** Ndarray: module aliases *)


module Operator = struct
  include Owl_operator.Make_Basic (Owl_dense_ndarray_generic)
  include Owl_operator.Make_Extend (Owl_dense_ndarray_generic)
  include Owl_operator.Make_Ndarray (Owl_dense_ndarray_generic)
end


module S = struct
  include Owl_dense_ndarray_s
  include Operator
  module Scalar = Owl_maths
  module Linalg = Owl_linalg_d

  (* inject function aliases *)
  let inv = Owl_linalg_s.inv

  let tril ?(k=0) x = Owl_dense_matrix_generic.tril ~k x

  let triu ?(k=0) x = Owl_dense_matrix_generic.triu ~k x

  let qr x =
    let q, r, _ = Owl_linalg_s.qr ~thin:true ~pivot:false x in
    (q,r)

  let lyapunov = Owl_linalg_s.lyapunov
end


module D = struct
  include Owl_dense_ndarray_d
  include Operator
  module Scalar = Owl_maths
  module Linalg = Owl_linalg_d

  (* inject function aliases *)

  let inv = Owl_linalg_d.inv

  let mpow = Owl_linalg_d.mpow

  let tril ?(k=0) x = Owl_dense_matrix_generic.tril ~k x

  let triu ?(k=0) x = Owl_dense_matrix_generic.triu ~k x

  let qr x =
    let q, r, _ = Owl_linalg_d.qr ~thin:true ~pivot:false x in
    (q,r)

  let lyapunov = Owl_linalg_d.lyapunov
end


