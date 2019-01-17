(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** Ndarray: module aliases *)


module Operator = struct
  include Owl_operator.Make_Basic (Owl_base_dense_ndarray_generic)
  (* include Owl_operator.Make_Extend (Owl_base_dense_ndarray_generic) *)
  include Owl_operator.Make_Ndarray (Owl_base_dense_ndarray_generic)
end

module S = struct
  include Owl_base_dense_ndarray_s
  include Operator
  module Scalar = Owl_base_maths
  module Linalg = struct
    let inv _x = failwith "Owl_base_dense_ndarray_algodiff_linalg:inv not implemented"
    let qr _x = failwith "Owl_base_dense_ndarray_algodiff_linalg:qr not implemented"
    let lyapunov _x = failwith "Owl_base_dense_ndarray_algodiff_linalg:qr not implemented"
  end
  let diag ?(k=0) x = Owl_base_dense_ndarray_generic.diag ~k x
  let tril ?(k=0) x = Owl_base_dense_ndarray_generic.tril ~k x
  let triu ?(k=0) x = Owl_base_dense_ndarray_generic.triu ~k x
end


module D = struct
  include Owl_base_dense_ndarray_d
  include Operator
  module Scalar = Owl_base_maths
  module Linalg = struct 
    let inv _x = failwith "Owl_base_dense_ndarray_algodiff_linalg:inv not implemented"
    let qr _x = failwith "Owl_base_dense_ndarray_algodiff_linalg:qr not implemented"
    let lyapunov _x = failwith "Owl_base_dense_ndarray_algodiff_linalg:lyapunov not implemented"

  end
  let diag ?(k=0) x = Owl_base_dense_ndarray_generic.diag ~k x
  let tril ?(k=0) x = Owl_base_dense_ndarray_generic.tril ~k x
  let triu ?(k=0) x = Owl_base_dense_ndarray_generic.triu ~k x
end

