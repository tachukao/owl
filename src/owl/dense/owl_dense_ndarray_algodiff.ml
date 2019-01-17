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
  module Linalg = struct
    include Owl_linalg_s
    let qr x = let q, r, _ = qr x in q, r
  end

  let diag ?(k=0) x = Owl_dense_ndarray_generic.diag ~k x
  let tril ?(k=0) x = Owl_dense_matrix_generic.tril ~k x
  let triu ?(k=0) x = Owl_dense_matrix_generic.triu ~k x

end


module D = struct
  include Owl_dense_ndarray_d
  include Operator
  module Scalar = Owl_maths
  module Linalg = struct 
    include Owl_linalg_d
    let qr x = let q, r, _ = qr x in q, r
  end

  let diag ?(k=0) x = Owl_dense_ndarray_generic.diag ~k x
  let tril ?(k=0) x = Owl_dense_matrix_generic.tril ~k x
  let triu ?(k=0) x = Owl_dense_matrix_generic.triu ~k x

end


