(*

#directory "/tmp/l/github/e3/src_ln";;

#mod_use "e3_core_types.ml";;
#mod_use "hashed_sets_and_maps.ml";;
#mod_use "set_map_types.ml";;
#mod_use "e3_core.ml";;
#mod_use "e3_array.ml";;
#mod_use "core_types.ml";;
#mod_use "core.ml";;
#mod_use "common_impl.ml";;
#mod_use "e3_simple.ml";;
#mod_use "e3_test.ml";;
#mod_use "hashtbl_impl.ml";;
#mod_use "simple_impl.ml";;
#mod_use "examples.ml";;
#mod_use "e3_examples.ml";;

*)


open Hashed_sets_and_maps;;

module H = struct

  type t = int
  type t_hashed = int
  let hash x = x
  
end


module S = Default_hashset_impl(H)

let s = S.std_empty ()

let s = S.std_add 2 s

let _ = S.elements s

let s = S.std_add 3 s
