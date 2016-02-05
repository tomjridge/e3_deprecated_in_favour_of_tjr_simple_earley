
(** basic set *)
module type Set_t = sig

  type elt
  type t
  val std_empty: unit -> t

  (* indicate whether the elt was already in the set *)
  val std_add: elt -> t -> (t * bool)
    
  val std_mem: elt -> t -> bool
  
end


(* maps to sets of values *)
module type Map_t = sig

  type t
  type key
  type value
  val map_empty: unit -> t
  val map_add_cod: key -> value -> t -> t

end


module type Mfc = sig
  include Map_t
  val map_fold_cod: key -> (value -> 'b -> 'b) -> t -> 'b -> 'b
end

module type Mce = sig
  include Map_t
  val map_cod_empty: key -> t -> bool
end

module type Mbk = sig
  include Mfc
  (* FIXME following - should use destructive update with subsignature? *)
  include Mce with type t:=t and type key := key and type value:=value
end

module type Mck = sig
  include Mfc
  val map_find_cod: key -> value -> t -> bool
end

(* FIXME obsolete *)
(*
module type Mti = sig
  include Map_t
  val map_find_cod: key -> value -> t -> bool
end
*)

module type Mssii = sig
  include Map_t
  val mssii_elts_cod: key -> t -> value list
end

(* result type of Default_map_impl *)
module type Mall = sig
  include Map_t
  val map_fold_cod: key -> (value -> 'b -> 'b) -> t -> 'b -> 'b
  val map_cod_empty: key -> t -> bool
  val map_find_cod: key -> value -> t -> bool
  val mssii_elts_cod: key -> t -> value list      
end


(*  FIXME probably have to restrict the type t in the following so that keys and valu types are explicit, and impl type is abstract *)
module
  Bintree_map_impl(Key_ord: Map.OrderedType)(Value_ord:Map.OrderedType) :
    Mall with type key = Key_ord.t and type value = Value_ord.t
= struct

  type key = Key_ord.t
  type value = Value_ord.t
                 
  module M = Map.Make(Key_ord)
  module S = Set.Make(Value_ord)
  type t = S.t M.t
  let find k m = try M.find k m with _ -> S.empty
  let map_empty () = M.empty
  let map_add_cod k v m = (
    let s = find k m in
    let s' = S.add v s in
    let m' = M.add k s' m in
    m')
  let map_fold_cod k f m b0 = (
    let s = find k m in
    let r = S.fold f s b0 in
    r)
  let map_cod_empty k m = S.is_empty (find k m)
  let map_find_cod k v m = S.mem v (find k m)
  let mssii_elts_cod k m = (find k m) |> S.elements
   
end

module Bintree_set_impl(Elt_ord: Set.OrderedType) = struct

  module S = Set.Make(Elt_ord)
  type elt = Elt_ord.t
  type t = S.t
  let std_empty () = S.empty
  let std_mem x s = S.mem x s
  (* use this in 4.03 after 30bb2c39d8509dc741c0321c700b512820059eb *)
  let std_add x s = (
    (* let present = std_mem x s in *)
    let s' = S.add x s in
    let present' = (s' == s) in 
    (* let _ = assert (present' = present) in *)
    (s',present'))
  (* use this < 4.03 *)
  let std_add' x s = (
    let present = S.mem x s in
    let s' = S.add x s in
    (s',present))
  
end
