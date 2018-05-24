
(** basic set; these ops don't have to store elts *)
module type Basic_set_t = sig

  type elt
  type t
    
  val set_empty: unit -> t

  val set_is_empty: t -> bool

  (* indicate whether the elt was already in the set *)
  val set_add: elt -> t -> (t * bool)
    
  val set_mem: elt -> t -> bool
  
end

(** set *)
module type Set_t = sig
  include Basic_set_t
      
  val set_fold: (elt -> 'b -> 'b) -> t -> 'b -> 'b

  val set_elements: t -> elt list

end


module Bintree_set_impl(Elt_ord: Set.OrderedType)
  :  Set_t with type elt=Elt_ord.t
= struct

  module S = Set.Make(Elt_ord)
  type elt = Elt_ord.t
  type t = S.t
  let set_empty () = S.empty
  let set_mem x s = S.mem x s
  (* use this in 4.03 after 30bb2c39d8509dc741c0321c700b512820059eb *)
  let set_add x s = (
    (* let present = std_mem x s in *)
    let s' = S.add x s in
    let present' = (s' == s) in 
    (* let _ = assert (present' = present) in *)
    (s',present'))
  (* use this < 4.03 *)
  let set_add' x s = (
    let present = S.mem x s in
    let s' = S.add x s in
    (s',present))
  let set_fold f s init = S.fold f s init
  let set_is_empty s = S.is_empty s
  let set_elements s = S.elements s
end





(* maps to values *)
module type Map_t = sig

  type t
  type key
  type value
  val map_empty: unit -> t
  val map_add: key -> value -> t -> t
  val map_find: key -> t -> value option

end


(*  FIXME probably have to restrict the type t in the following so that keys and valu types are explicit, and impl type is abstract *)
module
  Bintree_map_impl(Key_ord: Map.OrderedType)(Value_ord:Map.OrderedType) :
    Map_t with type key = Key_ord.t and type value = Value_ord.t
= struct

  type key = Key_ord.t
  type value = Value_ord.t
                 
  module M = Map.Make(Key_ord)
  type t = value M.t
  let map_empty () = M.empty
  let map_add k v m = (
    let m' = M.add k v m in
    m')
  let map_find k m = (
    try Some(M.find k m) with Not_found -> None
  )
   
end

