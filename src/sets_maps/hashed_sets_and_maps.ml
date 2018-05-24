(* a set implemented using hashtables *)

let default_hashtbl_size = 100

module type T_t = sig
  type t
end

(* use a type isomorphic to the original - hashing may be expensive *)
module type Iso_type_t = sig
  type t
  type t_iso

  val iso: t -> t_iso
end


module Basic_hashset_impl(Iso_type:Iso_type_t)
  : Set_map_types.Basic_set_t with type elt=Iso_type.t
= struct

  module Iso_type = Iso_type

  type elt = Iso_type.t
  type t = (Iso_type.t_iso,unit) Hashtbl.t (* set implemented as a map from t_iso to unit *)

  let iso = Iso_type.iso
  
  let set_empty : unit -> t = fun () -> Hashtbl.create default_hashtbl_size
  let set_is_empty s = (Hashtbl.length s = 0)
  let set_add : elt -> t -> (t * bool) = (
    fun e s -> (
        let before = Hashtbl.length s in
        let _ = Hashtbl.replace s (iso e) () in
        let after = Hashtbl.length s in
        (s,before==after)) (* check whether elt was already in the set *)
  )
  let set_mem : elt -> t -> bool = (
    fun e s -> Hashtbl.mem s (iso e)
  )

  (*
  let set_fold f s init = (
    let f' k v acc = f k acc in
    Hashtbl.fold f' s init
  )
  let set_elements s = (
    let r = ref [] in
    let f k v acc = r:=k::!r in
    let () = Hashtbl.fold f s () in
    !r
  )
     *)
  
end  

(* additional useful functions *)
module Default_hashset_impl(Iso_type:Iso_type_t)
  : Set_map_types.Basic_set_t with type elt=Iso_type.t
= struct

  module Iso_type = Iso_type

  type elt = Iso_type.t
  type t = (Iso_type.t_iso,Iso_type.t) Hashtbl.t (* set implemented as a map from t_iso to t *)

  let iso = Iso_type.iso
  
  let set_empty : unit -> t = fun () -> Hashtbl.create default_hashtbl_size

  let set_is_empty s = (Hashtbl.length s = 0)

  let set_add : elt -> t -> (t * bool) = (
    fun e s -> (
        let before = Hashtbl.length s in
        let _ = Hashtbl.replace s (iso e) e in
        let after = Hashtbl.length s in
        (s,before=after) (* check whether elt was already in the set *)
      )
  )

  let set_mem : elt -> t -> bool = (
    fun e s -> Hashtbl.mem s (iso e)
  )

  let set_fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b = (
    fun f s0 (init:'b) -> (
        Hashtbl.fold
          (fun k v (i:'b) ->
             f v i
          )
          s0
          init
      )
  )

  let set_elements : t -> elt list = (
    fun s0 -> (
        let elts = ref [] in
        let _ =
          Hashtbl.iter
            (fun k v -> (
                 (elts:=v::!elts);
                 ()
               )
            )
            s0
        in
        !elts
      )
  )
  
end


module Default_hashmap_impl(K:Iso_type_t)(V:T_t)
  : Set_map_types.Map_t with type key = K.t and type value = V.t
= struct

  module K = K

  type key = K.t
  type value = V.t

  type t = (K.t_iso,value) Hashtbl.t

  let map_empty : unit -> t = (
    fun () -> Hashtbl.create default_hashtbl_size)

  let map_add : key -> value -> t -> t = (
    fun k v m0 -> (
        let hk = K.iso k in (
          Hashtbl.replace m0 hk v;
          m0)
      )
  )
  
  let map_find : key -> t -> value option = (
    fun k0 m0 -> (
        let hk = K.iso k0 in
        let s = (
          try Some(Hashtbl.find m0 hk) with _ -> None)
        in
        s
      )
  )

  
end
