(* a set implemented using hashtables *)

let default_hashtbl_size = 100

(* use a type isomorphic to the original - hashing may be expensive *)
module type Iso_type_t = sig
  type t
  type t_iso

  val iso: t -> t_iso
end


module Basic_hashset_impl(Iso_type:Iso_type_t) = struct

  module Iso_type = Iso_type

  type elt = Iso_type.t
  type t = (Iso_type.t_iso,unit) Hashtbl.t (* set implemented as a map from t_iso to t *)

  let iso = Iso_type.iso
  
  let std_empty : unit -> t = fun () -> Hashtbl.create default_hashtbl_size (* FIXME *)
  let std_add : elt -> t -> t = (
    fun e s -> (
        Hashtbl.replace s (iso e) ();
        s)
  )
  let std_mem : elt -> t -> bool = (
    fun e s -> Hashtbl.mem s (iso e)
  )
  
end  


(* additional useful functions *)
module Default_hashset_impl(Iso_type:Iso_type_t) = struct

  module Iso_type = Iso_type

  type elt = Iso_type.t
  type t = (Iso_type.t_iso,Iso_type.t) Hashtbl.t (* set implemented as a map from t_iso to t *)

  let iso = Iso_type.iso
  
  let std_empty : unit -> t = fun () -> Hashtbl.create default_hashtbl_size (* FIXME *)
  let std_add : elt -> t -> t = (
    fun e s -> (
        Hashtbl.replace s (iso e) e;
        s)
  )
  let std_mem : elt -> t -> bool = (
    fun e s -> Hashtbl.mem s (iso e)
  )

  let fold : t -> (elt -> 'b -> 'b) -> 'b -> 'b = (
    fun s0 f (init:'b) -> (
        Hashtbl.fold
          (fun k v (i:'b) ->
             f v i
          )
          s0
          init
      )
  )

  let is_empty : t -> bool = (fun m0 -> Hashtbl.length m0 = 0)

  let elements : t -> elt list = (
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



module Default_hashmap_impl(K:Iso_type_t)(V:Iso_type_t) = struct

  module K = K
  module V = V

  type key = K.t
  type value = V.t

  module V_set = Default_hashset_impl(V)
  
  type t = (K.t_iso,V_set.t) Hashtbl.t

  let map_empty : unit -> t = fun () -> Hashtbl.create default_hashtbl_size
  let map_find : t -> key -> V_set.t = (
    fun m0 k0 -> (
        let hk = K.iso k0 in
        let s =
          try
            Hashtbl.find m0 hk
          with _ -> (
              let s = V_set.std_empty () in              
              let () = Hashtbl.replace m0 hk s in
              s
            )
        in
        s
      )
  )
  let map_add_cod: key -> value -> t -> t = (
    fun k v m -> 
      let vs = map_find m k in
      let _ = V_set.std_add v vs in  (* imperative, so we don't have to reinsert into the map *)
      m
  )

  let map_fold_cod: key -> (value -> 'b -> 'b) -> t -> 'b -> 'b = (
    fun k f m0 (init:'b) -> (
        let vs = map_find m0 k in
        V_set.fold vs f init
      )                       
  )

  let map_cod_empty: key -> t -> bool = (
    fun k m0 -> (
        let vs = map_find m0 k in
        V_set.is_empty vs
      )
  )


  let map_find_cod: key -> value -> t -> bool = (
    fun k v m0 -> (
        let vs = map_find m0 k in
        V_set.std_mem v vs
      )
  )

  let mssii_elts_cod: key -> t -> value list = (
    fun k m0 -> (
        let vs = map_find m0 k in
        V_set.elements vs
      )                
  )

  
end

