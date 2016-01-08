(* a set implemented using hashtables and a hashing function on elts *)
module type Hashed_type_t = sig
  type t
  type t_hashed

  val hash: t -> t_hashed
end

module Default_hashset_impl(Hashed_type:Hashed_type_t) = struct

  module Hashed_type = Hashed_type

  type elt = Hashed_type.t
  type t = (Hashed_type.t_hashed,Hashed_type.t) Hashtbl.t (* set implemented as a map from t_hashed to t *)

  let hash = Hashed_type.hash
  
  let std_empty : unit -> t = fun () -> Hashtbl.create 17 (* FIXME *)
  let std_add : elt -> t -> t = (
    fun e s -> (
        Hashtbl.replace s (hash e) e;
        s)
  )
  let std_mem : elt -> t -> bool = (
    fun e s -> Hashtbl.mem s (hash e)
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



module Default_hashmap_impl(K:Hashed_type_t)(V:Hashed_type_t) = struct

  module K = K
  module V = V

  type key = K.t
  type value = V.t

  module V_set = Default_hashset_impl(V)
  
  type t = (K.t_hashed,V_set.t) Hashtbl.t

  let map_empty : unit -> t = fun () -> Hashtbl.create 17
  let map_find : t -> key -> V_set.t = (
    fun m0 k0 -> (
        let hk = K.hash k0 in
        let s =
          try
            Hashtbl.find m0 hk
          with _ -> (
              let s = V_set.std_empty () in              
              let _ = Hashtbl.replace m0 hk in
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

