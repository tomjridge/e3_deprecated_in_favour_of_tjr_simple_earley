(** Implementation based on hashtables.

    We want to remain parametric in symbols and items, but implement
    sets and maps using hashtables. This assumes that we have some way
    of constructing keys from symbols.

*)

open Core_types

module type Hashkey_t = sig

  module Symbol: Symbol_t
  module Item: Item_t with module Symbol=Symbol


  type hashed_sym_t
  val hash_sym: Symbol.sym -> hashed_sym_t

  type hashed_tm_t
  val hash_tm: Symbol.tm -> hashed_tm_t  

  type hashed_nt_item_t
  val hash_nt_item: Item.nt_item -> hashed_nt_item_t


  type hashed_sym_item_t
  val hash_sym_item: Item.sym_item -> hashed_sym_item_t

  
  type hashed_item_t
  val hash_item: Item.item -> hashed_item_t

  type hashed_sym_list_t
  val hash_sym_list: Item.sym_list -> hashed_sym_list_t
    
end


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



module
  Mk_hashset_impl
    (Symbol: Symbol_t)
    (Item: Item_t with module Symbol=Symbol)
    (Hashkey: Hashkey_t with module Symbol = Symbol and module Item = Item)
= struct

  module Symbol = Symbol
  module Item = Item
  module Hashkey = Hashkey

  
  (* target types *)
  module type Sets_t = (Sets_t with module Item=Item)
  module type Maps_t = (Maps_t with module Symbol=Symbol and module Item=Item)

  
  module Sets: Sets_t with module Item=Item = struct
    module Item = Item

    module Hashed_type = struct
      type t = Item.item
      type t_hashed = Hashkey.hashed_item_t
      let hash : t -> t_hashed = Hashkey.hash_item
    end

    module Set_todo_done = Default_hashset_impl(Hashed_type)        
  end


  module Maps : Maps_t with module Symbol=Symbol and module Item = Item = struct
    module Symbol = Symbol
    module Item = Item

    open Symbol
    open Item


    module Hashed_int_sym = struct 
      type t = int * sym
      type t_hashed = (int * Hashkey.hashed_sym_t)
      let hash : t -> t_hashed = fun (x,y) -> (x,Hashkey.hash_sym y)
    end
    
    module Hashed_nt_item = struct
      type t = nt_item
      type t_hashed = Hashkey.hashed_nt_item_t
      let hash : t -> t_hashed = Hashkey.hash_nt_item
    end

    module Hashed_sym_item = struct
      type t = sym_item
      type t_hashed = Hashkey.hashed_sym_item_t
      let hash : t -> t_hashed = Hashkey.hash_sym_item
    end

    module Hashed_int = struct
      type t = int
      type t_hashed = t
      let hash : t -> t_hashed = fun x -> x
    end

    module Hashed_tm_int = struct 
      type t = tm * int
      type t_hashed = (Hashkey.hashed_tm_t * int)
      let hash : t -> t_hashed = fun (x,y) -> (Hashkey.hash_tm x,y)
    end


    module Hashed_ssii = struct
      type t = sym_list * sym * int * int
      type t_hashed = (Hashkey.hashed_sym_list_t * Hashkey.hashed_sym_t * int * int)
      let hash : t -> t_hashed = fun (x,y,z,w) -> Hashkey.(hash_sym_list x,hash_sym y,z,w)
    end
    
    
    module Map_blocked_key = Default_hashmap_impl(Hashed_int_sym)(Hashed_nt_item)

    module Map_complete_key = Default_hashmap_impl(Hashed_int_sym)(Hashed_sym_item)

    module Map_tm_int = Default_hashmap_impl(Hashed_tm_int)(Hashed_int)

    module Map_sym_sym_int_int = Default_hashmap_impl(Hashed_ssii)(Hashed_int)
    
  end
  
  
end

