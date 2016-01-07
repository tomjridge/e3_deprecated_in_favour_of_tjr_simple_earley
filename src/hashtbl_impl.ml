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


  type hashed_item_t
  val hash_item: Item.item -> hashed_item_t
    
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
  type t = (Hashed_type.t_hashed,unit) Hashtbl.t (* set implemented as a map from t_hashed to () *)

  let hash = Hashed_type.hash
  
  let std_empty : unit -> t = fun () -> Hashtbl.create 17 (* FIXME *)
  let std_add : elt -> t -> t = (
    fun e s -> (
        Hashtbl.replace s (hash e) ();
        s)
  )
  let std_mem : elt -> t -> bool = (
    fun e s -> Hashtbl.mem s (hash e)
  )

end



module Default_hashmap_impl(K:Hashed_type_t)(V:Hashed_type_t) = struct

  module K = K
  module V = V

  type key = K.t
  type value = V.t
    
  type t = (K.t_hashed,V.t_hashed) Hashtbl.t

  let map_empty : unit -> t = fun () -> Hashtbl.create 17
  let map_add_cod: 
  
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


  module Maps (*: Maps_t with module Symbol=Symbol and module Item = Item*) = struct
    module Symbol = Symbol
    module Item = Item

    open Symbol
    open Item

    
    module Map_blocked_key = struct 
      module HT1 = struct
        type t = int * sym
        type t_hashed = (int * Hashkey.hashed_sym_t)
        let hash : t -> t_hashed = fun (x,y) -> (x,Hashkey.hash_sym y)
      end

      
      
    end
    

  end
  
  
end

