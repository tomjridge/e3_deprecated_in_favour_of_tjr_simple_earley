(*

    We want to remain parametric in symbols and items, but implement
    sets and maps using hashtables. This assumes that we have some way
    of hashing symbols, items etc.

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

open Hashed_sets_and_maps


module
  Mk_hashed_impl
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

  
  module Sets: Sets_t = struct
    module Item = Item

    module Iso_type = struct
      type t = Item.item
      type t_iso = Hashkey.hashed_item_t
      let iso : t -> t_iso = Hashkey.hash_item
    end

    module Set_todo_done = Basic_hashset_impl(Iso_type)        
  end


  module Maps : Maps_t = struct
    module Symbol = Symbol
    module Item = Item

    open Symbol
    open Item


    module Iso_int_sym = struct 
      type t = int * sym
      type t_iso = (int * Hashkey.hashed_sym_t)
      let iso : t -> t_iso = fun (x,y) -> (x,Hashkey.hash_sym y)
    end
    
    module Iso_nt_item = struct
      type t = nt_item
      type t_iso = Hashkey.hashed_nt_item_t
      let iso : t -> t_iso = Hashkey.hash_nt_item
    end

    module Iso_sym_item = struct
      type t = sym_item
      type t_iso = Hashkey.hashed_sym_item_t
      let iso : t -> t_iso = Hashkey.hash_sym_item
    end

    module Iso_int = struct
      type t = int
      type t_iso = t
      let iso : t -> t_iso = fun x -> x
    end

    module Iso_tm_int = struct 
      type t = tm * int
      type t_iso = (Hashkey.hashed_tm_t * int)
      let iso : t -> t_iso = fun (x,y) -> (Hashkey.hash_tm x,y)
    end


    module Iso_ssii = struct
      type t = sym_list * sym * int * int
      type t_iso = (Hashkey.hashed_sym_list_t * Hashkey.hashed_sym_t * int * int)
      let iso : t -> t_iso = fun (x,y,z,w) -> Hashkey.(hash_sym_list x,hash_sym y,z,w)
    end
    
    
    module Map_blocked_key = Default_hashmap_impl(Iso_int_sym)(Iso_nt_item)

    module Map_complete_key = Default_hashmap_impl(Iso_int_sym)(Iso_sym_item)

    module Map_tm_int = Default_hashmap_impl(Iso_tm_int)(Iso_int)

    module Map_sym_sym_int_int = Default_hashmap_impl(Iso_ssii)(Iso_int)
    
  end
  
  
end



(* the impl *)

module Hashkey = struct

  module Symbol = Common_impl.Symbol
  module Item = Common_impl.Item

  open Symbol
  open Item
  
  let id = (fun x -> x)
  
  type hashed_sym_t = sym
  let hash_sym = id

  type hashed_tm_t = tm
  let hash_tm = id

  type hashed_nt_item_t = nt_item
  let hash_nt_item = id

  type hashed_sym_item_t = sym_item
  let hash_sym_item = id
    
  type hashed_item_t = item
  let hash_item = id

  type hashed_sym_list_t = sym_list
  let hash_sym_list = id
  
end

module Hashed_impl = Mk_hashed_impl(Common_impl.Symbol)(Common_impl.Item)(Hashkey)

module Sets = Hashed_impl.Sets
module Maps = Hashed_impl.Maps

module Impl = Common_impl.Mk_impl(Sets)(Maps)
include Impl

