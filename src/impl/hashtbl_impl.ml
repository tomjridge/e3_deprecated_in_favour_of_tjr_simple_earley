(*

    We want to remain parametric in symbols and items, but implement
    sets and maps using hashtables. This assumes that we have some way
    of hashing symbols, items etc.

*)

open Core_types

module type Iso_params_t = sig

  module Symbol: Symbol_t
  module Item: Item_t with module Symbol=Symbol


  type iso_sym_t
  val sym_to_iso: Symbol.sym -> iso_sym_t

  type iso_tm_t
  val tm_to_iso: Symbol.tm -> iso_tm_t  

  type iso_nt_item_t
  val nt_item_to_iso: Item.nt_item -> iso_nt_item_t


  type iso_sym_item_t
  val sym_item_to_iso: Item.sym_item -> iso_sym_item_t

  
  type iso_item_t
  val item_to_iso: Item.item -> iso_item_t

  type iso_sym_list_t
  val sym_list_to_iso: Item.sym_list -> iso_sym_list_t
    
end

open Hashed_sets_and_maps


module
  Mk_hashed_impl
    (Symbol: Symbol_t)
    (Item: Item_t with module Symbol=Symbol)
    (Iso_params: Iso_params_t with module Symbol = Symbol and module Item = Item)
= struct

  module Symbol = Symbol
  module Item = Item
  module Iso_params = Iso_params

  
  (* target types *)
  module type Sets_t = (Sets_t with module Item=Item)
  module type Maps_t = (Maps_t with module Symbol=Symbol and module Item=Item)

  
  module Sets: Sets_t = struct
    module Item = Item

    module Iso_type = struct
      type t = Item.item
      type t_iso = Iso_params.iso_item_t
      let iso : t -> t_iso = Iso_params.item_to_iso
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
      type t_iso = (int * Iso_params.iso_sym_t)
      let iso : t -> t_iso = fun (x,y) -> (x,Iso_params.sym_to_iso y)
    end
    
    module Iso_nt_item = struct
      type t = nt_item
      type t_iso = Iso_params.iso_nt_item_t
      let iso : t -> t_iso = Iso_params.nt_item_to_iso
    end

    module Iso_sym_item = struct
      type t = sym_item
      type t_iso = Iso_params.iso_sym_item_t
      let iso : t -> t_iso = Iso_params.sym_item_to_iso
    end

    module Iso_int = struct
      type t = int
      type t_iso = t
      let iso : t -> t_iso = fun x -> x
    end

    (*
    module Iso_tm_int = struct 
      type t = tm * int
      type t_iso = (Iso_params.iso_tm_t * int)
      let iso : t -> t_iso = fun (x,y) -> (Iso_params.tm_to_iso x,y)
    end
    *)

    module Iso_ssii = struct
      type t = sym_list * sym * int * int 
      type t_iso = (Iso_params.iso_sym_list_t * Iso_params.iso_sym_t * int * int)
      (*      type t_iso = (int * int * (Iso_params.iso_sym_t * Iso_params.iso_sym_list_t)) *)
      let iso : t -> t_iso = fun (x,y,z,w) -> Iso_params.(sym_list_to_iso x,sym_to_iso y,z,w)
      (* let iso : t -> t_iso = fun (x,y,z,w) -> Iso_params.(z,w,(sym_to_iso y,sym_list_to_iso x)) *)
    end
    
    
    module Map_blocked_key = Default_hashmap_impl(Iso_int_sym)(Iso_nt_item)

    module Map_complete_key = Default_hashmap_impl(Iso_int_sym)(Iso_int)

    (* module Map_tm_int = Default_hashmap_impl(Iso_tm_int)(Iso_int) *)

    module Map_sym_sym_int_int = Default_hashmap_impl(Iso_ssii)(Iso_int)
    
  end
  
  
end



(* the impl *)

module Iso_params = struct

  module Symbol = Common_impl.Symbol
  module Item = Common_impl.Item

  open Symbol
  open Item
  
  let id = (fun x -> x)
  
  type iso_sym_t = sym
  let sym_to_iso = id

  type iso_tm_t = tm
  let tm_to_iso = id

  type iso_nt_item_t = nt_item
  let nt_item_to_iso = id

  type iso_sym_item_t = sym_item
  let sym_item_to_iso = id
    
  type iso_item_t = item
  let item_to_iso = id

  type iso_sym_list_t = sym_list
  let sym_list_to_iso = id
  
end

module Hashed_impl = Mk_hashed_impl(Common_impl.Symbol)(Common_impl.Item)(Iso_params)

module Sets = Hashed_impl.Sets
module Maps = Hashed_impl.Maps

module Impl = Common_impl.Mk_impl(Sets)(Maps)
include Impl

