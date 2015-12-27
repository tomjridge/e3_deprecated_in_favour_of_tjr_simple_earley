(* the code in e3_core is parametric, but the explosion of type
   variables makes it slightly uncomfortable to work with; the types
   here are the parametric types, and these are fixed in e3_core using
   functors, but the code in e3_core can be pulled out of the functor
   and operate fully parametrically if required *)


module type Substring_t = sig

  type string_t

  type substring (* = Substring of (string_t * int * int) *)

  val mk_substring: string_t * int * int -> substring
  
end


module type Symbol_t = sig

  type nt

  type tm

  type sym

end


module type Item_t = sig

  module Symbol : Symbol_t
  
  open Symbol

  type tm_item

  type nt_item

  type sym_item

  type sym_list

  type item

  type ops_t = {
    sym_case       : sym -> [ `NT of nt | `TM of tm];
    sym_of_tm      : tm -> sym;
    mk_tm_coord    : (tm * int) -> tm_item;
    tm5            : tm_item -> tm;
    mk_sym_coord   : (sym * int * int) -> sym_item;
    sym6           : sym_item -> sym;
    nt2            : nt_item -> sym;
    shift_a2_b2_c2 : nt_item -> nt_item;
    (*  a2_length_1    : nt_item -> bool; *)
    b2_nil         : nt_item -> bool;
    (*  hd_a2          : nt_item -> sym; *)
    a2             : nt_item -> sym_list;
    hd_b2          : nt_item -> sym;
    (*?  nt_items_for_nt: nt -> substring -> nt_item list; *)
    mk_item        : [`NTITM of nt_item | `TMITM of tm_item ] -> item;
    dest_item      : item -> [`NTITM of nt_item | `TMITM of tm_item ];
    tm_dot_i9      : tm_item -> int;
    sym_dot_i9     : sym_item -> int;
    sym_dot_j9     : sym_item -> int;
    nt_dot_i9      : nt_item -> int;
    nt_dot_j9      : nt_item -> int;
    with_j9        : nt_item -> int -> nt_item;
    (*? p_of_tm        : tm -> substring -> int list; *)
  }

  val ops: ops_t
  
end


module type Maps_t = sig

  module Symbol : Symbol_t
  open Symbol

  module Item : Item_t
  open Item
  
  open Map_set_types

  type mbk_key = int * sym
  type mbk_value = nt_item
  module Map_blocked_key :
    (Mbk with type key:=mbk_key and type value:=mbk_value)

  type mck_key = int * sym
  type mck_value = sym_item
  module Map_complete_key :
    (Mck with type key:=mck_key and type value:=mck_value)

  type mssii_key = sym_list * sym * int * int
  type mssii_value = int
  module Map_sym_sym_int_int :
    (Mssii with type key:=mssii_key and type value:=mssii_value)

  type mti_key = tm * int
  type mti_value = int
  module Map_tm_int :
    (Mti with type key:=mti_key and type value:=mti_value)

end


module type Sets_t = sig

  module Item : Item_t

  open Map_set_types

  module Set_todo_done :
    (Set_t with type elt:=Item.item)

end

module type Ctxt_t = sig

  module Sets : Sets_t

  module Maps : Maps_t
  
end


module type Input_t = sig

  module Substring : Substring_t
  open Substring
      
  type input_t = {
    string5: string_t;
    length5: int;
  } 
  
end


module type Grammar_t = sig

  module Substring: Substring_t
  module Symbol : Symbol_t
  module Item : Item_t
  open Substring
  open Symbol
  open Item

  type grammar_t = {
    nt_items_for_nt: (nt -> substring -> nt_item list);
    p_of_tm: (tm -> substring -> int list) 
  }
  
end



module type Earley_state_t = sig

  module Item : Item_t
  module Ctxt : Ctxt_t

  open Item
  open Ctxt.Maps
  open Ctxt.Sets

  type ty_loop2 = {
    todo_done5: Set_todo_done.t;
    todo5: item list;
    oracle5: Map_sym_sym_int_int.t;
    tmoracle5: Map_tm_int.t;
    blocked5: Map_blocked_key.t;
    complete5: Map_complete_key.t;
  }
  
end
