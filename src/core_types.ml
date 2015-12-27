(* the code in e3_core is parametric, but the explosion of type
   variables makes it slightly uncomfortable to work with; the types
   here are the parametric types, and these are fixed in e3_core using
   functors, but the code in e3_core can be pulled out of the functor
   and operate fully parametrically if required *)


module type Substring_t = sig

  type string_t

  type substring (* = Substring of (string_t * int * int) *)

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


module type Ctxt_map = sig

  open Map_set_types

  module Map_blocked_key : Mbk

  module Map_complete_key : Mck

  module Map_sym_sym_int_int : Mssii

  module Map_tm_int : Mti

end


module type Ctxt_set = sig

  open Map_set_types

  module Set_todo_done : Set_t

end


module type Ctxt_t = sig

  module Maps : Ctxt_map
  module Sets : Ctxt_set

end


module type Input_t = sig

  module Substring : Substring_t

  open Substring
      
  type input_t = {
    string5: string_t;
    length5: int;
  } 
  
end


module type Earley_state_type = sig

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

