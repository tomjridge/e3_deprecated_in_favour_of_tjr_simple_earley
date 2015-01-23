(* specialize the types from e3_core_types, so that there are almost no type variables *)
module type TA = sig

  type nt
  type tm
  type sym

  type nt_item (* = pre_nt_item*int*int *)

  type tm_item (* = tm*int *)
  type sym_item (* = sym*int*int *)
  type sym_list

  type item (* = nt_item+tm_item *)

  type set_todo_done

  type map_blocked_key
  type map_complete_key
  type map_sym_sym_int_int
  type map_tm_int

  type 'string ty_ctxt = ('string,'a,'s,'m) E3_core_types.ty_ctxt 
  constraint 'a = <
    nt         :nt         ;
    tm         :tm         ;
    sym        :sym        ;
    tm_item    :tm_item    ;
    sym_item   :sym_item   ;
    sym_list   :sym_list   ;
    nt_item    :nt_item    ;
    item       :item       ;
    string     :'string     ;
  > constraint 's = <
    todo_done: item; (* nt_item+tm_item *)
    set_todo_done: set_todo_done;
  > constraint 'm = <
    sym:sym;
    tm:tm;
    nt_item: nt_item;
    sym_item: sym_item;
    sym_list   :sym_list   ;
    map_blocked_key: map_blocked_key;  
    map_complete_key: map_complete_key;  
    map_sym_sym_int_int: map_sym_sym_int_int;  
    map_tm_int: map_tm_int;  
  >

  type ty_loop2 = 'a E3_core_types.ty_loop2 
    constraint 'a = <
      item: item;
      set_todo_done: set_todo_done;
      map_blocked_key: map_blocked_key;  
      map_complete_key: map_complete_key;  
      map_sym_sym_int_int: map_sym_sym_int_int;  
      map_tm_int: map_tm_int;  
    >

  (*  val x_ctxt: string ty_ctxt' *)

end

(* output type of functor E3 below; the functor E3 specializes some of these types to the input XX types *)
module type TB = sig
  type 'a ty_ctxt
  type ty_loop2
  val earley: 'a ty_ctxt -> ty_loop2 -> ty_loop2
end


module E3 (XX: TA) : TB with type 'a ty_ctxt = 'a XX.ty_ctxt and type ty_loop2 = XX.ty_loop2
