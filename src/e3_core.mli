(* specialize the types from e3_core_types, so that there are almost no type variables *)

module type TA = sig

  type 'string nt
  type 'string tm
  type 'string sym

  type 'string nt_item (* = pre_nt_item*int*int *)

  type 'string tm_item (* = tm*int *)
  type 'string sym_item (* = sym*int*int *)
  type 'string sym_list

  type 'string item (* = nt_item+tm_item *)
  
  type 'string set_todo_done

  type 'string map_blocked_key
  type 'string map_complete_key
  type 'string map_sym_sym_int_int
  type 'string map_tm_int

  type 'string ty_ctxt = ('string,'a,'s,'m) E3_core_types.ty_ctxt 
  constraint 'a = <
    nt         :'string nt         ;
    tm         :'string tm         ;
    sym        :'string sym        ;
    tm_item    :'string tm_item    ;
    sym_item   :'string sym_item   ;
    sym_list   :'string sym_list   ;
    nt_item    :'string nt_item    ;
    item       :'string item       ;
    string     :'string     ;
  > constraint 's = <
    todo_done: 'string item; 
    set_todo_done: 'string set_todo_done;
  > constraint 'm = <
    sym:'string sym;
    tm:'string tm;
    nt_item: 'string nt_item;
    sym_item: 'string sym_item;
    sym_list   :'string sym_list   ;
    map_blocked_key: 'string map_blocked_key;  
    map_complete_key: 'string map_complete_key;  
    map_sym_sym_int_int: 'string map_sym_sym_int_int;  
    map_tm_int: 'string map_tm_int;  
  >

  type 'string ty_loop2 = ('string,'a) E3_core_types.ty_loop2 
    constraint 'a = <
      item: 'string item;
      set_todo_done: 'string set_todo_done;
      map_blocked_key: 'string map_blocked_key;  
      map_complete_key: 'string map_complete_key;  
      map_sym_sym_int_int: 'string map_sym_sym_int_int;  
      map_tm_int: 'string map_tm_int;  
    >

end  (* TA *)

(* output type of functor E3 below; the functor E3 specializes some of these types to the input XX types *)
module type TB = sig
  type 'string ty_ctxt
  type 'string ty_loop2
  val earley: 'string ty_ctxt -> 'string ty_loop2 -> 'string ty_loop2
end

module E3 (XX: TA) : TB with type 'a ty_ctxt = 'a XX.ty_ctxt and type 'a ty_loop2 = 'a XX.ty_loop2
