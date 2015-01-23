(* the code in e3_core is parametric, but the explosion of type
   variables makes it slightly uncomfortable to work with; the types
   here are the parametric types, and these are fixed in e3_core using
   functors, but the code in e3_core can be pulled out of the functor
   and operate fully parametrically if required *)

type 'a substring = 'a * int * int

type 'a ty_ops = {
  sym_case       : 'sym -> [ `NT of 'nt | `TM of 'tm];
  sym_of_tm      : 'tm -> 'sym;
  mk_tm_coord    : ('tm * int) -> 'tm_item;
  tm5            : 'tm_item -> 'tm;
  mk_sym_coord   : ('sym * int * int) -> 'sym_item;
  sym6           : 'sym_item -> 'sym;
  nt2            : 'nt_item -> 'sym;
  shift_a2_b2_c2 : 'nt_item -> 'nt_item;
(*  a2_length_1    : 'nt_item -> bool; *)
  b2_nil         : 'nt_item -> bool;
(*  hd_a2          : 'nt_item -> 'sym; *)
  a2             : 'nt_item -> 'sym_list;
  hd_b2          : 'nt_item -> 'sym;
  nt_items_for_nt: 'nt -> 'string substring -> 'nt_item list;
  mk_item        : [`NTITM of 'nt_item | `TMITM of 'tm_item ] -> 'item;
  dest_item      : 'item -> [`NTITM of 'nt_item | `TMITM of 'tm_item ];
  tm_dot_i9      : 'tm_item -> int;
  sym_dot_i9     : 'sym_item -> int;
  sym_dot_j9     : 'sym_item -> int;
  nt_dot_i9      : 'nt_item -> int;
  nt_dot_j9      : 'nt_item -> int;
  with_j9        : 'nt_item -> int -> 'nt_item;
  p_of_tm        : 'tm -> 'string substring -> int list;
} constraint 'a = <
  nt         :'nt         ;
  tm         :'tm         ;
  sym        :'sym        ;
  tm_item    :'tm_item    ;
  sym_item   :'sym_item   ;
  sym_list   :'sym_list   ;
  nt_item    :'nt_item    ;
  item       :'item       ;
  string     :'string     ;
>


type 'a std = {
  std_empty: unit -> 't;
  std_add: 'elt -> 't -> 't;
  std_mem: 'elt -> 't -> bool;
} constraint 'a = <
  elt:'elt;
  t: 't
>

type 'a ctxt_set = {
  set_todo_done: <elt:'todo_done; t:'set_todo_done> std;
} constraint 'a = <
  todo_done: 'todo_done; (* nt_item+tm_item *)
  set_todo_done: 'set_todo_done;
>

type 'a mbk = {
  mbk_empty: unit -> 't;
  mbk_add_cod: 'key -> 'value -> 't -> 't;
  mbk_fold_cod: 'b. 'key -> ('value -> 'b -> 'b) -> 't -> 'b -> 'b;
  mbk_cod_empty: 'key -> 't -> bool;
} constraint 'a = <
  mbk_key: 'key;
  mbk_value: 'value;
  t: 't
>

type 'a mck = {
  mck_empty: unit -> 't;
  mck_add_cod: 'key -> 'value -> 't -> 't;
  mck_fold_cod: 'b. 'key -> ('value -> 'b -> 'b) -> 't -> 'b -> 'b;
} constraint 'a = <
  mck_key: 'key;
  mck_value: 'value;
  t: 't
>

type 'a mti = {
  mti_empty: unit -> 't;
  mti_add_cod: 'key -> 'value -> 't -> 't;
  mti_find_cod: 'key -> 'value -> 't -> bool;
} constraint 'a = <
  mti_key: 'key;
  mti_value: 'value;
  t: 't
>

type 'a mssii = {
  mssii_empty: unit -> 't;
  mssii_add_cod: 'key -> 'value -> 't -> 't;
  mssii_elts_cod: 'key -> 't -> 'value list;
} constraint 'a = <
  mssii_key: 'key;
  mssii_value: 'value;
  t: 't
>

type 'a ctxt_map = {
  map_blocked_key: <mbk_key:int*'sym; mbk_value:'nt_item; t:'map_blocked_key> mbk;
  map_complete_key: <mck_key:int*'sym; mck_value:'sym_item; t:'map_complete_key> mck;
  map_sym_sym_int_int: <mssii_key:'sym_list*'sym*int*int; mssii_value:int; t:'map_sym_sym_int_int> mssii;
  map_tm_int: <mti_key:'tm*int; mti_value:int; t:'map_tm_int> mti;
} constraint 'a = <
  sym:'sym;
  tm:'tm;
  nt_item: 'nt_item;
  sym_item: 'sym_item;
  sym_list: 'sym_list;
  map_blocked_key: 'map_blocked_key;  
  map_complete_key: 'map_complete_key;  
  map_sym_sym_int_int: 'map_sym_sym_int_int; 
  map_tm_int: 'map_tm_int;  
>

type ('string,'a,'s,'m) ty_ctxt = {
  string5: 'string;
  length5: int;
  item_ops5: 'a ty_ops;
  sets: 's ctxt_set;
  maps: 'm ctxt_map;
} constraint 'a = <
  nt         :'nt         ;
  tm         :'tm         ;
  sym        :'sym        ;
  tm_item    :'tm_item    ;
  sym_item   :'sym_item   ;
  sym_list   :'sym_list   ;
  nt_item    :'nt_item    ;
  item       :'item       ;
  string     :'string     ;
> constraint 's = <
  todo_done: 'todo_done; 
  set_todo_done: 'set_todo_done;
> constraint 'm = <
  sym:'sym;
  tm:'tm;
  nt_item: 'nt_item;
  sym_item: 'sym_item;
  sym_list: 'sym_list;
  map_blocked_key: 'map_blocked_key;  
  map_complete_key: 'map_complete_key;  
  map_sym_sym_int_int: 'map_sym_sym_int_int;  
  map_tm_int: 'map_tm_int;  
>

type 'a ty_loop2 = {
  todo_done5: 'set_todo_done;
  todo5: 'item list;
  oracle5: 'map_sym_sym_int_int;
  tmoracle5: 'map_tm_int;
  blocked5: 'map_blocked_key;
  complete5: 'map_complete_key;
} constraint 'a = <
  item: 'item;
  set_todo_done: 'set_todo_done;
  map_blocked_key: 'map_blocked_key;  
  map_complete_key: 'map_complete_key;  
  map_sym_sym_int_int: 'map_sym_sym_int_int;  
  map_tm_int: 'map_tm_int;  
>
