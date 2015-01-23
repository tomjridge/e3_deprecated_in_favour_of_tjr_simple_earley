(* example datastructures for earley; not efficient *)

module CC = struct
  
  open E3_core_types
  
  type nt = int (* assumed even *)
  type tm = int (* odd *)
  type sym = int
  type nt_item = nt * sym list * sym list * int * int
  type tm_item = tm * int
  type sym_item = sym * int * int
  type item = [ `NTITM of nt_item | `TMITM of tm_item ]
  
  type ty_oracle = (sym list * sym) -> (int * int) -> int list
  
  let mk_ops nt_items_for_nt p_of_tm = (
    let id = fun x -> x in
    {
      sym_case       =(fun x -> if x mod 2 = 0 then `NT x else `TM x);
      sym_of_tm      =id;
      mk_tm_coord    =id;
      tm5            =(fun (tm,i) -> tm);
      mk_sym_coord   =id;
      sym6           =(fun (sym,i,j) -> sym);
      nt2            =(fun (nt,_,_,_,_) -> nt);
      shift_a2_b2_c2 =(fun (nt,_as,b::bs,i,j) -> (nt,b::_as,bs,i,j));
  (*    a2_length_1    =(fun (nt,_as,_,_,_) -> match _as with [x] -> true | _ -> false); *)
      b2_nil         =(fun (nt,_,bs,_,_) -> match bs with [] -> true | _ -> false);
  (*    hd_a2          =(fun (_,a::_,_,_,_) -> a); *)
      a2             =(fun (_,_as,_,_,_) -> _as);
      hd_b2          =(fun (_,_,b::_,_,_) -> b);
      nt_items_for_nt=nt_items_for_nt;
      mk_item        =id;
      dest_item      =id;
      tm_dot_i9      =(fun (tm,i) -> i);
      sym_dot_i9     =(fun (sym,i,j) -> i);
      sym_dot_j9     =(fun (sym,i,j) -> j);
      nt_dot_i9      =(fun (nt,_,_,i,j) -> i);
      nt_dot_j9      =(fun (nt,_,_,i,j) -> j);
      with_j9        =(fun (nt,_as,bs,i,_) -> fun j -> (nt,_as,bs,i,j));
      p_of_tm        =p_of_tm
    })
  
  let compare_i x1 y1 = (x1 - y1)
  
  let compare_ii (x1,x2) (y1,y2) = (
    let x = x1 - y1 in
    if x<>0 then x else
      x2-y2)
    
  let compare_iii (x1,x2,x3) (y1,y2,y3) = (
    let x = x1 - y1 in
    if x<>0 then x else
      let x=x2 - y2 in
      if x<>0 then x else
        x3 - y3)
  
  let compare_iiii (x1,x2,x3,x4) (y1,y2,y3,y4) = (
    let x = x1 - y1 in
    if x<>0 then x else
      let x=x2 - y2 in
      if x<>0 then x else
        let x=x3 - y3 in
        if x<>0 then x else
          x4 - y4)
  
  let compare_nt_item (nt,_as,bs,i,j) (nt',_as',bs',i',j') = (
    let x = compare_iii (nt,i,j) (nt',i',j') in
    if x<>0 then x else
      Pervasives.compare (_as,bs) (_as',bs'))
  
  let compare_item i1 i2 = (
    match (i1,i2) with
    | (`TMITM _, `NTITM _) -> -1
    | (`NTITM _, `TMITM _) -> 1
    | (`TMITM x, `TMITM y) -> (compare_ii x y)
    | (`NTITM x, `NTITM y) -> (compare_nt_item x y))
  
  
  module Sets_maps = (struct
  
    let max_array_size = 1000
  
    (* implementation as a 2D array to hashtbl *)
    let ij_of_item k = (match k with | `NTITM(_,_,_,i,j) -> (i,j) | `TMITM(_,i) -> (i,i))
  
    let set_todo_done n = {
      std_empty=(fun () -> 
        let arr = Array.make_matrix (n+1) (n+1) (Hashtbl.create 0) in
        let _ = 
          for i = 0 to n do
            for j = i to n do
              arr.(i).(j) <- Hashtbl.create n
            done
          done
        in
        arr);
      std_add=(fun (k:item) t -> 
        let (i,j) = ij_of_item k in
        let tbl = t.(i).(j) in
        Hashtbl.replace tbl k (); t);
      std_mem=(fun (k:item) t -> 
        let (i,j) = ij_of_item k in
        let tbl = t.(i).(j) in
        Hashtbl.mem tbl k);
    }
  
    let sets n = { set_todo_done=(set_todo_done n) }
  
    (* implement cod as a hashtbl *)
    let map_blocked_key n = {
      mbk_empty=(fun () -> Hashtbl.create (if n < max_array_size then n else max_array_size));
      mbk_add_cod=(fun k v t -> 
        let h = 
          try Hashtbl.find t k with | Not_found -> 
            let h = Hashtbl.create (if n < max_array_size then n else max_array_size) in
            let _ = Hashtbl.replace t k h in
            h
        in
        let _ = Hashtbl.replace h v () in
        t);
      mbk_fold_cod=(fun k f t acc -> 
        try 
          let h = Hashtbl.find t k in
          Hashtbl.fold (fun k _ acc -> f k acc) h acc
        with | Not_found -> acc);
      mbk_cod_empty=(fun k t ->  
          try
            let h = Hashtbl.find t k in  (* invariant: all cod hashtables are non-empty *)
            false
          with | Not_found -> true);
    }
  
    let map_complete_key n = {
      mck_empty=(fun () -> Hashtbl.create (if n < max_array_size then n else max_array_size));
      mck_add_cod=(fun k v t -> 
        let h = 
          try Hashtbl.find t k with | Not_found -> 
            let h = Hashtbl.create (if n < max_array_size then n else max_array_size) in
            let _ = Hashtbl.replace t k h in
            h
        in
        let _ = Hashtbl.replace h v () in
        t);
      mck_fold_cod=(fun k f t acc -> 
        try 
          let h = Hashtbl.find t k in
          Hashtbl.fold (fun k _ acc -> f k acc) h acc
        with | Not_found -> acc)
    }
  
    let map_tm_int n = {
      mti_empty=(fun () -> Hashtbl.create (if n < max_array_size then n else max_array_size));
      mti_add_cod=(fun k v t -> 
        let h = 
          try Hashtbl.find t k with | Not_found -> 
            let h = Hashtbl.create (if n < max_array_size then n else max_array_size) in
            let _ = Hashtbl.replace t k h in
            h
        in
        let _ = Hashtbl.replace h v () in
        t);
      mti_find_cod=(fun k v t -> 
        try 
          let h = Hashtbl.find t k in
          Hashtbl.mem h v
        with | Not_found -> false)
    }
  
    let map_sym_sym_int_int n = {
      mssii_empty=(fun () -> Hashtbl.create (if n < max_array_size then n else max_array_size));
      mssii_add_cod=(fun k v t -> 
        let h = 
          try Hashtbl.find t k with | Not_found -> 
            let h = Hashtbl.create (if n < max_array_size then n else max_array_size) in
            let _ = Hashtbl.replace t k h in
            h
        in
        let _ = Hashtbl.replace h v () in
        t);
      mssii_elts_cod=(fun k t -> 
        try 
          let h = Hashtbl.find t k in
          let f1 k' v' acc = k'::acc in
          Hashtbl.fold f1 h []
        with | Not_found -> [])
    }
  
    let maps n = {
      map_blocked_key=(map_blocked_key n); 
      map_complete_key=(map_complete_key n); 
      map_sym_sym_int_int=(map_sym_sym_int_int n); 
      map_tm_int=(map_tm_int n); 
    }
  
  end)

  type map_blocked_key = (int*sym, (nt_item, unit) Hashtbl.t) Hashtbl.t
  type map_complete_key = (int*sym, (sym_item,unit) Hashtbl.t) Hashtbl.t
  type map_sym_sym_int_int = (sym list*sym*int*int, (int,unit) Hashtbl.t) Hashtbl.t
  type map_tm_int = (tm * int, (int, unit) Hashtbl.t) Hashtbl.t
  type sym_list = sym list

  type set_todo_done = (item, unit) Hashtbl.t array array

  type ty_loop2 = 'a E3_core_types.ty_loop2 
  constraint 'a = <
      item: item;
      set_todo_done: set_todo_done;
      map_blocked_key: map_blocked_key;  
      map_complete_key: map_complete_key;  
      map_sym_sym_int_int: map_sym_sym_int_int;  
      map_tm_int: map_tm_int;  
    >

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
    todo_done: item; 
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


end (* CC *)


module BB = E3_core.E3(CC)


open E3_core_types
open CC

let mk_ctxt nt_items_for_nt p_of_tm txt len = {
  string5=txt;
  length5=len;
  item_ops5=mk_ops nt_items_for_nt p_of_tm;
  sets=Sets_maps.sets len;
  maps=Sets_maps.maps len }

let mk_init_loop2 ctxt init_items = (
  let sets = ctxt.sets in
  let maps = ctxt.maps in
  let s0 = {
    todo_done5=(
      let f1 = (fun s itm -> sets.set_todo_done.std_add itm s) in
      List.fold_left f1 (sets.set_todo_done.std_empty ()) init_items);
    todo5=(init_items);
    oracle5=maps.map_sym_sym_int_int.mssii_empty ();
    tmoracle5=maps.map_tm_int.mti_empty ();
    blocked5=maps.map_blocked_key.mbk_empty ();
    complete5=maps.map_complete_key.mck_empty ()
  } in
  s0)

let earley' nt_items_for_nt p_of_tm txt len init_items = (
  let ctxt = mk_ctxt nt_items_for_nt p_of_tm txt len in
  let s0 = mk_init_loop2 ctxt init_items in
  (ctxt,BB.earley ctxt s0))

let post_process ctxt s0 = (
  let open E3_core in
  let o = s0.oracle5 in
  let o = fun (syms1,sym2) -> fun (i,j) -> 
    ctxt.maps.map_sym_sym_int_int.mssii_elts_cod (syms1,sym2,i,j) o in
  o)

type nt = CC.nt
type tm = CC.tm
type sym = CC.sym
type nt_item = CC.nt_item
type ty_oracle = CC.ty_oracle

type 'a params = {
  nt_items_for_nt: nt -> ('a*int*int) -> nt_item list;
  p_of_tm: tm -> ('a*int*int) -> int list }
  
let earley params nt txt len = (
  let nt_items_for_nt = params.nt_items_for_nt in
  let p_of_tm = params.p_of_tm in
  let init_items = [`NTITM(nt,[],[nt],0,0)] in
  let (ctxt,s0) = earley' nt_items_for_nt p_of_tm txt len init_items in
  post_process ctxt s0)

let (_:'a params -> nt -> 'a -> int -> ty_oracle) = earley


