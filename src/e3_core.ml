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

  type ty_loop2 = 'a E3_core_types.ty_loop2 
    constraint 'a = <
      item: item;
      set_todo_done: set_todo_done;
      map_blocked_key: map_blocked_key;  
      map_complete_key: map_complete_key;  
      map_sym_sym_int_int: map_sym_sym_int_int;  
      map_tm_int: map_tm_int;  
    >

end

(* output type of functor E3 below; the functor E3 specializes some of these types to the input XX types *)
module type TB = sig
  type 'a ty_ctxt
  type ty_loop2
  val earley: 'a ty_ctxt -> ty_loop2 -> ty_loop2
end


module E3 (XX: TA) : TB with type 'a ty_ctxt = 'a XX.ty_ctxt and type ty_loop2 = XX.ty_loop2 = struct

(* for interactive development

module XX : TA = (val (Obj.magic 0) : TA)

*)

  type 'a ty_ctxt = 'a XX.ty_ctxt
  type ty_loop2 = XX.ty_loop2

  open E3_core_types (* for record selectors *)  
  open XX
  (* for this to have the meaning we think it does, we must be working with binarized grammars - see note 2014-01-10; still safe to process if |b2| >=1 *)
  let update_oracle : 'a ty_ctxt -> map_sym_sym_int_int -> nt_item * int -> map_sym_sym_int_int = (fun ctxt m (itm,l) ->
    let ops = ctxt.item_ops5 in
    let (syms1,sym2) = (ops.a2 itm,ops.hd_b2 itm) in
    let (i,k,j) = (ops.nt_dot_i9 itm,ops.nt_dot_j9 itm,l) in
    let key = (syms1,sym2,i,j) in
    let m = ctxt.maps.map_sym_sym_int_int.mssii_add_cod key k m in 
    m)
  
  (*  let update_oracle m (itm,l) = m *)
  
  let update_tmoracle : 'a ty_ctxt -> map_tm_int -> tm*int*int -> map_tm_int = (fun ctxt m (tm,i,j) ->
    let key = (tm,i) in
    let m = ctxt.maps.map_tm_int.mti_add_cod key j m in
    m)
  
  let todo_is_empty s0 = (s0.todo5=[])
  
  let add_todo ctxt s0 itm = { s0 with 
    todo5=(itm::s0.todo5); 
    todo_done5=(ctxt.sets.set_todo_done.std_add itm s0.todo_done5) }
  
  let pop_todo s0 = (
    match s0.todo5 with
    | [] -> (failwith "pop_todo")
    | x::xs -> ({s0 with todo5=xs},x))
  
  (* bitm is an nt_item *)
  (* O(ln n) *)
  let cut: 'a ty_ctxt -> nt_item -> sym_item -> ty_loop2 -> ty_loop2 = (fun ctxt bitm citm s0 ->
    let ops = ctxt.item_ops5 in
    let nitm = ops.mk_item (`NTITM ((ops.shift_a2_b2_c2 bitm) |> (fun x -> ops.with_j9 x (ops.sym_dot_j9 citm)))) in
    let s0 = (
      (* if this could be made O(1) our implementation would be O(n^3) overall *)
      if (ctxt.sets.set_todo_done.std_mem nitm s0.todo_done5) then
        s0
      else
        add_todo ctxt s0 nitm)
    in
    s0)
  
  let loop2: 'a ty_ctxt -> ty_loop2 -> ty_loop2 = (fun ctxt s0 ->
    let maps = ctxt.maps in
    let sets = ctxt.sets in
    let map_complete_key = maps.map_complete_key in
    let map_blocked_key = maps.map_blocked_key in
    let ops = ctxt.item_ops5 in
    let (s0,itm0) = pop_todo s0 in
    (* FIXME add a case construct rather than dests *)
    match ops.dest_item itm0 with
    | `NTITM nitm -> (
        let complete = ops.b2_nil nitm in
        match complete with
        | true -> (
            let citm = ops.mk_sym_coord (ops.nt2 nitm, ops.nt_dot_i9 nitm, ops.nt_dot_j9 nitm) in
            let k = (ops.sym_dot_i9 citm (* FIXME could be from dot_i9 nitm *),ops.sym6 citm) in
            (* FIXME check whether citm has already been done? *)
            (*let citms = map_complete_key.find2 k s0.complete5 in *)
            match false (* sets.set_sym_item.std_mem citm citms *) with (* FIXME this optimization didn't buy much *)
            | true -> s0
            | false -> (
                (* O(n. ln n) *)
                (* process complete item against blocked items *)
                let f1 bitm s1 = cut ctxt bitm citm s1 in
                let s0 = map_blocked_key.mbk_fold_cod k f1 s0.blocked5 s0 in
                (* record complete item *)
                let s0 = {s0 with complete5=(map_complete_key.mck_add_cod k citm s0.complete5)} in
                (* we also update the oracle at this point; FIXME this appears very costly *)
                let f2 bitm s1 = {s1 with oracle5=(update_oracle ctxt s1.oracle5 (bitm,ops.sym_dot_j9 citm))} in
                (* O(n. ln n) *)
                let s0 = map_blocked_key.mbk_fold_cod k f2 s0.blocked5 s0 in
                s0))
        | false -> (
            let bitm = nitm in
            let sym = ops.hd_b2 bitm in
            let k = (ops.nt_dot_j9 bitm,sym) in
            (* let bitms = map_blocked_key.find2 k s0.blocked5 in *)
            let new_key = map_blocked_key.mbk_cod_empty k s0.blocked5 in
            (* record blocked item *)
            let s0 = {s0 with blocked5=(map_blocked_key.mbk_add_cod k bitm s0.blocked5)} in
            (* process blocked item against complete items *)
            (* O(n. ln n) *)
            let f3 citm s1 = cut ctxt bitm citm s1 in
            let s0 = map_complete_key.mck_fold_cod k f3 s0.complete5 s0 in
            (* we also update the oracle at this point; FIXME this appears very costly *)
            (* O(n. ln n) *)
            let f4 citm s1 = {s1 with oracle5=(update_oracle ctxt s1.oracle5 (bitm,ops.sym_dot_j9 citm)) } in        
            let s0 = map_complete_key.mck_fold_cod k f4 s0.complete5 s0 in
            (* the invariant should be: (j,nt) is nonempty iff all
               nt items for j are already in set_todo_done; (j,tm) is
               nonempty iff all tmitems for j are already in
               set_todo_done; FIXME in which case we don't have to check
               whether all these new items are already in set_todo_done
               when new_key - they aren't *)
            if new_key then (
              match ops.sym_case sym with
              | `NT nt -> (
                  let rs = ops.nt_items_for_nt nt (ctxt.string5, ops.nt_dot_i9 nitm, ops.nt_dot_j9 nitm) in
                  let f1 s1 pnitm = (
                    let nitm = ops.mk_item (`NTITM pnitm) in
                    if (sets.set_todo_done.std_mem nitm s1.todo_done5) then s1 else
                      add_todo ctxt s1 nitm)
                  in
                  let s0 = List.fold_left f1 s0 rs in
                  s0)
              | `TM tm -> (
                  let titm = ops.mk_item(`TMITM(ops.mk_tm_coord (tm,ops.nt_dot_j9 nitm))) in
                  if (sets.set_todo_done.std_mem titm s0.todo_done5) then s0 else 
                    add_todo ctxt s0 titm))
            else
              s0
  	      ))
    | `TMITM titm -> (
        let tm = ops.tm5 titm in
        let p = ops.p_of_tm tm in
        let i = ops.tm_dot_i9 titm in
        let rs = p (ctxt.string5,i,ctxt.length5) in 
        let sym = ops.sym_of_tm tm in
        let k = (i,sym) in
        (* lots of new complete items, so complete5 must be updated, but we must also process blocked *)
        (* let bitms = map_blocked_key.find2 k s0.blocked5 in *)
        (* update complete set *)
        let f5 s1 j = (
          let citm = ops.mk_sym_coord (sym,i,j) in
          {s1 with complete5=(map_complete_key.mck_add_cod k citm s1.complete5)}) 
        in
        let s0 = List.fold_left f5 s0 rs in
        let f8 s1 j = (
          let i = ops.tm_dot_i9 titm in 
          let citm = ops.mk_sym_coord (sym,i,j) in
          let f6 bitm s1 = cut ctxt bitm citm s1 in
          (* O(n. ln n) *)
          (* let s1 = sets.set_nt_item.fold f1 bitms s1 in *)
          let s1 = map_blocked_key.mbk_fold_cod k f6 s1.blocked5 s1 in
          (* we also update the oracle at this point *)
          let f7 bitm s1 = {s1 with oracle5=(update_oracle ctxt s1.oracle5 (bitm,ops.sym_dot_j9 citm)) } in
          (* O(n. ln n) *)
          (* let s1 = {s1 with oracle5=(sets.set_nt_item.fold f1 bitms s1.oracle5) } in (* FIXME note bitms wasn't used linearly - but that doesn't matter because bitms isn't updated in this loop *) *)
          let s1 = map_blocked_key.mbk_fold_cod k f7 s1.blocked5 s1 in
          (* and the tmoracle *)
          let s1 = {s1 with tmoracle5=(update_tmoracle ctxt s1.tmoracle5 (tm,i,j)) } in
          s1)
        in
        let s0 = List.fold_left f8 s0 rs in
        (* let s0 = {s0 with complete5=(map_complete_key.add k cs s0.complete5)} in *)
        s0))
  
  (* if porting to an imperative language, use a while loop for the following *)
  let rec earley: 'a ty_ctxt -> ty_loop2 -> ty_loop2 = fun ctxt s0 -> (if todo_is_empty s0 then s0 else (earley ctxt (loop2 ctxt s0)))
  
end
