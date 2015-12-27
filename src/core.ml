(** Earley code *)

open Core_types



module
  E3
    (Substring: Substring_t) (Symbol: Symbol_t)
    (Item: Item_t with module Symbol=Symbol)
    (Sets: Sets_t with module Item=Item)
    (Maps: Maps_t with module Symbol=Symbol and module Item=Item)
    (Ctxt: Ctxt_t with module Sets=Sets and module Maps=Maps)
    (Input: Input_t with module Substring=Substring)
    (Grammar: Grammar_t with
      module Substring=Substring and module Symbol=Symbol and module Item=Item)
    (Earley_state: Earley_state_t with module Item=Item and module Ctxt=Ctxt)

= struct

  open Substring
  open Symbol
  open Item
  open Ctxt
  open Maps
  open Sets
  open Input
  open Grammar
  open Earley_state

  
  let update_oracle:
    Map_sym_sym_int_int.t -> nt_item * int ->
    Map_sym_sym_int_int.t
    = (fun m (itm,l) ->
        let (syms1,sym2) = (ops.a2 itm,ops.hd_b2 itm) in
        let (i,k,j) = (ops.nt_dot_i9 itm,ops.nt_dot_j9 itm,l) in
        let key = (syms1,sym2,i,j) in
        let m = Maps.Map_sym_sym_int_int.map_add_cod key k m in 
        m)


  let update_tmoracle:
    Map_tm_int.t -> tm*int*int
    -> Map_tm_int.t
    = (fun m (tm,i,j) ->
    let key = (tm,i) in
    let m = Maps.Map_tm_int.map_add_cod key j m in
    m)

  
  let todo_is_empty s0 = (s0.todo5=[])

  let add_todo s0 itm = {
    s0 with 
    todo5=(itm::s0.todo5); 
    todo_done5=(Set_todo_done.std_add itm s0.todo_done5) }
  

  let pop_todo s0 = (
    match s0.todo5 with
    | [] -> (failwith "pop_todo")
    | x::xs -> ({s0 with todo5=xs},x))
  
  (* bitm is an nt_item *)
  (* O(ln n) *)
  let cut:
    nt_item -> sym_item -> ty_loop2
    -> ty_loop2
    = (fun bitm citm s0 ->
        let nitm = ops.mk_item (`NTITM ((ops.shift_a2_b2_c2 bitm) |> (fun x -> ops.with_j9 x (ops.sym_dot_j9 citm)))) in
        let s0 = (
          (* if this could be made O(1) our implementation would be O(n^3) overall *)
          if (Set_todo_done.std_mem nitm s0.todo_done5) then
            s0
          else
            add_todo s0 nitm)
        in
        s0)


  (* perhaps grammar and input should be included in the state? *)
  let loop2:
    (Grammar.grammar_t)
    -> (Input.input_t)
    -> ty_loop2 -> ty_loop2
    = (fun g0 i0 s0 ->
        
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
            let f1 bitm s1 = cut bitm citm s1 in
            let s0 = Map_blocked_key.map_fold_cod k f1 s0.blocked5 s0 in
            (* record complete item *)
            let s0 = {s0 with complete5=(Map_complete_key.map_add_cod k citm s0.complete5)} in
            (* we also update the oracle at this point; FIXME this appears very costly *)
            let f2 bitm s1 = {s1 with oracle5=(update_oracle s1.oracle5 (bitm,ops.sym_dot_j9 citm))} in
            (* O(n. ln n) *)
            let s0 = Map_blocked_key.map_fold_cod k f2 s0.blocked5 s0 in
            s0))
    | false -> (
        let bitm = nitm in
        let sym = ops.hd_b2 bitm in
        let k = (ops.nt_dot_j9 bitm,sym) in
        (* let bitms = map_blocked_key.find2 k s0.blocked5 in *)
        let new_key = Map_blocked_key.map_cod_empty k s0.blocked5 in
        (* record blocked item *)
        let s0 = {s0 with blocked5=(Map_blocked_key.map_add_cod k bitm s0.blocked5)} in
        (* process blocked item against complete items *)
        (* O(n. ln n) *)
        let f3 citm s1 = cut bitm citm s1 in
        let s0 = Map_complete_key.map_fold_cod k f3 s0.complete5 s0 in
        (* we also update the oracle at this point; FIXME this appears very costly *)
        (* O(n. ln n) *)
        let f4 citm s1 = {s1 with oracle5=(update_oracle s1.oracle5 (bitm,ops.sym_dot_j9 citm)) } in        
        let s0 = Map_complete_key.map_fold_cod k f4 s0.complete5 s0 in
        (* the invariant should be: (j,nt) is nonempty iff all
           nt items for j are already in set_todo_done; (j,tm) is
           nonempty iff all tmitems for j are already in
           set_todo_done; FIXME in which case we don't have to check
           whether all these new items are already in set_todo_done
               when new_key - they aren't *)
        if new_key then (
          match ops.sym_case sym with
          | `NT nt -> (
              let rs = g0.nt_items_for_nt nt (mk_substring(i0.string5, ops.nt_dot_i9 nitm, ops.nt_dot_j9 nitm)) in
              let f1 s1 pnitm = (
                let nitm = ops.mk_item (`NTITM pnitm) in
                if (Set_todo_done.std_mem nitm s1.todo_done5) then s1 else
                  add_todo s1 nitm)
              in
              let s0 = List.fold_left f1 s0 rs in
              s0)
          | `TM tm -> (
              let titm = ops.mk_item(`TMITM(ops.mk_tm_coord (tm,ops.nt_dot_j9 nitm))) in
              if (Set_todo_done.std_mem titm s0.todo_done5) then s0 else 
                add_todo s0 titm))
        else
          s0
      ))
| `TMITM titm -> (
    let tm = ops.tm5 titm in
    let p = g0.p_of_tm tm in
    let i = ops.tm_dot_i9 titm in
    let rs = p (mk_substring(i0.string5,i,i0.length5)) in 
    let sym = ops.sym_of_tm tm in
    let k = (i,sym) in
    (* lots of new complete items, so complete5 must be updated, but we must also process blocked *)
    (* let bitms = map_blocked_key.find2 k s0.blocked5 in *)
    (* update complete set *)
    let f5 s1 j = (
      let citm = ops.mk_sym_coord (sym,i,j) in
      {s1 with complete5=(Map_complete_key.map_add_cod k citm s1.complete5)}) 
    in
    let s0 = List.fold_left f5 s0 rs in
    let f8 s1 j = (
      let i = ops.tm_dot_i9 titm in 
      let citm = ops.mk_sym_coord (sym,i,j) in
      let f6 bitm s1 = cut bitm citm s1 in
      (* O(n. ln n) *)
      (* let s1 = sets.set_nt_item.fold f1 bitms s1 in *)
      let s1 = Map_blocked_key.map_fold_cod k f6 s1.blocked5 s1 in
      (* we also update the oracle at this point *)
      let f7 bitm s1 = {s1 with oracle5=(update_oracle s1.oracle5 (bitm,ops.sym_dot_j9 citm)) } in
      (* O(n. ln n) *)
      (* let s1 = {s1 with oracle5=(sets.set_nt_item.fold f1 bitms s1.oracle5) } in (* FIXME note bitms wasn't used linearly - but that doesn't matter because bitms isn't updated in this loop *) *)
      let s1 = Map_blocked_key.map_fold_cod k f7 s1.blocked5 s1 in
      (* and the tmoracle *)
      let s1 = {s1 with tmoracle5=(update_tmoracle s1.tmoracle5 (tm,i,j)) } in
      s1)
    in
    let s0 = List.fold_left f8 s0 rs in
    (* let s0 = {s0 with complete5=(map_complete_key.add k cs s0.complete5)} in *)
    s0))

  
end
