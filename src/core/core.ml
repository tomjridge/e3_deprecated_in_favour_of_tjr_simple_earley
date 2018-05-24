(** Earley core code. *)

open Core_types



module
  E3
    (Symbol: Symbol_t)
    (Item: Item_t with module Symbol=Symbol)
    (Sets: Sets_t with module Item=Item)
    (Maps: Maps_t with module Symbol=Symbol and module Item=Item)
    (Substring: Substring_t) 
    (Ctxt: Ctxt_t with module Substring=Substring and module Symbol=Symbol and module Item=Item)
    (Earley_state: Earley_state_t with module Symbol=Symbol and module Item=Item and module Sets=Sets and module Maps=Maps)

= struct

  module Symbol=Symbol
  module Item=Item
  module Sets=Sets
  module Maps=Maps

  open Symbol
  open Item
  open Maps
  open Sets
  open Substring
  open Ctxt
  open Earley_state

  
  let todo_is_empty s0 = (s0.todo5=[])

  let unsafe_add_todo s0 itm = (
    let td = s0.todo_done5 in
    let (td',itm_already_present) = Set_todo_done.std_add itm td in
    (*    let _ = assert (not itm_already_present) in *)
    { s0 with
      todo5=(itm::s0.todo5);
      todo_done5=td' }
  )
  
  (* doc:rcf *)
  let add_todo s0 itm = (
    let td = s0.todo_done5 in
    (* O(ln n) *)
    (* let present = Set_todo_done.std_mem itm td in *)
    let (td',itm_already_present) = Set_todo_done.std_add itm td in
    (* let _ = assert (present = itm_already_present) in *)
    { s0 with 
      todo5=(if itm_already_present then s0.todo5 else itm::s0.todo5); 
      todo_done5=td' })
  
  
  let pop_todo s0 = (
    match s0.todo5 with
    | [] -> (failwith "pop_todo")
    | x::xs -> ({s0 with todo5=xs},x))


  let new_nt_item: nt_item -> int -> nt_item = (
    fun bitm j -> (
        (ops.shift_a2_b2_c2 bitm) |> (fun x -> ops.with_j9 x j)
      ))

  (* doc:abc *)
  let update_oracle:
    Map_sym_sym_int_int.t -> nt_item * int -> Map_sym_sym_int_int.t = (
    fun m (itm,j) ->
      let (syms1,sym2) = (ops.a2 itm,ops.hd_b2 itm) in
      let (i,k) = (ops.nt_dot_i9 itm,ops.nt_dot_j9 itm) in
      let key = (syms1,sym2,i,j) in
      let m = Maps.Map_sym_sym_int_int.map_add_cod key k m in 
      m)
  
  (* doc:4s4 *)
  let cut: nt_item -> int -> ty_loop2 -> ty_loop2 = (
    fun bitm j s0 ->
      let nitm = ops.mk_item (`NTITM (new_nt_item bitm j)) in
      (* update the oracle *)
      (* O(ln n) with sets implemented as binary trees; if this
         could be made O(1) our implementation would be O(n^3)
         overall *)
      let s0 = { s0 with oracle5=(update_oracle s0.oracle5 (bitm,j)) } in
      add_todo s0 nitm)


  (* doc:ixo *)
  let step: (Ctxt.ctxt_t) -> ty_loop2 -> ty_loop2 = (
    fun c0 ->
      let g0 = c0.g0 in
      let i0 = c0.i0 in
fun s0 ->

let (s0,itm0) = pop_todo s0 in
(* FIXME add a case construct rather than dests *)
match ops.dest_item itm0 with
| `NTITM nitm -> (  (* 10 *)
    let complete = ops.b2_nil nitm in
    match complete with
    | true -> (
        let (citm_sym,k,j) =  (* ie citm *)
          (ops.nt2 nitm, ops.nt_dot_i9 nitm, ops.nt_dot_j9 nitm) in
        let key = (k,citm_sym) in
        (* 12 check whether citm has already been done? no improvement *)
        (* record complete item; FIXME would like to have an
           indication of whether the j was already in the cod when
           adding, since then we don't have to access complete5
           again *)
        let present = Map_complete_key.map_find_cod key j s0.complete5 in
        match present with
        | true -> s0
        | false -> (
            let s0 =
              {s0 with
               complete5=(Map_complete_key.map_add_cod key j s0.complete5)}
            in
            (* 15 O(n. ln n) cut complete item against blocked items *)
            (* FIXME loop with constant j could be optimized *)
            let f1 bitm s1 = cut bitm j s1 in
            let s0 = Map_blocked_key.map_fold_cod key f1 s0.blocked5 s0 in
            s0))
    | false -> (  (* 20 *)
        let bitm = nitm in
        let (bitm_sym,i,k) =
          (ops.hd_b2 bitm, ops.nt_dot_i9 nitm, ops.nt_dot_j9 nitm) in
        let key = (k,bitm_sym) in
        let new_key = Map_blocked_key.map_cod_empty key s0.blocked5 in
        (* 25 record blocked item *)
        let s0 =
          { s0 with
            blocked5=(Map_blocked_key.map_add_cod key bitm s0.blocked5)}
        in
        (* O(n. ln n) process blocked item against complete items *)
        (* FIXME constant key could be optimized; constant bitm could
           be optimized *)
        (* FIXME this parameterization corresponds to pulling out cods
           from maps; perhaps we could add extra operation to pull out a
           cod, and push back; for imperative impl, the push back would
           do nothing of course *)
        let f3 j s1 = cut bitm j s1 in
        let s0 = Map_complete_key.map_fold_cod key f3 s0.complete5 s0 in
        (* 40 *)
        if new_key then (
          match sym_case bitm_sym with
          | `NT nt -> (
              let rs = g0.nt_items_for_nt nt (i0.string5, i, k) in
              let f1 s1 nitm = (
                let itm = ops.mk_item (`NTITM nitm) in
                (* new_key=true, so no need to check todo_done *)
                unsafe_add_todo s1 itm)
              in
              let s0 = List.fold_left f1 s0 rs in
              s0)
          | `TM tm -> (
              let titm = ops.mk_item(`TMITM(ops.mk_tm_coord (tm,k))) in
              (* new_key=true, so no need to check todo_done *)
              unsafe_add_todo s0 titm))
        else
          s0
      ))
| `TMITM titm -> (  (* 50 *)
    let (tm,k) = (ops.tm5 titm,ops.tm_dot_i9 titm) in
    let sym = sym_of_tm tm in
    let p = g0.p_of_tm tm in
    let rs = p (i0.string5,k,i0.length5) in 
    let key = (k,sym) in
    (* 60 update complete items *)
    let f5 s1 j =
      { s1 with complete5=(Map_complete_key.map_add_cod key j s1.complete5)}
    in
    (* FIXME could combine this fold over rs with the following *)
    (* FIXME fold with constant key could be optimized *)
    (* FIXME allow map_add_cod key js? *)
    let s0 = List.fold_left f5 s0 rs in
    (* 70 cut citm against blocked *)
    let f8 s1 j = (
      (* 80 O(n. ln n) process new citm against all blocked items *)
      (* FIXME fold_cod with constant key could be optimized; can we
         do some computation when given key and map? need to know that
         cod of map is constant which we do; perhaps better to
         optimize the underlying map for the case that a key is
         repeatedly used *)
      let f6 bitm s1 = cut bitm j s1 in
      let s1 = Map_blocked_key.map_fold_cod key f6 s1.blocked5 s1 in
      s1)
    in
    let s0 = List.fold_left f8 s0 rs in
    s0))

  (* if porting to an imperative language, use a while loop for the following *)
  let earley: ctxt_t -> ty_loop2 -> ty_loop2 = (
    fun c0 ->
      let stp = step c0 in
      let rec loop s0 = (
        if todo_is_empty s0 then s0 else (loop (stp s0)))
      in
      loop)

end
