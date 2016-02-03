(* example datastructures for earley, excluding sets and maps *)

open Core_types

module Symbol = struct

  type nt = int 
  type tm = int 
  type sym = [ `NT of nt | `TM of tm ]
  
  let sym_case : sym -> [ `NT of nt | `TM of tm] = fun x -> x

  let sym_of_tm : tm -> sym = fun x -> `TM x
  
end

module Item = struct

  module Symbol = Symbol
  open Symbol
  
  type nt_item = nt * sym list * sym list * int * int
  type tm_item = tm * int
  type sym_item = sym * int * int
  type item = [ `NTITM of nt_item | `TMITM of tm_item ]
  type sym_list = sym list

  (* group all ops here *)
  type ops_t = {
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

  let ops = (
    let id = fun x -> x in
    {
(*      sym_case       =sym_case;
        sym_of_tm      =sym_of_tm; *)
      mk_tm_coord    =id;
      tm5            =(fun (tm,i) -> tm);
      mk_sym_coord   =id;
      sym6           =(fun (sym,i,j) -> sym);
      nt2            =(fun (nt,_,_,_,_) -> `NT nt);
      shift_a2_b2_c2 =(fun (nt,_as,b::bs,i,j) -> (nt,b::_as,bs,i,j));
      (*    a2_length_1    =(fun (nt,_as,_,_,_) -> match _as with [x] -> true | _ -> false); *)
      b2_nil         =(fun (nt,_,bs,_,_) -> match bs with [] -> true | _ -> false);
      (*    hd_a2          =(fun (_,a::_,_,_,_) -> a); *)
      a2             =(fun (_,_as,_,_,_) -> _as);
      hd_b2          =(fun (_,_,b::_,_,_) -> b);
      mk_item        =id;
      dest_item      =id;
      tm_dot_i9      =(fun (tm,i) -> i);
      sym_dot_i9     =(fun (sym,i,j) -> i);
      sym_dot_j9     =(fun (sym,i,j) -> j);
      nt_dot_i9      =(fun (nt,_,_,i,j) -> i);
      nt_dot_j9      =(fun (nt,_,_,i,j) -> j);
      with_j9        =(fun (nt,_as,bs,i,_) -> fun j -> (nt,_as,bs,i,j));
    })

end

module Substring = struct

  type string_t
  type substring = (string_t * int * int)
  
end

module Ctxt = struct

  module Substring = Substring
  module Symbol = Symbol
  module Item = Item

  open Substring
  open Symbol
  open Item

  type input_t = {
    string5: string_t;
    length5: int;
  } 

  type grammar_t = {
    (* the first int is the start of the nt_itm in whose rhs this nt
       occurs; the second int is the index from which we are trying to
       parse an nt *)
    nt_items_for_nt: (nt -> (string_t*int*int) -> nt_item list);
    (* the second int is typically the length of the input *)
    p_of_tm: (tm -> (string_t*int*int) -> int list) 
  }

  type ctxt_t = {
    g0:grammar_t;
    i0:input_t
  }
  
end





module
  Mk_impl
    (Sets: Sets_t with module Item=Item)
    (Maps: Maps_t with module Symbol=Symbol and module Item=Item)
= struct


  module Earley_state = struct

    module Symbol = Symbol
    module Item = Item
    module Sets = Sets
    module Maps = Maps

    open Item
    open Maps
    open Sets

    type ty_loop2 = {
      todo_done5: Set_todo_done.t;
      todo5: item list;
      oracle5: Map_sym_sym_int_int.t;
      (*      tmoracle5: Map_tm_int.t; *)
      blocked5: Map_blocked_key.t;
      complete5: Map_complete_key.t;
    }

  end

  
  module E3 = Core.E3(Symbol)(Item)(Sets)(Maps)(Substring)(Ctxt)(Earley_state)

  let earley : Ctxt.ctxt_t -> Earley_state.ty_loop2 -> Earley_state.ty_loop2 = E3.earley

  (* export for use *)

  include Symbol
  type nt_item = Item.nt_item

  type 'a params = {
    nt_items_for_nt: nt -> ('a*int*int) -> nt_item list;
    p_of_tm: tm -> ('a*int*int) -> int list }

  type oracle_t = (sym list * sym) -> (int * int) -> int list

  (** Parsing also returns information about which extents correspond to terminals. *)
  type tm_oracle_t = tm -> (int * int) -> bool

  (* FIXME we really want to hide the types such as ctxt_t, so the user
     doesn't have to see any details of this module *)
  let run_earley' : Ctxt.ctxt_t -> Symbol.nt -> Earley_state.ty_loop2 = (
    fun c0 nt0 ->
      let open Symbol in
      let open Item in
      let open Earley_state in
      let item0 = `NTITM(nt0,[],[`NT nt0],0,0) |> Item.ops.mk_item in
      let s0 : Earley_state.ty_loop2 = {
        todo_done5=Sets.Set_todo_done.std_empty();
        todo5=[item0];
        oracle5=Maps.Map_sym_sym_int_int.map_empty();
        (*        tmoracle5=Maps.Map_tm_int.map_empty(); *)
        blocked5=Maps.Map_blocked_key.map_empty();
        complete5=Maps.Map_complete_key.map_empty();
      }
      in
      earley c0 s0
  )


  let run_earley: 'a params -> nt -> 'a -> int -> (oracle_t * tm_oracle_t) = (
    fun p0 nt0 s0 i0 -> (
        let open Substring in
        let open Ctxt in
        let open Earley_state in
        let alpha_to_string_t (x:'a) = ((Obj.magic x):Substring.string_t) in
        let string_t_to_alpha (x:string_t) = ((Obj.magic x):'a) in
        let c0 = {
          g0={
            nt_items_for_nt=(fun nt (s,i,j) -> p0.nt_items_for_nt nt (s|>string_t_to_alpha,i,j));
            p_of_tm=(fun tm (s,i,j) -> (
                  p0.p_of_tm tm (s|>string_t_to_alpha,i,j)
                ));
          };
          i0={
            string5=(s0|>alpha_to_string_t);
            length5=i0;
          };
        }
        in
        let s1 : Earley_state.ty_loop2 = run_earley' c0 nt0 in
        let oracle = (fun (syms1,sym2) -> fun (i,j) -> 
            Maps.Map_sym_sym_int_int.mssii_elts_cod (syms1,sym2,i,j) s1.oracle5)
        in
        let tm_oracle = (fun tm -> fun (i,j) ->
            (* FIXME inefficient? *)
            let key = (i,`NT tm) in
            let v = j in
            Maps.Map_complete_key.map_find_cod key v s1.complete5)
        in
        (oracle,tm_oracle)
      )
  )



end
