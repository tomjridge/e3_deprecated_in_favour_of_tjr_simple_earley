(* example datastructures for earley; not efficient *)

open Core_types

module Symbol = struct

  type nt = int (* assumed even *)
  type tm = int (* odd *)
  type sym = int

  let id = fun x -> x
  let sym_case = (fun x -> if x mod 2 = 0 then `NT x else `TM x)
  let sym_of_tm = id
  
end

module Item = struct

  module Symbol = Symbol
  open Symbol
  
  type nt_item = nt * sym list * sym list * int * int
  type tm_item = tm * int
  type sym_item = sym * int * int
  type item = [ `NTITM of nt_item | `TMITM of tm_item ]
  type sym_list = sym list
  
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

  let ops = (
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


module Sets_maps = struct

  open Map_set_types
      
  module S = Symbol
  module I = Item

  open S
  open I
  
  (* target types *)
  module type Sets_t = (Sets_t with module Item=I)
  module type Maps_t = (Maps_t with module Symbol=S and module Item=I)

  (* orderings *)
  module I1 : Map.OrderedType = struct
    type t = int
    let compare = compare_i
  end

  module II : Map.OrderedType = struct
    type t = int * int
    let compare = compare_ii
  end

  module III : Map.OrderedType = struct
    type t = int * int * int
    let compare = compare_iii
  end

  module Nt_item_ord : Set.OrderedType with type t=nt_item = struct
    type t = nt_item
    let compare : t -> t -> int = compare_nt_item
  end

  module Item_ord : Set.OrderedType with type t=item = struct
    type t = item
    let compare : t -> t -> int = compare_item
  end
  
  
  module Sets : Sets_t = struct
    module Item = I
    module Set_todo_done: (Set_t with type elt:=item) =
      Default_set_impl(Item_ord) 
  end


  
  module Maps : Maps_t = struct

    module Symbol = S
    module Item = I
    
    type mbk_key = int * sym
    type mbk_value = nt_item    
    module Map_blocked_key = Default_map_impl(II)(Nt_item_ord)

    type mck_key = int * sym
    type mck_value = sym_item
    module Map_complete_key = Default_map_impl(II)(III)

    type mti_key = tm * int
    type mti_value = int
    module Map_tm_int = Default_map_impl(II)(I1)

    type mssii_key = sym_list * sym * int * int
    type mssii_value = int
    module Key_ord : Map.OrderedType with type t=mssii_key = struct
      type t = mssii_key
      let compare : t -> t -> int = Pervasives.compare
    end
    module Map_sym_sym_int_int = Default_map_impl(Key_ord)(I1)
          
  end
  
  

  
end
