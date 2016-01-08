open Core_types

include Common_impl

module Compare = struct
  
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

end
  

module Sets_maps = struct

  open Set_map_types
  open Compare
      
  module S = Symbol
  module I = Item

  open S
  open I
  
  (* target types *)
  module type Sets_t = (Sets_t with module Item=I)
  module type Maps_t = (Maps_t with module Symbol=S and module Item=I)

  (* orderings *)


(*  
  module I1 : Map.OrderedType with type t=int = struct
    type t = int
    let compare = compare_i
  end

  module II : Map.OrderedType with type t=int * int = struct
    type t = int * int
    let compare = compare_ii
  end

  module III : Map.OrderedType with type t=(int*int*int) = struct
    type t = int * int * int
    let compare = compare_iii
  end

  module Sym_ord : Set.OrderedType with type t=sym = struct
    type t = sym
    let compare : t -> t -> int = Pervasives.compare
  end
*)


  
  module Nt_item_ord : Set.OrderedType with type t=nt_item = struct
    type t = nt_item
    let compare : t -> t -> int = compare_nt_item
  end

  module Item_ord : Set.OrderedType with type t=item = struct
    type t = item
    let compare : t -> t -> int = compare_item
  end
  
  
  module Sets : Sets_t with module Item=I = struct
    module Item = I      
    module Set_todo_done: (Set_t with type elt=item) =
      Default_set_impl(Item_ord) 
  end


  
  module Maps : Maps_t with module Symbol=S and module Item=I = struct

    module Symbol = S
    module Item = I
    open Symbol
    open Item

    type mbk_key = int * sym
    type mbk_value = nt_item    
    module Map_blocked_key = Default_map_impl
        (struct
          type t = mbk_key
          let compare: t -> t -> int = Pervasives.compare
        end)
        (Nt_item_ord)

    type mck_key = int * sym
    type mck_value = sym_item
    module Map_complete_key = Default_map_impl
        (struct
          type t = mck_key
          let compare: t -> t -> int = Pervasives.compare
        end)
        (struct
          type t = mck_value
          let compare: t -> t -> int = Pervasives.compare
        end)


    type mti_key = tm * int
    type mti_value = int
    module Map_tm_int = Default_map_impl
        (struct
          type t = mti_key
          let compare: t -> t -> int = Pervasives.compare
        end)
        (struct
          type t = mti_value
          let compare: t -> t -> int = Pervasives.compare
        end)
        

    type mssii_key = sym_list * sym * int * int
    type mssii_value = int
    module Key_ord : Map.OrderedType with type t=mssii_key = struct
      type t = mssii_key
      let compare : t -> t -> int = Pervasives.compare
    end
    module Map_sym_sym_int_int = Default_map_impl
        (Key_ord)
        (struct
          type t = mssii_value
          let compare: t -> t -> int = Pervasives.compare
        end)
        
        
          
  end
    
end  (* Sets_maps *)

module Sets = Sets_maps.Sets
module Maps = Sets_maps.Maps

module Impl = Common_impl.Mk_impl(Sets)(Maps)

include Impl
