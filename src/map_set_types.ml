
(* basic set *)
module type Set_t = sig

  type elt
  type t
  val std_empty: unit -> t
  val std_add: elt -> t -> t
  val std_mem: elt -> t -> bool
  
end


(* maps to sets of values *)
module type Map_t = sig

  type t
  type key
  type value
  val map_empty: unit -> t
  val map_add_cod: key -> value -> t -> t

end


module type Mfc = sig
  include Map_t
  val map_fold_cod: key -> (value -> 'b -> 'b) -> t -> 'b -> 'b
end

module type Mce = sig
  include Map_t
  val map_cod_empty: key -> t -> bool
end

module type Mbk = sig
  include Mfc
  (* FIXME following - should use destructive update with subsignature? *)
  include Mce with type t:=t and type key := key and type value:=value
end

module type Mck = sig include Mfc end

module type Mti = sig
  include Map_t
  val map_find_cod: key -> value -> t -> bool
end

module type Mssii = sig
  include Map_t
  val mssii_elts_cod: key -> t -> value list
end

module Default_map_impl(Key_ord: Map.OrderedType)(Value_ord:Map.OrderedType) = struct

  type key = Key_ord.t
  type value = Value_ord.t
                 
  module M = Map.Make(Key_ord)
  module S = Set.Make(Value_ord)
  type t = S.t M.t
  let find k m = try M.find k m with _ -> S.empty
  let map_empty () = M.empty
  let map_add_cod k v m = (
    let s = find k m in
    let s' = S.add v s in
    let m' = M.add k s' m in
    m')
  let map_fold_cod k f m b0 = (
    let s = find k m in
    let r = S.fold f s b0 in
    r)
  let map_cod_empty k m = S.is_empty (find k m)
  let map_find_cod k v m = S.mem v (find k m)
  let mssii_elts_cod k m = (find k m) |> S.elements
   
end

module Tmp_ko : Map.OrderedType = struct
  type t = int
  let compare = (Pervasives.compare:int -> int -> int)
end

module X: Mssii = Default_map_impl(Tmp_ko)(Tmp_ko)
