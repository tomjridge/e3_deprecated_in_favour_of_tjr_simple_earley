open Substring

type nt

type tm

type sym

type tm_item

type nt_item

type sym_item

type sym_list

type item

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
  nt_items_for_nt: nt -> substring -> nt_item list;
  mk_item        : [`NTITM of nt_item | `TMITM of tm_item ] -> item;
  dest_item      : item -> [`NTITM of nt_item | `TMITM of tm_item ];
  tm_dot_i9      : tm_item -> int;
  sym_dot_i9     : sym_item -> int;
  sym_dot_j9     : sym_item -> int;
  nt_dot_i9      : nt_item -> int;
  nt_dot_j9      : nt_item -> int;
  with_j9        : nt_item -> int -> nt_item;
  p_of_tm        : tm -> substring -> int list;
}

