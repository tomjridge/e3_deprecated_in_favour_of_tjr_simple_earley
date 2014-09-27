(** A simple interface to Earley. 

    The implementation is very inefficient - this is only for
    demonstration purposes. DO NOT USE IN PRODUCTION CODE!

    Examples of usage can be found in the modules {E3_examples} and
    {E3_test} (no ocamldoc - consult the source).
*)


(** Nonterminals must be even integers *)
type nt = int

(** Terminals must be odd integers *)
type tm = int

(** Symbols are either nts or tms *)
type sym = int

(** An item, a tuple representing an Earley item of the form E -> alpha.beta,i,j *)
type nt_item = nt * sym list * sym list * int * int

(** The parameters for Earley are the grammar (encoded as a function
    nt_items_for_nt, see {E3_examples} for an example), and a function
    {p_of_tm} which takes a terminal and a substring, and returns the
    prefixes (represented as an index) of the substrings that can be
    parsed by that terminal. *)
type 'a params = {
  nt_items_for_nt: nt -> int -> nt_item list;
  p_of_tm: tm -> ('a*int*int) -> int list }

(** The result of parsing is an oracle which, given a list of symbols
    alpha, and a symbol X, and a span (i,j), returns the list of integers
    k such that (i,k) can be parsed as alpha, and (k,j) can be parsed as
    X. *)
type ty_oracle = (sym list * sym) -> (int * int) -> int list

val earley: 'a params -> nt -> 'a -> int -> ty_oracle
