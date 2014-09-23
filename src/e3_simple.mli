(** A simple interface to Earley. The implementation is very
    inefficient - this is only for demonstration purposes. DO NOT USE
    IN PRODUCTION CODE! *)


(** Nonterminals must be even integers *)
type nt = int

(** Terminals must be odd integers *)
type tm = int

(** Symbols are either nts or tms *)
type sym = int

(** The parameters for Earley are the grammar (a list of rules), and a
    function {p_of_tm} which takes a terminal and a substring, and returns
    the prefixes (represented as an index) of the substrings that can be
    parsed by that terminal. *)
type 'a params = {
  grammar: (nt*sym list) list;
  p_of_tm: tm -> ('a*int*int) -> int list }

(** The result of parsing is an oracle which, given a list of symbols
    alpha, and a symbol X, and a span (i,j), returns the list of integers
    k such that (i,k) can be parsed as alpha, and (k,j) can be parsed as
    X. *)
type ty_oracle = (sym list * sym) -> (int * int) -> int list

val earley: 'a params -> nt -> 'a -> int -> ty_oracle
