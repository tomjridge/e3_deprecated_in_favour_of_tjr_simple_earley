(**********************************************************************)
(* example grammar *)

(* we define two terminals; for Earley_int_interface, terminals are
   odd ints *)
let eps = 1
let a1 = 3

(* note that our notion of terminal parsers (a function) is extremely
   general (the most general!); we must return the ints k representing
   the prefixes (i,k) that could be parsed of the input (i,j) *)
let parse_eps = (fun (s,i,j) -> if i<=j then [i] else [])

let parse_a1 = (fun (s,i,j) -> 
  if i < j && i < String.length s && String.get s i = '1' then 
    [i+1]
  else
    [])

(* function giving the terminal parsers *)
let p_of_tm = (fun tm -> 
  if tm=eps then parse_eps
  else if tm=a1 then parse_a1
  else failwith "p_of_tm: p8t")

(* define a nonterminal; for Earley_int_interface, nonterminals are
   even ints *)
let e = 2

(* example grammar: E -> E E E | "1" | eps *)
let g = [
  (e,[e;e;e]);
  (e,[a1]);
  (e,[eps])]




(**********************************************************************)
(* process grammar and input with earley *)

let run_earley_string txt = (
  let open E3_simple in
  let params = { grammar=g; p_of_tm=p_of_tm } in
  E3_simple.earley 
    params
    e
    txt 
    (String.length txt))

let txt = "11111"

let o = run_earley_string txt

(* check the type of o *)
let _ = 
  let open E3_simple in
  let (_ : sym list * sym -> int * int -> int list) = o in
  ()

(* if we want to cut [E E] and E between positions 0 and 5, where do
   we cut? N.B. the list is in reversed order *)
let _ = (
  let rs = o ([e;e],e) (0,5) in
  let rs = List.sort Pervasives.compare rs in 
  assert([0;1;2;3;4;5] = rs))

(* How fast is the earley parser? In a top-level, the following
   returns in about 1s. Compiled this whole file takes about 0.3 s. *)
let o = run_earley_string "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"
