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

let txt = "1111"
let len = String.length txt

let o = run_earley_string txt

let asbss = [
  ([],[e;e;e]);
  ([e],[e;e]);
  ([e;e],[e]);
  ([],[a1]);
  ([],[eps])]

let string_of_sym s = (
  if s = e then "E" 
  else if s = a1 then "1"
  else if s = eps then "#"
  else failwith "string_of_sym")

let string_of_syms syms = syms |> List.map string_of_sym |> String.concat ""

(* upto' i j = [i+1..j-1] *)
let rec upto' i j = (if i+1<j then (i+1)::(upto' (i+1) j) else [])

(* upto i j = [i..j] *)
let rec upto i j = (if i<=j then i::(upto (i+1) j) else [])

let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b)

let rec allpairs f l1 l2 =
  match l1 with
   h1::t1 ->  itlist (fun x a -> f h1 x :: a) l2 (allpairs f t1 l2)
   | [] -> []

let ijs = allpairs (fun x y -> x,y) (upto 0 len) (upto 0 len)
  
let args = allpairs (fun x y -> x,y) asbss ijs

let f1 ((_as,bs),(i,j)) = (List.map string_of_sym _as, string_of_sym (List.hd bs), i,j, o (_as,List.hd bs) (i,j) |> List.sort Pervasives.compare)

let rs = List.map f1 args |> List.sort Pervasives.compare

let _ = assert(rs =   [([], "#", 0, 0, [0]); ([], "#", 0, 1, []); ([], "#", 0, 2, []);
   ([], "#", 0, 3, []); ([], "#", 0, 4, []); ([], "#", 1, 0, []);
   ([], "#", 1, 1, [1]); ([], "#", 1, 2, []); ([], "#", 1, 3, []);
   ([], "#", 1, 4, []); ([], "#", 2, 0, []); ([], "#", 2, 1, []);
   ([], "#", 2, 2, [2]); ([], "#", 2, 3, []); ([], "#", 2, 4, []);
   ([], "#", 3, 0, []); ([], "#", 3, 1, []); ([], "#", 3, 2, []);
   ([], "#", 3, 3, [3]); ([], "#", 3, 4, []); ([], "#", 4, 0, []);
   ([], "#", 4, 1, []); ([], "#", 4, 2, []); ([], "#", 4, 3, []);
   ([], "#", 4, 4, [4]); ([], "1", 0, 0, []); ([], "1", 0, 1, [0]);
   ([], "1", 0, 2, []); ([], "1", 0, 3, []); ([], "1", 0, 4, []);
   ([], "1", 1, 0, []); ([], "1", 1, 1, []); ([], "1", 1, 2, [1]);
   ([], "1", 1, 3, []); ([], "1", 1, 4, []); ([], "1", 2, 0, []);
   ([], "1", 2, 1, []); ([], "1", 2, 2, []); ([], "1", 2, 3, [2]);
   ([], "1", 2, 4, []); ([], "1", 3, 0, []); ([], "1", 3, 1, []);
   ([], "1", 3, 2, []); ([], "1", 3, 3, []); ([], "1", 3, 4, [3]);
   ([], "1", 4, 0, []); ([], "1", 4, 1, []); ([], "1", 4, 2, []);
   ([], "1", 4, 3, []); ([], "1", 4, 4, []); ([], "E", 0, 0, [0]);
   ([], "E", 0, 1, [0]); ([], "E", 0, 2, [0]); ([], "E", 0, 3, [0]);
   ([], "E", 0, 4, [0]); ([], "E", 1, 0, []); ([], "E", 1, 1, [1]);
   ([], "E", 1, 2, [1]); ([], "E", 1, 3, [1]); ([], "E", 1, 4, [1]);
   ([], "E", 2, 0, []); ([], "E", 2, 1, []); ([], "E", 2, 2, [2]);
   ([], "E", 2, 3, [2]); ([], "E", 2, 4, [2]); ([], "E", 3, 0, []);
   ([], "E", 3, 1, []); ([], "E", 3, 2, []); ([], "E", 3, 3, [3]);
   ([], "E", 3, 4, [3]); ([], "E", 4, 0, []); ([], "E", 4, 1, []);
   ([], "E", 4, 2, []); ([], "E", 4, 3, []); ([], "E", 4, 4, [4]);
   (["E"], "E", 0, 0, [0]); (["E"], "E", 0, 1, [0; 1]);
   (["E"], "E", 0, 2, [0; 1; 2]); (["E"], "E", 0, 3, [0; 1; 2; 3]);
   (["E"], "E", 0, 4, [0; 1; 2; 3; 4]); (["E"], "E", 1, 0, []);
   (["E"], "E", 1, 1, [1]); (["E"], "E", 1, 2, [1; 2]);
   (["E"], "E", 1, 3, [1; 2; 3]); (["E"], "E", 1, 4, [1; 2; 3; 4]);
   (["E"], "E", 2, 0, []); (["E"], "E", 2, 1, []); (["E"], "E", 2, 2, [2]);
   (["E"], "E", 2, 3, [2; 3]); (["E"], "E", 2, 4, [2; 3; 4]);
   (["E"], "E", 3, 0, []); (["E"], "E", 3, 1, []); (["E"], "E", 3, 2, []);
   (["E"], "E", 3, 3, [3]); (["E"], "E", 3, 4, [3; 4]);
   (["E"], "E", 4, 0, []); (["E"], "E", 4, 1, []); (["E"], "E", 4, 2, []);
   (["E"], "E", 4, 3, []); (["E"], "E", 4, 4, [4]);
   (["E"; "E"], "E", 0, 0, [0]); (["E"; "E"], "E", 0, 1, [0; 1]);
   (["E"; "E"], "E", 0, 2, [0; 1; 2]); (["E"; "E"], "E", 0, 3, [0; 1; 2; 3]);
   (["E"; "E"], "E", 0, 4, [0; 1; 2; 3; 4]); (["E"; "E"], "E", 1, 0, []);
   (["E"; "E"], "E", 1, 1, [1]); (["E"; "E"], "E", 1, 2, [1; 2]);
   (["E"; "E"], "E", 1, 3, [1; 2; 3]); (["E"; "E"], "E", 1, 4, [1; 2; 3; 4]);
   (["E"; "E"], "E", 2, 0, []); (["E"; "E"], "E", 2, 1, []);
   (["E"; "E"], "E", 2, 2, [2]); (["E"; "E"], "E", 2, 3, [2; 3]);
   (["E"; "E"], "E", 2, 4, [2; 3; 4]); (["E"; "E"], "E", 3, 0, []);
   (["E"; "E"], "E", 3, 1, []); (["E"; "E"], "E", 3, 2, []);
   (["E"; "E"], "E", 3, 3, [3]); (["E"; "E"], "E", 3, 4, [3; 4]);
   (["E"; "E"], "E", 4, 0, []); (["E"; "E"], "E", 4, 1, []);
   (["E"; "E"], "E", 4, 2, []); (["E"; "E"], "E", 4, 3, []);
   (["E"; "E"], "E", 4, 4, [4])])
