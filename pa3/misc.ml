(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

let sqsum xs = 
  let f a x = a + (x*x) in
  let base = 0 in
  List.fold_left f base xs;;

let pipe fs = 
  let f a x = fun y' -> x(a y') in
  let base = fun y -> y  in
    List.fold_left f base fs;;

let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = h ^ sep ^ sepConcat sep t in
      let base = h  in
      let l = t in
        List.fold_left f base l;;

let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]";;

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

let rec clone x n =
  if n <= 0 then []
  else x :: clone x (n-1);;

let rec padZero l1 l2 = 
  if List.length l1 = List.length l2
    then (l1, l2)
  else if List.length l1 < List.length l2
    then padZero (0::l1) l2
  else
    padZero l1 (0::l2);;

let rec removeZero l = match l with
 | [] -> []
 | h::t -> match h with
   | 0 -> removeZero t
   | _ -> h::t;;

let bigAdd l1 l2 = 
  let add (l1, l2) =
    let f a x =
      match a with
      | (carryin, listy) -> 
	  match x with
	  | (a', b') -> let sum = a' + b' + carryin in
	    if sum < 10 then (0, sum::listy)
	    else
	      let carryout = sum / 10 in
	      let digit = sum mod 10 in
	      (carryout, digit::listy)
	
    in

    let base = (0, []) in

    let args = List.rev (List.combine l1 l2) in

    let (finalcarryout, res) = List.fold_left f base args in
      finalcarryout::res
  in 
    removeZero ( add (padZero l1 l2));;

let rec mulByDigit i l = 
  let mul x = x * i in
  let theList = List.map mul l in
  bigAdd theList [];;

let bigMul l1 l2 = 
  let f a x = 
    match a with
    |(numZero, soFar) -> (numZero + 1, bigAdd ((mulByDigit x l1) @ clone 0 numZero) soFar) in
  let base = (0, []) in
  let args = l2 in
  let (_, res) = List.fold_left f base args in
    res;;

