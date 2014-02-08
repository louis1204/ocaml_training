(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
   This function takes a list of integers and sums up the elements of the list
   (see the digits function below for an example of what is expected)
*) 

let rec sumList list = match list with
  | [] -> 0
  | head::tail -> head + sumList tail;;

(* digitsOfInt : int -> int list 
   This function takes an int and returns a list of the digits of the integer
 *)

let rec digitsOfInt n =
  if n = 0 then []
  else if n < 10 then n :: []
  else digitsOfInt(n/10) @ [(n mod 10)];;

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)

(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)

(* additivePersistence : int -> int 
   This function takes an int and returns the number of times we sum up its
   digits before it is less than 10
*) 

let additivePersistence n =
  let inc =  fun x -> x + 1 in
    let count = 0 in
      let rec breakdown (n, counter) =
         if (n < 10) then counter
         else(
           breakdown ( sumList( digitsOfInt( n )), inc counter)) in
      breakdown(n, count);;

(* digitalRoot : int -> int 
   This function takes an int and returns the number that is less than ten
   after we repeatedly sum up its digits.
*) 

let digitalRoot n = 
  let rec breakdown (n) =
    if (n < 10) then n
    else(
      breakdown ( sumList( digitsOfInt( n )))) in
  breakdown(n);;

(* listReverse : a' list -> a' list 
   This function takes a list and returns the reverse of that list.
*) 

let listReverse l =
  let rec reverse list = match list with
    | [] -> []
    | head::tail -> reverse(tail) @ [head] in
  reverse l;;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)

let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0;;

(* palindrome : string -> bool 
   This function takes a string and returns true if it is a palindrome
   and false if it is not a palindrome.
*) 

let palindrome w = 
  if listReverse ( explode ( w )) = explode ( w ) then true
  else false;;
