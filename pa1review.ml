(* returns the sum of a list *)
let rec sumList list = match list with
[] -> 0
| head::tail -> head + sumList tail;;

(* returns the digits of an int *)
let rec digitsOfInt num = 
if num < 10
    then [num]
else
  digitsOfInt(num/10) @ [num mod 10];;

(* additive persistence. # of sums of digits until 1 digit *)
let additivePersistence num = 
  let count = 0 in
    let rec breakdown(n, counter) = 
      if n > 9 then
	  breakdown(sumList(digitsOfInt(n)), (counter+1))
      else
	  counter
    in
    breakdown(num, count);;

(* digital root. the resulting number from additive persistence *)
let digitalRoot num = 
  let count = 0 in
    let rec breakdown(n, counter) = 
      if n > 9 then
	  breakdown(sumList(digitsOfInt(n)), (counter+1))
      else
	  n
    in
    breakdown(num, count);;

(* listReverse *)
let rec listReverse list = match list with
[] -> []
| head::tail -> listReverse(tail)@[head];;

(*explode*)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0;;

(*palindrome*)
let palindrome string = 
 if listReverse(explode(string)) = explode(string) then true
 else false;;
