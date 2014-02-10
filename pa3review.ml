(* sqsum return the square and sum of each element in a list *)
let sqsum(list) = 
 let accum sum num =
   num * num + sum
 in
 List.fold_left accum 0 list;; (* forgot about the base case! *)

(* pipe - list of functions and a value - returns the value going 
through all the functions *)

let pipe(fnList) =
 let f func1 func2 = fun input -> func2(func1(input)) in
 let base = fun input' -> input' in
 List.fold_left f base fnList;;

(* f is our accumulator in this case.
val f : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = <fun>
fold_left will then do f for each in fnList func 1 being the first function
and func 2 being the second function. We call pipe with pipe [] 9 where
nine can be replaced with whatever your list of functions take in. This
is because of the fun input for f that we also need to supply with the initial
input.
*)

(* sepConcat seperates list of string and joints each element with a string
between. *)

let sepConcat seperator stringList = match stringList with
 [] -> ""
|head::tail -> 
 let concater accum item = head ^ seperator ^ sepConcat seperator tail in
 let base = head in
 let list = tail in
  List.fold_left concater base list;;

(* stringOfList to converter a list to string first convert every item to a string
then sepConcat to add a ; between all *)
let stringOfList converter list = "[" ^ sepConcat "; " (List.map converter list) ^ "]";;

(* clone - clones a number n times *)
let rec clone item times = if times > 0 then item :: clone item (times-1) else [];;

(* padZero use clone to pad on left a list till same size *)
let padZero list1 list2 = 
if List.length list1 > List.length list2 then
 (list1, (clone 0 (List.length list1 - List.length list2)) @ list2)
else
 ((clone 0 (List.length list2 - List.length list1)) @ list1, list2);;

(* removeZero - removes 0 from the left of the list *)
let rec removeZero list = match list with
[] -> []
|head::tail -> if head == 0 then removeZero tail else head::tail;;

(* bigAdd - adds two integer lists *)
let bigAdd list1' list2' =
 let rec bigAddHelper((list1, list2), res, carry) = match List.rev list1, List.rev list2 with
 |([], []) -> if carry then 1::res else res
 |(head1::tail1, head2::tail2) -> if carry then 
                                   if head1 + head2 + 1 > 9 then
                                    bigAddHelper((List.rev tail1, List.rev tail2), head1+head2+1-10::res, true)
                                   else
                                    bigAddHelper((List.rev tail1, List.rev tail2), head1+head2+1::res, false)
                                  else
                                   if head1 + head2 > 9 then
                                    bigAddHelper((List.rev tail1, List.rev tail2), head1+head2-10::res, true)
                                   else
                                    bigAddHelper((List.rev tail1, List.rev tail2), head1+head2::res, false)
 in
  bigAddHelper((padZero list1' list2'), [], false);;
  
(* mulByDigit number List - multiplies List with number *)
let mulByDigit digit list = 
 let rec mulByDigitHelper(digit, origList, resList) = 
   if digit > 0 then mulByDigitHelper(digit - 1, origList, (bigAdd origList resList))
   else resList in match padZero [] list with
    (zerosList, sameList) -> mulByDigitHelper(digit, list, zerosList);;

(* bigMul list1 list2 - multiplies 2 lists 
let bigMul list1' list2' =
 let rec bigMulHelper(list1, list2, result, times) = match List.rev list2 with
  [] -> result
 |head::tail -> bigMulHelper(list1, List.rev tail, (bigAdd (mulByDigit((head * (exponent 10 times)) list1) result)), times + 1) in
 bigMulHelper(list1', list2', [0], 0 );;

let exponent base' exp' = 
 let rec exponentHelper(base, exp, res) = 
  if exp > 0 then
   exponentHelper(base, exp-1, res*base)
  else
   res
 in
 exponentHelper(base', exp', 1);;
*)
let bigMul l1 l2 = 
  let f a x = 
    match a with
    |(numZero, soFar) -> (numZero + 1, bigAdd ((mulByDigit x l1) @ clone 0 numZero) soFar) in
  let base = (0, []) in
  let args = l2 in
  let (_, res) = List.fold_left f base args in
    res;;

