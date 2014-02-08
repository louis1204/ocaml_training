(* assoc - finds the first matching key and returns value o.w. value *)
let rec assoc(value, key, list)= match list with
|[] -> value
| (key', value')::tail -> if key = key' then value'
                  else assoc(value, key, tail);;  

(* remove duplicates *)
let removeDuplicates(list') =
  let rec removeDuplicatesHelper(list) = match List.rev(list) with
  |[]->[]
  |head::tail -> if List.mem head tail then 
                  removeDuplicatesHelper(List.rev(tail))
                 else [head]@removeDuplicatesHelper(List.rev(tail))
  in List.rev(removeDuplicatesHelper(list'));;

(* wwhile - f returns (result, boolean) while boolean wwhile(f, result) 
o.w. result.

Pretty much we just keep repeating f on its own result until we get false,
which will then get our result*)

let f x = let xx = x*x*x in (xx, xx < 100);;

let rec wwhile(f, x) = match f(x) with
|(result, boolean) -> if boolean then wwhile(f, result)
                      else result;;

(* fixpoint - if f(x) = x and f(f(x)) = f(x) then f(x) otherwise fixpoint(f, f(x))*)
let rec fixpoint(f, x) =
if f(x) = x && f(f(x)) = f(x) then f(x)
else fixpoint(f, f(x));;
