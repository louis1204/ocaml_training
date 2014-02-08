(* assoc *)
let rec assoc(value, key, list)= match list with
|[] -> value
| (key', value')::tail -> if key = key' then value'
                  else assoc(value, key, tail);;  

(* remove duplicates *)
let rec removeDuplicates(list) = match List.rev(list) with
|[]->[]
|head::tail -> if List.mem head tail then removeDuplicates(List.rev(tail))
               else List.rev(head::removeDuplicates(List.rev(tail)));;
