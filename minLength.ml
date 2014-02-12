let minLength list = if list == [] then 0 else
 let rec minLengthHelper(list, count) = match list with
 |[[]] -> 0
 |[] -> count
 |head::tail -> if List.length head < count then minLengthHelper(tail, List.length head)
                else minLengthHelper(tail, count) in
 minLengthHelper(list, max_int);;

let minLength list = if list == [] then 0 else
 let rec minLengthHelper(list, count) = match list with
 |[] -> count
 |head::tail -> if head < count then minLengthHelper(tail, head)
                else minLengthHelper(tail, count) in
 minLengthHelper(List.map List.length list, max_int);;