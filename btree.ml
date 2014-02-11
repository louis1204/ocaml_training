
(* type definition of ocaml binary tree node *)
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

(* sample btree *)
let sampleBtree = Node(4, Node(2, Node(1, Empty, Empty), Node(3, Empty, Empty)), Node(6, Node(5, Empty, Empty), Node(7, Empty, Empty)));;

(* member tries to find if a node is a member of the btree *)

let rec member btree node = match btree with
Empty -> false
|Node(curNode, left, right) -> if node == curNode then true else 
                           if node < curNode then member left node else
                            member right node;;

(* insert insert a new element into a btree *)
let rec insert element btree = match btree with
Empty -> Node(element, Empty, Empty)
|Node(curNode, left, right) -> if element <= curNode then Node(curNode, (insert element left), right) else
                               Node(curNode, left, (insert element right));;
(* returning Node in the second case will push down the elements so we will still have
the tree in the end. *)

(* insert subtree *)
let rec insertSubtree btreeFrom btreeTo = match (btreeFrom, btreeTo) with
|(from, Empty) -> from
|(Node(elementFrom, _, _), Node(elementTo, leftTo, rightTo)) -> 
 if elementFrom < elementTo then
  Node(elementTo, insertSubtree btreeFrom leftTo, rightTo)
 else
  Node(elementTo, leftTo, insertSubtree btreeFrom rightTo);;


(* delete an element from a tree *)
let delete element tree =
if not(member tree element) then tree (*element not found*)
else let rec deleteHelper(element, tree) = match tree with
|Node(curNode, Empty, Empty) -> Empty
|Node(curNode, Empty, right) -> if curNode == element then right else Node(curNode, Empty, deleteHelper(element, right))
|Node(curNode, left, Empty) -> if curNode == element then left else Node(curNode, deleteHelper(element, left), Empty)
|Node(curNode, left, Node(nodeRight, leftRight, rightRight)) -> 
 if curNode == element then Node(nodeRight, insertSubtree left leftRight, rightRight) else
 if element < curNode then Node(curNode, deleteHelper(element, left), Node(nodeRight, leftRight, rightRight)) else
 Node(curNode, left, deleteHelper(element, Node(nodeRight, leftRight, rightRight))) in
 deleteHelper(element, tree);;
