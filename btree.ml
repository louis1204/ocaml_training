
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
