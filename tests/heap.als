
module heap

open util/relation
open binary_tree

/*
sig Node {
	left, right : Node+NIL,
	key,key',key'' : set Int
}

one sig Root in Node {}

fact {
	heap_p[Root, left, right, key]
}

run {no (left+right).Root and some right.Node } for 6 but 1 Object
*/

pred heap_p[root:Node+NIL,left,right : Node -> (Node+NIL), key : Node -> Int]
{
	function[key,Node]
	binary_tree[Node,left,right]
	all p : root.*(left+right) | all c : p.(left+right) | key[p] >= key[c]
}

/*
check {
	all left,right : Node -> (Node+NIL), key : Node -> Int |
		function[key,Node] and binary_tree[Node,left,right] => heap_p[NIL, left, right, key]
}
*/

pred permutation_r[k,k' : Node -> Int] 
{
	k.Int = k'.Int
	all i : Node.(k+k') | #(Node<:k).i = #(Node<:k').i
} 

/*
check {
	permutation_r[key,key'] => #key = #key'
} for 3 but 3 int

check {
	permutation_r[key,key] 
} 

check {
	permutation_r[key,key'] <=> permutation_r[key',key]
} 

check {
	permutation_r[key,key']  and permutation_r[key',key''] => permutation_r[key,key'']
} 
*/
