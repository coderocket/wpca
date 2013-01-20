
module heap

open binary_tree

/*
sig Node {
	left, right : Node+NIL,
	key : Int
}

one sig Root in Node {}

fact {
	heap_p[Root, left, right, key]
}

run {no (left+right).Root and some right.Node } for 6 but 1 Object
*/

pred heap_p[root:Node,left,right : Node -> (Node+NIL), key : Node -> Int]
{
	binary_tree[Node,left,right]
	all p : root.*(left+right) | all c : p.(left+right) | key[p] >= key[c]
}

pred permutation_r[k,k' : Node -> Int] 
{
	#k = #k'
all i : Node.k | #k.i = #k'.i
}

/*
check { 
	all k:Node-> Int | permutation_r[k,k]
}

check { 
	all k,k',k'':Node-> Int | permutation_r[k,k'] and permutation_r[k',k''] =>
		permutation_r[k,k'']
}

run permutation_r
*/
