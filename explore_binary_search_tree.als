
open binary_search_tree[Node]

sig Node {
	left,right,before,prev,next : set Node,
	key : set { i : Int | i >= 0 }
}

fact {	
	binary_search_tree[left,right,key]
	before = { x,y:Node | before_p[left,right,x,y] }
	next = {x,y:Node | prev_p[left,right,x,y] }
	Node<: prev = ~next
}

run { some before and some right and some left }

check {
	partialOrder[before, Node]
} for 5 Node

check {
	all x,y : Node | x->y in next => key[x] <= key[y]
}

//run { some n:Node, k:Int | some before and some right and some left and proper_place[n,k] }

