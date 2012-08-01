module binaryTree

open partialOrder[Node]

sig Node {
	left, right : lone Node
}

fact {
	tree[left+right]
	all x : Node | some x.left => x.left != x.right
}

check {
	no x : Node | some (x.left +x.right) and x.left = x.right
}

run { some Node } for 4
