
open binary_tree

pred almost_full[left,right:Node->(Node+NIL),root:(Node+NIL)] {
	binary_tree[Node,left,right]
	all x : Node | x.right !=NIL => x.left != NIL
	all x,y : Node | x.left != NIL and before[left,right,root,y,x] => (y.left != NIL and y.right != NIL)
}

fun path[left,right:Node->(Node+NIL),x,y:Node] : set Node {
	{ n : Node | x in n.*(left+right) and n in y.*(left+right) }
}

fun depth[left,right:Node->(Node+NIL),root:Node,x:Node] : Int {
	#path[left,right,x,root]
}

pred before[left,right:Node->(Node+NIL),root,x,y:Node] {
	depth[left,right,root,x] < depth[left,right,root,y] or 
	(depth[left,right,root,x] = depth[left,right,root,y] and 
		(some p : Node | x in p.left.*(left+right) and y in p.right.*(left+right)))
}
/*
sig Node {
	left, right : Node+NIL
}

one sig Root in Node {}

fact {
	almost_full[left,right,Root]
}

run {} for 3 but exactly 5 Node
*/
