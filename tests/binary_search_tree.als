module binary_search_tree

open binary_tree
open lists
/*
sig Node {
	left,right : set Node+NULL,
	key : Int
}

fact {	
	binary_search_tree[Node,left,right,key]
}

run { no Object } for 3 but 1 int
*/
pred binary_search_tree[
	dom : set univ,
	left: univ -> univ,
	right: univ -> univ,
	key : univ -> Int]
{
	binary_tree[dom,left,right]
	key in dom -> one Int

	all p,c : dom | c in p.left.*(left+right) => c.key <= p.key
	all p,c : dom | c in p.right.*(left+right) => p.key <= c.key

}
