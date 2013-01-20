
module binary_tree

open misc
open util/relation
/*
sig Node {
	left,right : set Node+NIL
}

fact {	
	binary_tree[Node,left,right]
}

run { no Object }

run { some r,p : Node | ! less_eq[left,right,first[left,right,r],p] }
*/

pred binary_tree[
    dom : set univ,
	left: univ -> univ,
	right: univ -> univ]
{
	acyclic[left+right,dom]
	left in dom -> one univ
	right in dom -> one univ

	let lft = left :> (univ-NIL), rgt = right :> (univ-NIL) | {
		lft + rgt in dom lone -> univ
		no lft & rgt
   }
}

pred less_eq[
	lft: univ -> univ,
	rgt: univ -> univ,
	x,y: univ] 
{
	let left = lft :> (univ-NIL), right = rgt :> (univ-NIL) | {
		some z : univ | x in (z.left.*(right+left) + z) and y in (z.right.*(left+right) + z)
	}
}

fun prev[
	left: univ -> univ,
	right: univ -> univ] : univ -> univ
{
	{ x,y : univ | x != y and less_eq[left,right,y,x] and 
						(all x':univ-x | less_eq[left,right,x',x] => less_eq[left,right,x',y]) }
}

fun next[
	left: univ -> univ,
	right: univ -> univ] : univ -> univ
{
	~(this/prev[left,right])
}

fun first[
	left: univ -> univ,
	right: univ -> univ,
	root:univ] : univ
{
	{ x : root.*(left+right) - NIL | no y : root.*(left+right) | y = this/prev[left,right,x] }
}

fun last[
	left: univ -> univ,
	right: univ -> univ,
	root:univ] :univ
{
	{ x : root.*(left+right) - NIL | no y : root.*(left+right) | y = this/next[left,right,x] }
}

