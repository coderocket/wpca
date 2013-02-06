
record Node {
	key : int
}
	
proc B[root:Node] modifies key
	{ key = KEY }
; B[root] ; B[root]
	{ permutation_r[key,KEY] }
	
theory heap
