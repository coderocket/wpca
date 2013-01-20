
record Node {
	left,right : Node,
	key : int
}

proc MaxHeapify[root:Node]
	{ key = KEY and heap_p[root.left, left, right, key] and heap_p[root.right, left, right, key] }
  k : Node
; k := root
; keeping
	all p : (root.*(left+right)) - k | all c : p.(left+right) | key[p] >= key[c]
  do k != NIL -> skip
  od
	{ permutation_r[key,KEY] and heap_p[root, left, right, key] }
theory heap
