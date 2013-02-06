
record Node {
	left,right : Node,
	key : int
}

proc MaxHeapify[root:Node] modifies key
	{ key = KEY and heap_p[root.left, left, right, key] and heap_p[root.right, left, right, key] }
  k : Node
; k := root
; keeping
	all p : (root.*(left+right)) - k | all c : p.(left+right) | c != NIL => key[p] >= key[c]
  do k != NIL and ((k.left != NIL and k.key < k.left.key) or (k.right != NIL and k.key < k.right.key)) ->
	if k.left = NIL or (k.right != NIL and k.right.key > k.left.key) -> 
		k.key,k.right.key,k := k.right.key, k.key, k.right
        [] k.right = NIL or (k.left != NIL and k.right.key <= k.left.key) -> 
        		k.key,k.left.key,k := k.left.key, k.key,k.left
        fi
  od
	{ permutation_r[key,KEY] and heap_p[root, left, right, key] and all n : Node - root.*(left+right) | n.key = n.KEY}
	
proc MakeHeap[root:Node] modifies key
	{ key = KEY and function[key,Node] and binary_tree[Node, left, right] }
; if root = NIL -> skip
  [] root != NIL -> MakeHeap[root.left]; MakeHeap[root.right] ; MaxHeapify[root]
  fi 
	{ permutation_r[key, KEY] and heap_p[root, left, right, key] and all n : Node - root.*(left+right) | n.key = n.KEY }
	
theory heap
