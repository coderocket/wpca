
record Node {
	left,right : Node,
	key : int
}

proc MaxHeapify[root:Node] modifies key
	{ key = KEY and heap_p[root.left, left, right, key] and heap_p[root.right, left, right, key] }
  k : Node
; k := root
; keeping
	all p : (root.*(left+right)) - k | all c : p.(left+right) | key[p] >= key[c]
  do k != NIL and (key[k] <= key[k.left] or key[k] <= key[k.right]) ->
	if k.left = NIL or (k.right != NIL and key[k.right] > key[k.left]) -> k.key,k.right.key,k := k.right.key, k.key, k.right
        [] k.right = NIL or (k.left != NIL and key[k.right] <= key[k.left]) -> k.key,k.left.key,k := k.left.key, k.key,k.left
        fi
  od
	{ permutation_r[key,KEY] and heap_p[root, left, right, key] }
	
proc MakeHeap[root:Node] modifies key
	{ binary_tree[Node, left, right] }
; if root = NIL -> skip
  [] MakeHeap[root.left]; MakeHeap[root.right]; MaxHeapify[root]
  fi 
	{ heap_p[root, left, right, key] }
	
theory heap
