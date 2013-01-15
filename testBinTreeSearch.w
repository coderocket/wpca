
record Node {
  left,right : Node,
  key : int
}

proc Find[r: Node, k:int, p:out Node]
	{ binary_search_tree[Node,left,right,key] and key[first[left,right,r]] < k < key[last[left,right,r]] }
  b,m,e : Node
; b,m,e := first[left,right,r], r, last[left,right,r]
; keeping
	less_eq[left,right,first[left,right,r],b] and less_eq[left,right,b,m] and less_eq[left,right,m,e] and less_eq[left,right,e,last[left,right,r]] and key[b] <= k <= key[e]
  do b.next[left,right] != e -> 
	if k < key[m] -> b,:= m
  od
; if b.right = NULL -> p := b
  [] b.right != NULL -> p := e
  fi
        { p in between[next[left,right],first[left,right,r],last[left,right,r]] and ( (key[p] <= k <= key[p.next[left,right]] and p.right = NULL) or
      (key[p.prev[left,right]] <= k <= key[p] and p.left = NULL) ) }

theory binary_search_tree
