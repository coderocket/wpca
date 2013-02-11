
record Node {
	left,right : Node
}

proc FindHeapLast[root:Node, last : out Node] 
	{ almost_full[left,right,root] }
; last := root
; keeping
	last in root.*(left+right) and no x : root.*(left+right) - last.*(left+right) | all y : last.*(left+right) - NIL | before[left,right,root,y,x]
  do last != NIL and last.left != NIL and last.right != NIL -> 
  	if last.right.left != NIL -> last := last.right
  	[] last.right.left = NIL -> 
  		if last.left.left = NIL and last.left.right = NIL -> last := last.right
  		[] last.left.left != NIL or last.left.right != NIL -> last := last.left
  		fi
  	fi
  []   last != NIL and last.left != NIL and last.right = NIL -> last := last.left
  od
	{ last in root.*(left+right) and no x : root.*(left+right) - last - NIL | before[left,right,root,last,x] }

theory almost_full
