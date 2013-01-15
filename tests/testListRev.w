
record Node {
  next : Node 
}

proc Reverse[h,p: Node]
	{ nullterminated[next,h] and h = H and next = NEXT }
; h,p := NULL,h
; keeping
	 separate[corange[next,p,NULL],corange[next,h,NULL]] and concat[~corange[next,h,NULL],corange[NEXT,p,NULL]] = corange[NEXT,H,NULL] 
  do p != NULL -> p.next,p,h := h,p.next,p
  od
	{ nullterminated[next,h] and ~corange[next,h,NULL] = corange[NEXT,H,NULL] }

theory lists
