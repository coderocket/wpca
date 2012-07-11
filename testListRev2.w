  next : Node <-> Node
; h,p: Node
	{ nullterminated[next,h] and h = H and next = NEXT }
; h,p := NULL,h
; keeping
	 p in between[NEXT,H,NULL] and h in between[NEXT,H,NULL] and separate[corange[next,p,NULL],corange[next,h,NULL]] and concat[rev[corange[next,h,NULL]],corange[NEXT,p,NULL]] = corange[NEXT,H,NULL] 
  do p != NULL -> p.next,p,h := h,p.next,p
  od
	{ nullterminated[next,h] and rev[corange[next,h,NULL]] = corange[NEXT,H,NULL] }

