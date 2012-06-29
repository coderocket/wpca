  next : Node <-> Node
; h,p : Node
	{ list[h,next] and h = H and next = NEXT }
; h,p := EOL,h
; keeping
	 member[p,next] and member[h,next] and separate[p,EOL,h,EOL,next] and concat[rev[subchain[h,EOL,next]],subchain[p,EOL,NEXT]] = subchain[H,EOL,NEXT] 
  do p != EOL -> p.next,p,h := h,p.next,p
  od
	{ list[h,next] and rev[subchain[h,EOL,next]] = subchain[H,EOL,NEXT] }

