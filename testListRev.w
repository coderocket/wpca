  next : Node <-> Node
; h,p : Node
	{ list[h,next] and h = H and next = NEXT }
; h,p := EOL,h
; keeping
	 concat[rev[subchain[h,EOL,next]],subchain[p,EOL,NEXT]] = subchain[H,EOL,NEXT] 
  do p != EOL -> p.next,p,h := h,p.next,p
  od
	{ rev[subchain[h,EOL,next]] = subchain[H,EOL,NEXT] }

