  next : Node <-> Node
; NULL, h, p : Node
	{ list[h,next] and h = H and next = NEXT and EOL = NULL }
; h,p := NULL,h
; keeping
	 member[p,next] and member[h,next] and separate[p,NULL,h,NULL,next] and concat[rev[subchain[h,NULL,next]],subchain[p,NULL,NEXT]] = subchain[H,NULL,NEXT] 
  do p != NULL -> p.next,p,h := h,p.next,p
  od
	{ list[h,next] and rev[subchain[h,NULL,next]] = subchain[H,NULL,NEXT] }

