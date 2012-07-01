  next : Node <-> Node
; head, new : Node
; NULL : Node
	{ list[head,next] and head = H and new != NULL and !(new in crange[head,NULL,next]) and next = NEXT and EOL = NULL }
; new.next := head 
; head := new
	{ list[head,next] and head = new and subchain[head.next,NULL,next] = subchain[H,NULL,NEXT] }

