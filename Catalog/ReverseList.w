
record Node {
  next : Node 
}

proc ListReverse[h: out Node]
	{ list_p[next] and h = H and next = NEXT }
  p : Node
; h,p := NIL, h
; keeping
	separate[list[h,next],list[p,next]] and
	concat[rev[list[h,next]],list[p,next]] = list[H,NEXT] 
  do p != NIL -> h,p,p.next := p,p.next,h
  od
	{ list_p[next] and concat[rev[list[h,next]],list[p,next]] = list[H,NEXT] }

theory list
