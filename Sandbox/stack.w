
record Node {
	data : T,
	next : Node
}

proc push[head : in out Node, t : in T]
	{ head = H and next = NEXT and list[h,next] }
  p : Node
; alloc[Node, p]
; p.next,head := head,p
	{ head.data = t and next[head.next,NULL] = NEXT[H,NULL] }

proc pop[head : in out Node]
	{ head = H and next = NEXT and head != NULL }
; head := head.next
	{ next[head, NULL] = NEXT[H.NEXT,NULL] }

proc top[head : in, t : out T]
	{ head != NULL }
; t := head.data
	{ t = head.data }

theory lists
