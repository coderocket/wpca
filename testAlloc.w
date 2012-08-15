
record Node {
	data : int
	; next : Node
}

proc Insert[h : Node]
 x : Node
	{ some (Node - extent) }
; x is new Node
; h.next := x
	{ h.next != NULL } 
