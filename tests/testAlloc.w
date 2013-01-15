
record Node {
	data : int
	; next : Node
}

proc Insert[h : Node]
	{ some (Node - extent) }
  x : Node
; x is new Node
; h.next := x
	{ h.next != NULL } 
