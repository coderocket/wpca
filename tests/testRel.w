proc testRel[head, EOL : Node ; next : Node <-> Node]
	{true}
; head.next := EOL
	{ list[head,next] }
