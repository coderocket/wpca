proc testList[head, p : Node; x : T]
	{ list[head] }
; p := head
; keeping
	p in head.*next and (all q : lrange[head, p] | q.data != x) 
  do p != EOL and p.data != x -> p := p.next od
	{ p in head.*next and (all q : lrange[head, p] | q.data != x) and (p != EOL => p.data = x) }

