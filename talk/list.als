
sig Node {
	next : set (Node+NIL)
}

one sig NIL {}

fact {
	list_p[next]
}

run {} for 4


pred list_p[next:Node->(Node+NIL)] 
{
	Node <: next in Node  -> one (Node+NIL)
	next :> Node in Node lone -> lone Node
	no iden & ^(Node <: next)
}

fun list[h:Node+NIL, next:Node->(Node+NIL)] : Node->(Node+NIL)
{
	{ x:Node, y : Node+NIL | x->y in next and x+y in h.*next }
}

fun rev[next:Node->(Node+NIL)] : Node->(Node+NIL)
{
	~(next :> Node) + first[next] -> NIL 
}

fun first[next:Node -> (Node+NIL)] : Node
{
	{ h : Node | h in next.(Node+NIL) and (no x : Node | x->h in next) }
}

fun last[next:Node -> (Node+NIL)] : Node
{
	{ h : Node | h in next.(Node+NIL) and h.next = NIL }
}

fun concat[next,next':Node -> (Node+NIL)] : Node -> (Node+NIL)
{
	(next :> Node) ++ next' + last[next] -> first[next']
}

fun nodes[next:Node -> (Node+NIL)] : set Node
{
	next.(Node+NIL)
}

pred separate[next,next':Node->(Node+NIL)] 
{
	no nodes[next] & nodes[next']
}

check {
	all next, next' : Node -> (Node+NIL) | 
		list_p[next] and list_p[next'] and separate[next,next'] => list_p[concat[next,next']]
}

