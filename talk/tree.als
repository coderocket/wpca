
sig Node {
	left,right : set (Node+NIL)
}

one sig NIL {}

fact {
	left in Node -> one (Node+NIL)
	right in Node -> one (Node+NIL)
	(left + right) :> Node in Node lone ->Node
	left & right in Node -> NIL
	no iden & ^(left+right)
}

run {} for 4
