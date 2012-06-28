module list

open chain[Node] 

sig Node {}

one sig EOL extends Node {}

pred list[head : Node, next : Node -> Node] {
	chain[next]
	head = minimal[next]
	EOL = maximal[next]
}	
