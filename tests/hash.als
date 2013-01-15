module hash[T]

open list

one sig State {
	next : Node -> Node,
	data : Node -> T,
	table : seq Node,
	h : T -> one Int
}

pred hash[h : T -> Int, table : seq Node, next : Node -> Node, data:Node -> T] {
	all n : Int.table | list[n,next] 
	h in T -> one { i : Int | 0 <= i and i < #table }
	all i,j : table.Node | i != j => separate[table[i], EOL, table[j], EOL, next]
	data in Node -> lone T
	all i : table.Node | h[crange[table[i], EOL,next].data] = i
}

run { hash[State.h, State.table, State.next, State.data] }


