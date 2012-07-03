module partialOrderUtils[T]

open util/relation

pred tree[r : T->T] {
	partialOrder[*r,T] and acyclic[r,T]
}

pred list[r : T->T] {
	tree[r]
	r in T lone -> lone T
}

// closed-closed [b..e]

fun ccrange[r:T->T,b,e:T] : T->T {
		after[r,b] <: r :> before[r,e]
}

// closed-open [b..e)

fun corange[r:T->T,b,e:T] : T->T {
		after[r,b] <: r :> (before[r,e] - e)
}

fun after[r:T->T,b:T] : set T {
	{ x : T | x in b.*r }
}

fun before[r:T->T,e:T] : set T {
	{ x : T | e in x.*r }
}

fun between[r:T->T,b,e:T] : set T {
	after[r,b] & before[r,e]
}

pred inbetween[p:T,r:T->T,b,e:T]  {
	p in between[r,b,e]
}

fun last[r:T->T] : T {
        { e : T | e in T.^r and no e.r }
}

fun first[r:T->T] : T {
        { e : T | e in ^r.T and no r.e }
}

fun concat[x,y:T->T] : T -> T {
        x + (last[x] -> first[y]) + y
}

pred separate[x,y :T->T] {
        no (x.T + T.x) & (y.T + T.y)
}

check {
	all r : T->T, b : T | after[r,b] = before[~r,b]
}

check {
	all r : T->T | list[r] => 
		ccrange[r,first[r],last[r]] = r 
}

check {
	all x,y : T->T | list[x] and list[y] and separate[x,y] =>
		list[concat[x,y]]
}

check {
	all x,y : T->T | list[x] and list[y] and separate[x,y] and some x => 
		first[concat[x,y]] = first[x] 
}

check {
	all x,y : T->T | list[x] and list[y] and separate[x,y] and some y => 
		last[concat[x,y]] = last[y] 
}

check {
	all x,y : T->T | list[x] and list[y] and no x => 
		first[concat[x,y]] = first[y] 
}

check {
	all x,y : T->T | list[x] and list[y] and no y => 
		last[concat[x,y]] = last[x] 
}

// trees

check {
	all x : T->T, b,e : T | tree[x] => tree[ccrange[x,b,e]]
}
