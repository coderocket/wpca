module arrays

open util/integer

fun range[b,e:Int] : set Int {
  { z : Int | z.gte[b] and z.lte[e] }
}

pred nochange[b,e:Int, x,X:seq univ] 
{
	let r = range[b,e.sub[1]] | {
		r <:x = r <: X
	}		
}

pred sorted[x : seq Int]
{
	all i : range[0,(#x).sub[2]] | x[i] <= x[i.add[1]]	
}

pred permutation[x, y: seq univ] {
	#x = #y
	all t : Int.x | #x.t= #y.t
}

// prefix

fun prefix[s: seq univ, n : Int] : seq univ {
	range[0,n.sub[1]] <: s
}

check {
	all s : seq univ | prefix[s,0] = none -> none 
}

check {
	all s : seq univ | prefix[s,#s] = s 
}

// suffix

fun suffix[s: seq univ, n : Int] : seq univ {
	range[(#s).sub[n],(#s).sub[1]] <: s
}

check {
	all s : seq univ | suffix[s,0] = none -> none 
}

check {
	all s : seq univ | suffix[s,#s] = s 
}

// largest number in array. If the array is empty returns the smallest possible
// integer in the current scope

fun Max[s : seq Int] : one Int {
	max[Int.s + min]
}

// Smallest number in array. If the array is empty returns the largest possible
// integer in the current scope.

fun Min[s : seq Int] : Int {
	min[Int.s + max]
}

