
open util/seqrel[Int]

open misc

// x is a sorted permutation of y

pred sorted[x,y : seq Int]
{
	#x = #y
	permutation[0,#x,x,y]
	all i : range[0,(#x).sub[2]] | x[i] <= x[i.add[1]]	
}

//run sorted

