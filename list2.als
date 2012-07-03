module list

open partialOrder[univ] 

one sig NULL {}

sig Node {}

pred nullterminated[r : univ -> univ, h : univ] {
	list[r]
	NULL in h.*r
}

fun rev[r : univ -> univ] : univ -> univ { ~r }
