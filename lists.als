module lists

open partialOrder[univ] 
open misc

pred nullterminated[r : univ -> univ, h : univ] {
	list[r]
	NULL in h.*r
}

