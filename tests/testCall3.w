
proc Set[a : out array of N int; N : nat; i : out int]
	{N > 0}
; a[0] := 0
	{a[0] = 0}

proc Use[b : out array of M int; M : nat]
 k : int
	{ b = B and M > 0 }
; Set[b,M,k]
	{ b[0] = 0 }
