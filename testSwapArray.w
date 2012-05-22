N : nat
; a : array of N int
; i,j : int
	{a = A }
; a[i],a[j] := a[j],a[i]
	{a[i] = A[j] and a[j] = A[i] and permutation[a,A] }

