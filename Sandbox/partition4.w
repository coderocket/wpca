
proc Partition[a: out array of N int, N : nat, i : out int]
	{ a = A and N > 1 }
; skip
	{ permutation[0,N,a,A] and 0 < i < N and (all j : 0..i-1 | a[j] <= a[i]) and (all j : i..N-1 | a[i] <= a[j]) } 
