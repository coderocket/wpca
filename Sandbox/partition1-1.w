
proc Partition[a: out array of N int, N : nat, i : out int]
	{ a = A and N > 1 }
  n : int
; n,i := 0,0 
; keeping
	permutation[0,n,a,A] and (all j : 0..i-1 | a[j] <= A[0]) and (all j : i..n-1 | A[0] <= a[j]) 
  do n < N -> skip
  od
	{ permutation[0,N,a,A] and 0 < i < N and (all j : 0..i-1 | a[j] <= A[0]) and (all j : i..N-1 | A[0] <= a[j]) } 
