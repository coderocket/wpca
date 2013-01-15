
proc Partition[a: out array of N int, N : nat, i : out int]
	{ a = A and N > 1 }
  n : int
; n,i := 1,0
; keeping
	permutation[0,n,a,A] and 0 <= i < n and a[i] = A[0] 
	and (all j : 0..i-1 | a[j] <= A[0]) and (all j : i+1..n-1 | A[0] <= a[j]) 
  do n < N -> 
	if a[i] <= a[n] -> n := n + 1
	[] a[i] > a[n] -> a[i] := a[n]; a[n] := a[i+1]; a[i+1] := a[i]; i,n := i+1,n+1
        fi
  od
	{ permutation[0,N,a,A] and 0 <= i < N and a[i] = A[0] 
	and (all j : 0..i-1 | a[j] <= A[0]) and (all j : i+1..N-1 | A[0] <= a[j]) } 
