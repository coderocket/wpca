  N : nat
; a : array of N int
; k,n : nat
 { a = A and all i,j : 0..N-1 | i <= j => a[i] <= a[j] }
; k,n := 1,1
; keeping
  	1 <= n and n <= k and (all i,j : 0..n-1 | i != j => a[i] != a[j]) and all i : 0..k-1 | some j : 0..n-1 | A[i] = a[j] 
  do k < N -> 
	if a[k] > a[n-1] -> a[n] := a[k] ; n,k := n+1,k+1
	[] a[k] <= a[n-1] -> k := k + 1
      	fi 
  od
 { (all i,j : 0..n-1 | i != j => a[i] != a[j]) and all i : 0..N-1 | some j : 0..n-1 | A[i] = a[j] }

