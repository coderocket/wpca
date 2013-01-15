proc InsertionSort[N : nat, a : array of N int, n,k : int]
 { a = A }
; n := 1
; keeping
    permutation[0,n,a,A] and n > 0 and all i,j : 0..n-1 | i <= j => a[i] <= a[j]
  do n < N -> 
     if a[n] >= a[n-1] -> n := n + 1
     [] a[n] < a[n-1] ->
	k := n - 1
        ; keeping
		all i : 0..n-1 | i != k => a[i] <= a[i+1] 
        do k > -1 and a[k] > a[k+1] -> a[k],a[k+1] := a[k+1],a[k]; k := k - 1 od 
     fi
  od
 { permutation[0,N,a,A] and all i,j : 0..N-1 | i <= j => a[i] <= a[j] }

