  N : nat
; a : array of N int
; n : int
 { a = A }
; n := 1
; keeping
    permutation[a,A] and all i,j : 0..n-1 | i <= j => a[i] <= a[j]
  do n < N -> 
     if a[n] > a[n-1] -> n := n + 1
     [] a[n] <= a[n-1] -> a[n-1],a[n] := a[n],a[n-1]; n := n + 1
     fi
  od
 { permutation[a,A] and all i,j : 0..N-1 | i <= j => a[i] <= a[j] }

