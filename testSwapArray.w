  N : nat
; a : array of N int
 { a = A }
; a[i],a[j] := a[j],a[i]
 { (all k : 0..N-1 | (k !=i and k != j) => a[k]= A[k]) and a[i]=A[j] and a[j] = A[i] }

