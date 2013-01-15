proc SwapArray[N : nat ; a : array of N int ; i,j : int]
 { a = A and 0 <= i and i < N and 0 <= j and j < N }
; a[i],a[j] := a[j],a[i]
 { permutation[a,A] and a[i]=A[j] and a[j] = A[i] }

