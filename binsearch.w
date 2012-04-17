  x : int 
; N : nat
; f : array of N int
; i,j : int
  { all k,l : 0..N-1 | k <= l => f[k] <= f[l] }
; keeping 
 { (i = N and no k : 0..N-1 | f[k] = x) or (0 <= i and i < N and f[i] = x) }
