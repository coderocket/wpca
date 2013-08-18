
  N : nat
; f : array of N int
; n : int
 { f = F }
; n := 1
; keeping
   permutation[f,F] and all i,j : 0..n-1 | i <= j => f[i] <= f[j] 
  do
     n < N and f[n-1] <= f[n] -> n := n + 1
  [] n < N and f[n-1] > f[n] -> f[n-1],f[n] := f[n],f[n-1] 
  od
 { permutation[f,F] and all i,j : 0..N-1 | i <= j => f[i] <= f[j] }
	
