proc Sum [N : nat ; f : array of N int ; i, s : int]
{ N >= 0 }
; s := 0
; i := 0
; keeping 
    s = sum j : 0..i-1 | f[j]
  do i != N -> s := s + f[i] ; i := i + 1 od
{ s = sum j : 0..N-1 | f[j] }
