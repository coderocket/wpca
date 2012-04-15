  N : nat
; f : array of N int
; i, s : int
{ true }
; s := 0
; i := 1
; keeping 
    s = sum j : 1..i | f[j]
  do i != N -> s := s + f[i] od
{ s = sum j : 1..N | f[j] }
