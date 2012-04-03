  N : nat 
; f : array of N int
; s : int
; n : int
; s := 0 ; n := 0
; do n != N -> s := s + f[n] ; n := n + 1 od
{ s = sum i : 0..N | f[i] }
