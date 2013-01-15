proc Search[x : int; N : nat; f : array of N int; i : int]
  { true }
; i := 0
; keeping
	all j : 0..i-1 | f[j] != x
  do i < N and f[i] != x -> i := i + 1 od
  { (i = N and no j : 0..N-1 | f[j] = x) or (0 <= i and i < N and f[i] = x) }
