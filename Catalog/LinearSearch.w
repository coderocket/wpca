proc LinearSearch[f : array of N int, N : nat, x : int, i : out int]
  { true }
; i := 0
; keeping
	0 <= i <= N and all j : 0..i-1 | f[j] != x 
  do i < N and f[i] != x -> i := i + 1
  od
  { 0 <= i <= N and all j : 0..i-1 | f[j] != x and i != N => f[i] = x}
