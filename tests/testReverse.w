
proc Reverse[a : array of N int, N : int]
  { a = A }
  j,k : int
; j,k := 0,N
; keeping 
    (all i : 0..j-1 | a[i] = A[N-i-1]) and
    (all i : k..N-1 | a[i] = A[N-i-1]) and 
    j <= k
  do j < k -> skip
  od
  { all i : 0..N-1 | a[i] = A[N-i-1] }
	
