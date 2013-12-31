
proc Reverse[a : array of N int, N : int]
  { a = A }
  j : int
; j := 0
; keeping 
    (all i : 0..j-1 | a[i] = A[N-i-1]) and
    (all i : N-j..N-1 | a[i] = A[N-i-1]) 
  do j <= (N-1) div 2  ->
    a[j], a[N-j-1] := a[N-j-1], a[j] ; j := j+1
  od
  { all i : 0..N-1 | a[i] = A[N-i-1] }
	
