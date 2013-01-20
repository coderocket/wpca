proc Max[a : array of N int, N : nat, m : out int]
	{ N > 0 }
  j : int
; j, m := 1,a[0] 
; keeping
	0 <= j <= N and (some i : 0..j-1 | m=a[i]) and all i : 0..j-1 | m >=a[i] 
  do j < N -> 
    if a[j] > m -> m := a[j]; j := j + 1
    [] a[j] <= m -> j := j + 1
    fi
  od
	{ (some i : 0..N-1 | m = a[i]) and all i : 0..N-1 | m >= a[i] }

