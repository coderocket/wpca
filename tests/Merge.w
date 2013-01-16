
proc Merge[left : array of N int, N : nat, right : array of M int, M : nat, result : out array of K int, K : nat]
	{ K = N+M and (all i : 0..N-2 | left[i] <= left[i+1]) and (all i : 0..M-2 | right[i] <= right[i+1]) }
  k,n,m : nat
; k,n,m := 0,0,0
; keeping
	n+m = k and sorted[subseq[result,0,k-1], append[subseq[left,0,n-1],subseq[right,0,m-1]]] and (all i:0..n-1,j:m..M-1 | right[j] >= left[i]) and (all i:n..N-1,j:0..m-1 | left[i] >= right[j])
  do n < N -> 
	if m = M or left[n] <= right[m] -> result[k],k,n := left[n], k+1,n+1 
	[] m < M and left[n] > right[m] -> result[k],k,m := right[m],k+1,m+1
	fi
  [] m < M -> 
	if n = N or right[m] <= left[n] -> result[k],k,m := right[m],k+1,m+1 
	[] n < N and right[m] > left[n] -> result[k],k,n := left[n],k+1, n+1 
	fi
  od
	{ sorted[result, append[left,right]] }

theory arrays
