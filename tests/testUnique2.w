  N : nat
; a : array of N int
; k,n : nat
 { a = A and monotonic[a,N] }
; k,n := 1,1
; keeping
  	1 <= n and n <= k and injective[a,n-1] and unique[a,A,k-1,n-1] 
  do k < N -> 
	if a[k] > a[n-1] -> a[n] := a[k] ; n,k := n+1,k+1
	[] a[k] <= a[n-1] -> k := k + 1
      	fi 
  od
 { injective[a,n-1] and unique[a,A,N-1,n-1] } 

where

pred monotonic[s : seq Int,N : Int]
{
 all i,j : 0..N-1 | i <= j => s[i] <= s[j] 
}

pred injective[s : seq Int, n:Int] 
{
  0..n <: a in Int lone -> lone Int
}

pred unique[s, S : seq Int, k, n : Int] 
{
}

