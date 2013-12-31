

proc Filter[a : array of N int, N : int, x : int, m : out int]
  { a = A }
  j : int
; j,m := 0,0
; keeping
    0 <= m <= j and j <= N and (all i : 0..m-1 | a[i] != x) and (all i : 0..j-1 | A[i] !=x => size[dres[0..m-1,a].A[i]] = size[dres[0..j-1, A].A[i]])  
  do j != N -> 
     if a[j] = x -> j := j + 1
     [] a[j] != x -> a[m], j, m := a[j], j+1, m+1
     fi 
  od 
  { 0 <= m <= N and (all i : 0..m-1 | a[i] != x) and all i :0..N-1 | A[i] != x => size[dres[0..m-1,a].A[i]] = size[A.A[i]] }
	
