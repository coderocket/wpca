x : int
  { x = X }
; if true -> x := -x 
  [] true -> skip fi
  { abs[x] = abs[X] }

