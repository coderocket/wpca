x : int
  { x = X }
; if true -> x := -x 
  [] true -> skip fi
; if true -> x := -x 
  [] true -> skip fi
  { x = X }

