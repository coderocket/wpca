proc Gcd[x, y : int]
  { x = X and y = Y and x > 0 and y > 0 }
; keeping 
    gcd[x,y] = gcd[X,Y] and x > 0 and y > 0
  do x > y -> x := x - y
  [] y > x -> 
     if true -> y := y - x 
     [] true -> y := y - 1
     fi
  od
  { x = gcd[X,Y] and y = gcd[X,Y] }
