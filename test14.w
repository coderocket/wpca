x,y,z:int
	{ x = X and y = Y }
; if x >= y -> z := x 
  [] y >= x -> skip
  fi
	{ x = X and y = Y and z = max[x,y] }

