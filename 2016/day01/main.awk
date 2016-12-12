
function abs(x){
  return ((x < 0.0) ? -x : x)
}

BEGIN { RS = ","; DX = 1; DY = 0; X = 0; Y = 0 }
/L/   { TMP = DX; DX = -DY; DY = TMP }
/R/   { TMP = DX; DX = DY; DY = -TMP }
      {
        CNT = substr($1, 2, length($1)-1)
        while (CNT--) {
          Y += DY; X += DX
          if(!FOUND) {
            if((X, Y) in SEEN) {
              print "Part Two: " abs(X) + abs(Y)
              FOUND = 1
            }
            SEEN[X, Y] = 1
          }
        }
      }
END   { print "Part One: " abs(X) + abs(Y) }
