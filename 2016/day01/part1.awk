function abs(x){return ((x < 0.0) ? -x : x)}

BEGIN { RS=","; DX=1; DY=0; X=0; Y=0 }
/L/   { TMP=DX; DX=-DY; DY=TMP }
/R/   { TMP=DX; DX=DY; DY=-TMP }
      { Y += DY * substr($1, 2, length($1)-1) }
      { X += DX * substr($1, 2, length($1)-1) }
END   { print abs(X)+abs(Y) }
