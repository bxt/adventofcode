PROGRAM: COUNT BRACES
MEMORY LAYOUT: didnt CHAR h1 count

+> set/skip didnt

,[
| >++++++++[<----->-]< sub 40 using h1
|
| [ not an open brace
| | -[<-> not a close brace so did
| | | >++++++++[<+++++>-]<+ restore: add 41 using h1
| | | . print
| | [-]]
| | <[-> close brace: didnt? so did
| | | >>-<< decrease count
| | <]>
| ]
| <[-> open brace: didnt? so did
| | >>+<< increment count
| <]>
| <+> didnt
[-],]

<-> did

>> go to count

SUBPROGRAM: PRINT NUMBER
MEMORY LAYOUT: didnt NUM

<+> set didnt
[<-> there is a number so did
| [ while we have a number x to work on
| | >++++++++++< put 10 into y operand

SUBPROGRAM: DIVMOD
MEMORY LAYOUT: x y x%y x/y 01 02
(x will be zero after run)

[ while x greater 0
| - decrease x
| >- decease y
| [ when y is still greater 0
| | >+ backup y to x%y
| | >> jump to 01
| ]
| > when y was greater 0 jump to 02
|   else to x%y which is y minus 1 now
| [ y fitted another time into x
| | +[-<+>] restore y from x%y
| | >+ increase x/y by 1
| | >> jump to 02
| ]
| <<<<< back to x
]
END SUBPROGRAM: DIVMOD

| | >[-]< zero old y operand
| | >>[-<<+>>]<< add x%10 to x
| | >>>[-<<+>>]<<< move x/10 to next position
| | +> add dummy 1 and shift by 1
| ]
| > jump one further so next section will find 0 for didnt
]
<[-> when didnt
| +>> just a dummy 1 and shift by 2
<]>

<< point to the last (digit plus 1)
[ while we have any digits
| - subtract the dummy 1
| >++++++++[<++++++>-]<  add ascii 0 = 48
| . print
| [-] zero out
| < jump to next digit
]

END SUBPROGRAM: PRINT NUMBER

<< go back to start

END PROGRAM: COUNT BRACES
