PROGRAM: COUNT BRACES
MEMORY LAYOUT: didnt CHAR h1 count h2 h3 neg

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
| | | >> scope count
| | | | >+< set h2 to 1
| | | | [>-]>[-> h2 = 0 and scope h3; if count = 0
| | | | >+< set neg
| | | | ] end if count = 0
| | | | << end scope h3
| | | | - decrease count
| | | << end scope count
| | <]>
| ]
| <[-> open brace: didnt? so did
| | >> scope count
| | | + increment count
| | | >+< set h2 to 1
| | | [>-]>[-> h2 = 0 and scope h3; if count = 0
| | | >-< unset neg
| | | ] end if count = 0
| | | << end scope h3
| | << end scope count
| <]>
| <+> didnt
[-],]

<-> did

>>>>>[-<<< if neg scope count and neg=0
| >>+++++++++[<+++++>-]<< put 45 into h2 using h3
| >.< print a minus with ascii 45 in h2
| >[-]< h2 = 0
| [+>+<] h2 = minus count
| >[-<+>]< and copy h2 to count
>>>]<<<<< end if neg scope count

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
