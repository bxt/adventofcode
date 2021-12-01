PROGRAM Advent Of Code 2021 day 1 part 1

READ IN NUMBERS
MEMORY LAYOUT: acc read helper lfCheck

  move on from acc >

  READLOOP ,[
      set lfCheck to true >>+<<
      minus 10 ----------
      not LF [
        set lfCheck to false >>-<<
        minus 38 to obtain number >++++++[-<------>]<--
        multiply acc by 10
          goto acc <
          copy acc [->>+<<]
          put 10 for each >>[-<<++++++++++>>]<<
          go back to number >
        add number to acc [-<+>]
    /not LF ]
    if LF >>[-<<
      move on two steps >>
    /if LF >>]<<
  /READLOOP ,]

  NOW WE SHOULD HAVE ALL NUMBERS IN MEMORY

COUNT NUMBER OF INCREASES
MEMORY LAYOUT: tbd



EXAMPLE INPUT:

199
200
208
210
200
207
240
269
260
263

EXAMPLE OUTPUT: 7