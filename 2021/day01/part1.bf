PROGRAM Advent Of Code 2021 day 1 part 1

padding for checking the previous measurement and initial 0 >>

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
      move two times for acc of next step >>
    /if LF >>]<<
  /READLOOP ,]

  < go to acc
  << go to penultimate number for next step

  now we should have all numbers in memory separated by 0

COUNT NUMBER OF INCREASES
MEMORY LAYOUT: penultimate 0 number prev helper/0 1 0 result

if previous measurement [
  copy it to prev and helper [- >>>+>+ <<<<]
  write it back from helper >>>>[-<<<<+>>>>]<<<<

  go to number ensure not zero >>+
  go to prev ensure not zero >+

  add 1 for position check and >>+<<

  starting at prev

  decrease and end up on correct spot [-<-[<]>>]
  we end up on the 1 if prev is bigger and on prev otherwise

  >[-] no increase do nothing

  >[-> increase
    increment result >>+<<
  ]

  back to prev and zero out <<<[-]
  back to number and zero out <[-]

  move result by two steps >>>>>>[-<<+>>]

  move to penultimate <<<<<<<<

  move further back to be on penultimate for next iteration <<

/if previous measurement  ]

clear number from last iteration >>[-]

point to result >>>>>>


Next section is copied from esolangs (dot) org:

// Print value
// Cells used: V Z n d 1 0 0 0
// V is the value you need to print; it is not modified
// Z is a zero sentinal and tmp
// All cells Z and up are cleared by this routine

>[-]>[-]+>[-]+<                         // Set n and d to one to start loop
[                                       // Loop on 'n'
    >[-<-                               // On the first loop
        <<[->+>+<<]                     // Copy V into N (and Z)
        >[-<+>]>>                       // Restore V from Z
    ]
    ++++++++++>[-]+>[-]>[-]>[-]<<<<<    // Init for the division by 10
    [->-[>+>>]>[[-<+>]+>+>>]<<<<<]      // full division
    >>-[-<<+>>]                         // store remainder into n
    <[-]++++++++[-<++++++>]             // make it an ASCII digit; clear d
    >>[-<<+>>]                          // move quotient into d
    <<                                  // shuffle; new n is where d was and
                                        //   old n is a digit
    ]                                   // end loop when n is zero
<[.[-]<]                                // Move to were Z should be and
                                        // output the digits till we find Z
<                                       // Back to V



Finally clear out register:

[-]


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