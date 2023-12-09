PPROGRAM: Advent of Code day 1 part 1

MEMORY LAYOUT:
sumOfFirstNumbers
sumOfLastNumbers
currentFirstNumber
currentLastNumber
lastReadChar
tmp1
charNeedsWork
currentNumber

now positions relative to lastReadChar >>>>

READLOOP ,[

  charNeedsWork := true >>[-]+<<
  NOT "LF" ----------[
  NOT "1" >++++++[- <------>]<---[
  NOT "2" -[
  NOT "3" -[
  NOT "4" -[
  NOT "5" -[
  NOT "6" -[
  NOT "7" -[
  NOT "8" -[
  NOT "9" -[
    ignore other character
    charNeedsWork := false >> - <<
  /NOT "9" [-]]
    add 1 to currentNumber >>>+<<<
  /NOT "8" [-]]
    add 1 to currentNumber >>>+<<<
  /NOT "7" [-]]
    add 1 to currentNumber >>>+<<<
  /NOT "6" [-]]
    add 1 to currentNumber >>>+<<<
  /NOT "5" [-]]
    add 1 to currentNumber >>>+<<<
  /NOT "4" [-]]
    add 1 to currentNumber >>>+<<<
  /NOT "3" [-]]
    add 1 to currentNumber >>>+<<<
  /NOT "2" [-]]
    add 1 to currentNumber >>>+<<<
  /NOT "1" [-]]
    add 1 to currentNumber >>>+<<<

    if charNeedsWork? >>[ <<
      clean out currentLastNumber <[-]>
      copy currentNumber to tmp1 and currentLastNumber >>>[- << + << + >>>>]<<<

      if currentFirstNumber is not zero <<[>>
        clean out tmp1 >[-]<
      /endif currentFirstNumber is not zero <<]>>

      move tmp1 to currentFirstNumber >[-<<<+>>>]<

      charNeedsWork := false >> - <<
    /endif charNeedsWork? >>]<<

    clean out currentNumber >>>[-]<<<
  /NOT "LF" [-]]
  LF: if charNeedsWork? >>[ <<
    move currentFirstNumber to sumOfFirstNumbers <<[- << + >>]>>
    move currentLastNumber to sumOfLastNumbers <[- << + >>]>
    charNeedsWork := false >> - <<
  /LF endif charNeedsWork? >>]<<
/READLOOP ,]

now positions relative to sumOfFirstNumbers <<<<

move currentFirstNumber to sumOfFirstNumbers >>[- << + >>]<<
move currentLastNumber to sumOfLastNumbers >>>[- << + >>]<<<

move sumOfFirstNumbers ten times to sumOfLastNumbers [- >++++++++++<]