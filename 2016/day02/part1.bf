PPROGRAM: Advent of Code day 2 part 1
MEMORY LAYOUT: CHAR h1 DIDNT x y h2

READLOOP ,[

DIDNT >>[-]+<<
NOT LF ----------[
NOT D >+++++++[<-------->-]<--[
NOT L --------[
NOT R ------[
NOT U ---[
  RESTORE >++++++++++[<++++++++>-]<+++++
  PRINT .
  DID >>-<<
/NOT U [-]]
U: DIDNT? >>[ DID- <<
  decrease y if y !=0
    move y to h1 and h2 >>>>[->+<<<<+>>>]<<<<
    restore y from h1 >[->>>+<<<]<
    if h2 != 0 decrease x; h2=0 >>>>>[<->[-]]<<<<<
  /decrease
/U /DIDNT? >>]<<
/NOT R [-]]
R: DIDNT? >>[ DID- <<
  increase x >>>+<<<
/R /DIDNT? >>]<<
/NOT L [-]]
L: DIDNT? >>[ DID- <<
  decrease x if x !=0
    move x to h1 and h2 >>>[->>+<<<<+>>]<<<
    restore x from h1 >[->>+<<]<
    if h2 != 0 decrease x >>>>>[<<->>[-]]<<<<<
  /decrease
/L /DIDNT? >>]<<
/NOT D [-]]
D: DIDNT? >>[ DID- <<
  increase y >>>>+<<<<
/D /DIDNT? >>]<<
/NOT LF [-]]
LF: DIDNT? >>[ DID- <<
  move x to h1 and h2 >>>[->>+<<<<+>>]<<<
  restore x from h1 >[->>+<<]<
  move y to h1 and 3*y to h2 >>>>[->+++<<<<+>>>]<<<<
  restore y from h1 >[->>>+<<<]<
  dec2str h2 using h1 >++++++[->>>>++++++++<<<<]<
  print h2 >>>>>.[-]<<<<<
/LF /DIDNT? >>]<<
/READLOOP ,]



EXAMPLE INPUT:

ULL
RRDDD
LURDL
UUUUD

EXAMPLE OUTPUT:

1985
