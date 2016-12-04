PPROGRAM: Advent of Code day 2 part 1
MEMORY LAYOUT: CHAR HELP DIDNT X Y

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
  decrease Y >>>>-<<<<
/U /DIDNT? >>]<<
/NOT R [-]]
R: DIDNT? >>[ DID- <<
  increase x >>>+<<<
/R /DIDNT? >>]<<
/NOT L [-]]
L: DIDNT? >>[ DID- <<
  decrease x >>>-<<<
/L /DIDNT? >>]<<
/NOT D [-]]
D: DIDNT? >>[ DID- <<
  increase y >>>>+<<<<
/D /DIDNT? >>]<<
/NOT LF [-]]
LF: DIDNT? >>[ DID- <<

/LF /DIDNT? >>]<<
/READLOOP ,]



EXAMPLE INPUT:

ULL
RRDDD
LURDL
UUUUD

EXAMPLE OUTPUT:

1985
