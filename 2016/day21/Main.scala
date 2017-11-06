import scala.io.Source

object Main {
  val swapPosition = "swap position (\\d+) with position (\\d+)".r
  val swapLetter = "swap letter ([a-z]) with letter ([a-z])".r
  val rotateLeft = "rotate left (\\d+) steps?".r
  val rotateRight = "rotate right (\\d+) steps?".r
  val specialRotate = "rotate based on position of letter ([a-z])".r
  val reverse = "reverse positions (\\d+) through (\\d+)".r
  val move = "move position (\\d+) to position (\\d+)".r

  def main(args: Array[String]): Unit = {
    println(scramble("abcdefgh")) // -> gfdhebac
    println(unscramble("fbgdceah")) // -> dhaegfbc
  }
  
  def instructions() = {
    Source.fromResource("input.txt").getLines()
  }

  def scramble(input: String): String = {
    Function.chain(instructions().map(scrambleStep).toSeq)(input)
  }

  def scrambleStep(line: String): String => String = {
    line match {
      case swapPosition(from, to) => (s => swapLetter(s(from.toInt), s(to.toInt))(s))
      case swapLetter(letter1, letter2) => swapLetter(letter1.head, letter2.head)
      case rotateLeft(steps) => rotate(steps.toInt)
      case rotateRight(steps) => rotate(-steps.toInt)
      case specialRotate(letter) => specialRotated(_, letter)
      case reverse(from, to) => reverseRange(from.toInt, to.toInt)
      case move(from, to) => moveIndex(from.toInt, to.toInt)
    }
  }

  def unscramble(input: String): String = {
    Function.chain(instructions().map(unscrambleStep).toSeq.reverse)(input)
  }
  
  def unscrambleStep(line: String): String => String = {
    line match {
      case rotateLeft(steps) => rotate(-steps.toInt)
      case rotateRight(steps) => rotate(steps.toInt)
      case specialRotate(letter) => unSpecialRotated(_, letter)
      case move(from, to) => moveIndex(to.toInt, from.toInt)
      case other => scrambleStep(other)
    }
  }

  def swapLetter(letter1: Char, letter2: Char)(input: String): String = {
    input.map(_ match {
      case `letter1` => letter2
      case `letter2` => letter1
      case anyLetter => anyLetter
    });
  }

  def rotate(steps: Int)(input: String): String = {
    val size = input.length()
    val normalizedSteps = ((steps % size) + size) % size
    input.drop(normalizedSteps) ++ input.take(normalizedSteps)
  }

  def reverseRange(from: Int, to: Int)(input: String): String = {
    val length = to - from + 1
    input.patch(from, input.drop(from).take(length).reverse, length)
  }

  def moveIndex(from: Int, to: Int)(input: String): String = {
    val letter = input(from)
    input.patch(from, Nil, 1).patch(to, letter.toString, 0)
  }

  def specialRotated(input: String, letter: String): String = {
    val index = input.indexOf(letter)
    val steps = specialRotateSteps(index)
    rotate(-steps)(input)
  }
  
  def unSpecialRotated(input: String, letter: String): String = {
    val index = input.indexOf(letter)
    val steps = (0 to input.length()-1).filter(indexBefore => {
      (indexBefore + specialRotateSteps(indexBefore)) % input.length() == index
    }).head
    rotate(index - steps)(input)
  }
  
  def specialRotateSteps(index: Int): Int = {
    index + 1 + (if (index >= 4) 1 else 0)
  }
}
