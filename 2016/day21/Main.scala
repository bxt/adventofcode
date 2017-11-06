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

  def unscramble(input: String): String = {
    Function.chain(instructions().map(unscrambleStep).toSeq.reverse)(input)
  }
  
  def unscrambleStep(line: String): String => String = {
    line match {
      case rotateLeft(steps) => rotate(_, -steps.toInt)
      case rotateRight(steps) => rotate(_, steps.toInt)
      case specialRotate(letter) => unSpecialRotated(_, letter)
      case move(from, to) => moved(_, to.toInt, from.toInt)
      case other => scrambleStep(other)
    }
  }

  def scrambleStep(line: String): String => String = {
    line match {
      case swapPosition(from, to) => (s => swap_letter(s, s(from.toInt), s(to.toInt)))
      case swapLetter(letter1, letter2) => swap_letter(_, letter1.head, letter2.head)
      case rotateLeft(steps) => rotate(_, steps.toInt)
      case rotateRight(steps) => rotate(_, -steps.toInt)
      case specialRotate(letter) => specialRotated(_, letter)
      case reverse(from, to) => reversed(_, from.toInt, to.toInt)
      case move(from, to) => moved(_, from.toInt, to.toInt)
    }
  }

  def swap_letter(input: String, letter1: Char, letter2: Char): String = {
    input.map(char => {
      char match {
        case `letter1` => letter2
        case `letter2` => letter1
        case anyLetter => anyLetter
      }
    });
  }

  def rotate(input: String, steps: Int): String = {
    val normalizedSteps = ((steps % input.length()) + input.length()) % input.length()
    input.drop(normalizedSteps) ++ input.take(normalizedSteps)
  }

  def reversed(input: String, from: Int, to: Int): String = {
    val length = to - from + 1
    input.patch(from, input.drop(from).take(length).reverse, length)
  }

  def moved(input: String, from: Int, to: Int): String = {
    val letter = input(from)
    input.patch(from, Nil, 1).patch(to, letter.toString, 0)
  }

  def specialRotated(input: String, letter: String): String = {
    val index = input.indexOf(letter)
    val steps = specialRotateSteps(index)
    rotate(input, -steps)
  }
  
  def unSpecialRotated(input: String, letter: String): String = {
    val index = input.indexOf(letter)
    val steps = (0 to input.length()-1).filter(indexBefore => {
      (indexBefore + specialRotateSteps(indexBefore)) % input.length() == index
    }).head
    rotate(input, index - steps)
  }
  
  def specialRotateSteps(index: Int): Int = {
    index + 1 + (if (index >= 4) 1 else 0)
  }
}
