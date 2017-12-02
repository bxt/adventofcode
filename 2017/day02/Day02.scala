import scala.io.Source

object Day02 {
   def main(args: Array[String]): Unit = {    
    val input = Source.fromResource("input.txt").getLines()
    println(input.map(line => {
      val numberList = line.split("\\s+").map(_.toInt)
      numberList.max - numberList.min
    }).sum) // 50376
  }
}