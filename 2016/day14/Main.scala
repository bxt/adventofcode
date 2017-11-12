import javax.xml.bind.DatatypeConverter.printHexBinary
import java.security.MessageDigest

object Day14 {
  val salt = "yjdafjpo"
  //val salt = "abc"
  
  def main(args: Array[String]): Unit = {
    time {
      println(findNthKeyIndex(63, index => getMd5(salt + index))) // -> 25427
      println(findNthKeyIndex(63, getStretchedMd5)) // -> 22045
    }
  }
  
  def findNthKeyIndex(n: Int, hasher: Int => String) = {
    generateKeys(hasher).drop(n).next._2
  }
  
  def generateKeys(hasher: Int => String): Iterator[(String, Int)] = {
    val hashes: Stream[String] = Stream.from(0).map(hasher)
    
    hashes.zipWithIndex.sliding(1001).filter(group => {
      val (hash, index) +: next1k = group
      
      findRepeatedChar(3)(hash).flatMap(repeatedChar => {
        //println(index + ": " + hash)
        next1k.find({case (hash, index) =>
          findRepeatedChar(5)(hash).filter(_ == repeatedChar).isDefined
        })
      }).isDefined
    }).map(_.head)
  }
  
  def findRepeatedChar(times: Int)(input: String): Option[Char] = {
    f"(.)\\1{${times - 1}%d}".r.findFirstIn(input).map(_.charAt(0))
  }
  
  def getMd5(input: String): String = {
    printHexBinary(MessageDigest.getInstance("MD5").digest(input.getBytes)).toLowerCase
  }
  
  def getStretchedMd5(index: Int): String = {
    Iterator.iterate(salt + index)(getMd5).drop(2017).next
  }
  
  def time[R](block: => R): R = {  
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
}