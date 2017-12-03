package day03

import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

object Visualisation {
  def main(args: Array[String]): Unit = {
    val size = 2560
    val max = (size-1)*(size-1)
    val c = Main.Point(size/2, size/2)
    val isPrime = primesUpTo(max)

    val out = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)

    for (index <- 1 to max) {
      if(isPrime(index)) {
        val p = Main.gridCoords(index) + c
        out.setRGB(p.x, p.y, 0xffffff)
      }
    }

    ImageIO.write(out, "jpg", new File("test_qwertz.jpg"))

    println("Done.")
  }

  def primesUpTo(n: Int) = {
    val isPrime = collection.mutable.BitSet(2 to n: _*) -- (4 to n by 2)
    for (p <- 2 +: (3 to Math.sqrt(n).toInt by 2) if isPrime(p)) {
      isPrime --= p * p to n by p
    }
    isPrime
  }
}
