package day20

import scala.io.Source

object Main {
  val ITERATIONS = 50000
  val lineRx = raw"p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>".r

  case class Vec3(x: Int, y: Int, z: Int) {
    def +(other: Vec3): Vec3 = {
      Vec3(x + other.x, y + other.y, z + other.z)
    }

    def manhattan = x.abs + y.abs + z.abs
  }

  case class Particle(id: Int, position: Vec3, velocity: Vec3, acceleration: Vec3) {
    def tick: Particle = {
      val newVelocity = velocity + acceleration
      Particle(id, position + newVelocity, newVelocity, acceleration)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day20/input.txt").getLines()
    val particles = input.zipWithIndex.map(parse _).toList

    println(particles.minBy(p => (p.acceleration.manhattan, p.velocity.manhattan, p.position.manhattan)).id)

    val ticks = iterate[List[Particle]](particles => removeColliding(particles).map(_.tick))(particles)

    println(ticks(ITERATIONS).length)
  }

  def iterate[A](f: A => A)(start: A): Stream[A] = {
    def tail(a: A): Stream[A] = a #:: tail(f(a))
    tail(start)
  }

  def parse(tuple: (String, Int)): Particle = tuple match {
    case (input, id) => input match {
      case lineRx(pxStr, pyStr, pzStr, vxStr, vyStr, vzStr, axStr, ayStr, azStr) =>
        Particle(id, Vec3(pxStr.toInt, pyStr.toInt, pzStr.toInt), Vec3(vxStr.toInt, vyStr.toInt, vzStr.toInt), Vec3(axStr.toInt, ayStr.toInt, azStr.toInt))

    }
  }

  def removeColliding(particles: List[Particle]): List[Particle] = {
    particles.groupBy(_.position).filter({ case (x, l) => l.length == 1 }).values.flatten.toList
  }
}
