package day09

import scala.io.Source

abstract class StreamItem
case class Group(score: Int, children: List[StreamItem]) extends StreamItem
case class Garbage(content: String) extends StreamItem


object Main {
  def main(args: Array[String]): Unit = {
    assert(new StreamParser("<lo!l!><o>").parseStreamItem() == Garbage("lo<o"))
    assert(score(parse("{}")) == 1)
    assert(score(parse("{{},{}}")) == 5)
    assert(score(parse("{{{}}}")) == 6)
    assert(garbage(parse("{{{}}}")) == 0)
    assert(garbage(parse("{{{}}}")) == 0)

    val input = parse(Source.fromResource("day09/input.txt").mkString)
    println(score(input)) // 13154
    println(garbage(input)) // 6369
  }

  def parse(input: String): Group = {
    new StreamParser(input).parse()
  }

  def score(streamItem: StreamItem): Int = streamItem match {
    case Group(s, children) => s + children.map(score _).sum
    case Garbage(_) => 0
  }

  def garbage(streamItem: StreamItem): Int = streamItem match {
    case Group(_, children) => children.map(garbage _).sum
    case Garbage(content) => content.length
  }

  class StreamParser(input: String) extends Parser(input) {
    var depth = 0

    def parse = parseGroup _

    def parseGroup(): Group = {
      eat('{')
      depth += 1
      val children = parseChildren
      depth -= 1
      eat('}')
      Group(depth + 1, children)
    }

    def parseChildren(): List[StreamItem] = {
      look match {
        case '}' => List()
        case other => parseStreamItem() +: (look match {
          case ',' => { eat(','); parseChildren() }
          case other => List()
        })
      }
    }

    def parseStreamItem(): StreamItem = {
      look match {
        case '{' => parseGroup
        case '<' => parseGarbage
        case other => raise(other + " not a start of StreamItem")
      }
    }

    def parseGarbage(): Garbage = {
      eat('<')
      val contents = parseGarbageContent
      eat('>')
      Garbage(contents)
    }

    def parseGarbageContent(): String = {
      val sb = new StringBuilder
      while(look != '>') {
        look() match {
          case '!' => { eat('!'); eat() }
          case other => sb.append(eat())
        }
      }
      sb.mkString
    }
  }

 class Parser(input: String) {
    var position = 0

    def eat(expected: Char): Unit = {
      if (eat != expected) raise("Expected " + expected + " got " + look)
    }

    def eat(): Char = {
      val result = look
      position += 1
      result
    }

    def look(): Char = {
      input(position)
    }

    def raise(error: String): Nothing = {
      throw new IllegalArgumentException(error + " at " + position)
    }
  }

}
