package org.editor

import tokens.{Method, Program, Clazz}
import util.parsing.combinator.JavaTokenParsers

/**
 * @author eav
 * Date: 13.08.11
 * Time: 22:42
 */
object SimpleJava extends DebugStandardTokenParsers {
  implicit def toWrapped( name: String ) = new {
    def !!![T]( p: Parser[T] ) = new Wrap(name, p)
  }

  val ID  = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r
  val NUM = """[1-9][0-9]*""".r

  def program = "PROGRAM" !!!
                clazz ^^ {case clazz: Clazz => new Program(clazz :: Nil)}

  def clazz = "CLASS" !!!
              "class" ~> ID ~ "{" ~ member <~ "}" ^^ {
    case ~(~(name, _), member) => new Clazz(name, member :: Nil)
  }

  def member = "MEMBER" !!!
               "void" ~> ID <~ "(" <~ ")" <~ "{" <~ "}" ^^ {name => new Method(name.toString)}

  def parse( text: String ) = parseAll(program, text)
}

trait DebugStandardTokenParsers extends JavaTokenParsers {

  class Wrap[+T]( name: String, parser: Parser[T] ) extends Parser[T] {
    def apply( in: Input ): ParseResult[T] = {
      val first = in.first
      val pos = in.pos
      val offset = in.offset
      val t = parser.apply(in)
      println(name + " for token " + first +
              " at position " + pos + " offset " + offset + " returns " + t)
      t
    }
  }

}
