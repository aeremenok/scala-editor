package org.editor

import tokens._
import util.parsing.combinator.JavaTokenParsers
import util.matching.Regex

/**
 * @author eav
 * Date: 13.08.11
 * Time: 22:42
 */
class SimpleJava extends DebugStandardTokenParsers with CodeFactory {
  implicit def toWrapped( name: String ) = new {
    def :?[T]( p: Parser[T] ) = new Wrap(name, p)
  }

  val ID  = regex1("""[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r)
  val NUM = """[1-9][0-9]*""".r

  def program = "PROGRAM" :?
                clazz.* ^^ newProgram

  def clazz = "CLASS" :?
              "class" ~> ID ~ "{" ~ member.* <~ "}" ^^ {
    case ~(~(name, _), members) => newClass(name, members)
  }

  def member = "MEMBER" :?
               ID ~ ID <~ "(" <~ ")" <~ "{" <~ "}" ^^ {
    case ~(typeName, methodName) => newMethod(typeName, methodName)
  }

  def parse( text: String ) = parseAll(program, text)
}

trait CodeFactory {
  def newProgram( classes: List[Clazz] ) = new Program(classes)

  def newClass( className: RichString, classMembers: List[Member] ) = new Clazz(className, classMembers)

  def newMethod( returnType: RichString, methodName: RichString ) = new Method(returnType, methodName)
}

case class RichString( string: String, start: Int, offset: Int )

trait DebugStandardTokenParsers extends JavaTokenParsers {
  def regex1( r: Regex ): Parser[RichString] = new Parser[RichString] {
    def apply( in: Input ) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      ( r findPrefixMatchOf ( source.subSequence(start, source.length) ) ) match {
        case Some(matched) => {
          val string = source.subSequence(start, start + matched.end).toString
          val richString = RichString(string, start, matched.end)
          Success(richString, in.drop(start + matched.end - offset))
        }
        case None =>
          val found = if ( start == source.length() ) "end of source" else "`" + source.charAt(start) + "'"
          Failure("string matching regex `" + r + "' expected but " + found + " found", in.drop(start - offset))
      }
    }
  }

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
