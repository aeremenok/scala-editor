package org.editor

import tokens._
import util.parsing.combinator.JavaTokenParsers
import util.matching.Regex

/**
 * @author eav
 * Date: 13.08.11
 * Time: 22:42
 */
object SimpleJava extends DebugStandardTokenParsers with CodeFactory {
  val ID  = richStringRegex("""[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r)
  val NUM = """[1-9][0-9]*""".r

  def program = clazz.* ^^ newProgram

  def clazz = "class" ~> ID ~ "{" ~ member.* <~ "}" ^^ {
    case ~(~(name, _), members) => newClass(name, members)
  }

  def member = ID ~ ID <~ "(" <~ ")" <~ "{" <~ "}" ^^ {
    case ~(typeName, methodName) => newMethod(typeName, methodName)
  }

  def parse( text: String ) = parseAll(program, text)

  def tryParsing( text: String ): Option[(Program, List[LinkageError])] = parse(text) match {
    case Success(prog: Program, _) => Some(prog, prog.resolve())
    case _ => None
  }
}

trait CodeFactory {
  def newProgram( classes: List[Class] ) = new Program(classes)

  def newClass( className: RichString, classMembers: List[Member] ) = new Class(className, classMembers)

  def newMethod( returnType: RichString, methodName: RichString ) = new Method(returnType, methodName)
}

case class RichString( string: String, start: Int, offset: Int ) {
  def replace( newString: String, wholeText: String ): String = {
    val textStart = wholeText.substring(0, start)
    val textEnd = wholeText.substring(start + offset)
    textStart + newString + textEnd
  }
}

trait DebugStandardTokenParsers extends JavaTokenParsers {
  def richStringRegex( r: Regex ): Parser[RichString] = new Parser[RichString] {
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
}
