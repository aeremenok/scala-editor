package org.editor

import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.combinator.{RegexParsers, JavaTokenParsers}

/**
 * @author eav
 * Date: 13.08.11
 * Time: 22:42
 */
object SimpleScala extends JavaTokenParsers {
  val ID  = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r
  val NUM = """[1-9][0-9]*""".r

  def program = clazz.*

  def classPrefix = "class" ~ ID ~ "(" ~ formals ~ ")"

  def classExt = "extends" ~ ID ~ "(" ~ actuals ~ ")"

  def clazz = classPrefix ~ opt(classExt) ~ "{" ~ ( member * ) ~ "}"

  def formals = repsep(ID ~ ":" ~ ID, ",")

  def actuals = expr.*

  def member = ( "val" ~ ID ~ ":" ~ ID ~ "=" ~ expr
                 | "var" ~ ID ~ ":" ~ ID ~ "=" ~ expr
                 | "def" ~ ID ~ "(" ~ formals ~ ")" ~ ":" ~ ID ~ "=" ~ expr
                 | "def" ~ ID ~ ":" ~ ID ~ "=" ~ expr
                 | "type" ~ ID ~ "=" ~ ID )

  def expr: Parser[Any] = ( factor ~ ( "+" ~ factor
                                       | "-" ~ factor ) ).*

  def factor = ( term ~ ( "." ~ ID ~ "(" ~ actuals ~ ")" ) ).*

  def term = ( "(" ~ expr ~ ")"
               | ID
               | NUM )

  def parse( text: String ) = parseAll(program, text)
}
