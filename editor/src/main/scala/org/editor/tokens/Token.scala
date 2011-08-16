package org.editor.tokens

import org.editor.RichString

/**
 * @author eav
 * Date: 14.08.11
 * Time: 18:10
 */
trait Token

trait Type extends Token

object Void extends Type

trait Member extends Token {
  def resolve( typeRegistry: Map[String, Type] ): List[LinkageError]
}

class Program( val classes: List[Clazz] ) extends Token {
  override def toString = "Program:\n" + classes.mkString("\n")

  def resolve( ): List[LinkageError] = {
    val classRegistry: Map[String, Type] = classes map {c => (c.name.string, c)} toMap
    val typeRegistry: Map[String, Type] = classRegistry ++ Map("void" -> Void)

    classes flatMap {_.resolve(typeRegistry)}
  }
}

class Clazz( val name: RichString, val members: List[Member] ) extends Type {
  override def toString = "\tClass " + name + ", members:\n" + members.mkString("\n")

  def resolve( typeRegistry: Map[String, Type] ): List[LinkageError] =
    members flatMap {_.resolve(typeRegistry)}
}

class Method( val returnTypeName: RichString, val name: RichString ) extends Member {
  var returnType: Type = _

  override def toString = "\t\tMethod " + name + ":" + returnTypeName

  def resolve( typeRegistry: Map[String, Type] ): List[LinkageError] = {
    typeRegistry.get(returnTypeName.string) match {
      case Some(retType) => returnType = retType; Nil
      case None => LinkageError("type not found: " + returnTypeName.string) :: Nil
    }
  }
}

case class LinkageError( name: String )

