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

trait Member extends Token with Visitable {
  def resolve( typeRegistry: Map[String, Type] ): List[LinkageError]

  protected def children = Nil
}

trait Visitable {
  def accept( v: Visitor ) {
    v.visit(this)
    children foreach {_.accept(v)}
  }

  protected def children: List[Visitable]
}

trait Visitor {
  def visit( v: Visitable )
}

case class LinkageError( error: String, wrongString: RichString )

class Program( val classes: List[Clazz] ) extends Token with Visitable {
  protected def children = classes

  def resolve( ): List[LinkageError] = {
    val classRegistry: Map[String, Type] = classes map {c => (c.name.string, c)} toMap
    val typeRegistry: Map[String, Type] = classRegistry ++ Map("void" -> Void)

    classes flatMap {_.resolve(typeRegistry)}
  }

  override def toString = "Program:\n" + classes.mkString("\n")
}

class Clazz( val name: RichString, val members: List[Member] ) extends Type with Visitable {
  protected def children = members

  def resolve( typeRegistry: Map[String, Type] ): List[LinkageError] =
    members flatMap {_.resolve(typeRegistry)}

  override def toString = "\tClass " + name + ", members:\n" + members.mkString("\n")
}

class Method( val returnTypeName: RichString, val name: RichString ) extends Member {
  var returnType: Type = _

  def resolve( typeRegistry: Map[String, Type] ): List[LinkageError] =
    typeRegistry.get(returnTypeName.string) match {
      case Some(retType) => returnType = retType; Nil
      case None => LinkageError("type not found: " + returnTypeName.string, returnTypeName) :: Nil
    }

  override def toString = "\t\tMethod " + name + ":" + returnTypeName
}
