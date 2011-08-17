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
  protected[tokens] def resolve( typeRegistry: Map[String, Type] ): List[LinkageError]

  protected[tokens] def getClassOccurences( className: String ): List[RichString]

  protected def children = Nil
}

case class LinkageError( error: String, wrongString: RichString )

class Program( val classes: List[Clazz] ) extends Token with Visitable {
  private val classRegistry: Map[String, Type] = classes map {c => (c.name.string, c)} toMap

  protected def children = classes

  def resolve( ): List[LinkageError] = {
    val typeRegistry: Map[String, Type] = classRegistry ++ Map("void" -> Void)

    classes flatMap {_.resolve(typeRegistry)}
  }

  def hasClass( name: String ): Boolean = classRegistry.contains(name)

  def renameClass( oldClassName: String, newClassName: String, code: String ): String =
    getClassOccurences(oldClassName).sortBy(_.start).foldRight(code) {( occurence, wholeCode ) =>
      occurence.replace(newClassName, wholeCode)
                                                                     }

  private def getClassOccurences( className: String ): List[RichString] = {
    classRegistry.get(className) match {
      case Some(clazz: Clazz) => {
        val otherOccurences = classes flatMap {_.getClassOccurences(className)}
        clazz.name :: otherOccurences
      }
      case _ => Nil
    }
  }

  override def toString = "Program:\n" + classes.mkString("\n")
}

class Clazz( val name: RichString, val members: List[Member] ) extends Type with Visitable {
  protected def children = members

  protected[tokens] def resolve( typeRegistry: Map[String, Type] ): List[LinkageError] =
    members flatMap {_.resolve(typeRegistry)}

  protected[tokens] def getClassOccurences( className: String ): List[RichString] =
    members flatMap {_.getClassOccurences(className)}

  override def toString = "\tClass " + name + ", members:\n" + members.mkString("\n")
}

class Method( val returnTypeName: RichString, val name: RichString ) extends Member {
  var returnType: Type = _

  protected[tokens] def resolve( typeRegistry: Map[String, Type] ): List[LinkageError] =
    typeRegistry.get(returnTypeName.string) match {
      case Some(retType) => {
        returnType = retType
        Nil
      }
      case None => LinkageError("type not found: " + returnTypeName.string, returnTypeName) :: Nil
    }

  override def toString = "\t\tMethod " + name + ":" + returnTypeName

  protected[tokens] def getClassOccurences( className: String ) = returnType match {
    case c: Clazz if ( c.name.string == className ) => returnTypeName :: Nil
    case _ => Nil
  }
}
