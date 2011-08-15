package org.editor.tokens

import collection.immutable.HashMap

/**
 * @author eav
 * Date: 14.08.11
 * Time: 18:10
 */
trait Token

class Program( val classes: List[Clazz] ) extends Token {
  private val classesByNames: Map[String, Clazz] = classes map {c => (c.name, c)} toMap

  def getClassByName( name: String ) = classesByNames.get(name)

  override def toString = "Program:\n" + classes.mkString("\n")
}

class Clazz( var name: String, val members: List[Member] ) extends Token {
  override def toString = "\tClass " + name + ", members:\n" + members.mkString("\n")
}

trait Member extends Token

class Method( var name: String ) extends Member {
  override def toString = "\t\tMethod " + name
}
