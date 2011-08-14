package org.editor.tokens

/**
 * @author eav
 * Date: 14.08.11
 * Time: 18:10
 */
trait Token

class Program( val classes: List[Clazz] ) extends Token {
  override def toString = "Program:\n" + classes.toString()
}

class Clazz( var name: String, val members: List[Member] ) extends Token {
  override def toString = "\n\tClass " + name + ", members:" + members
}

trait Member extends Token

class Method( var name: String ) extends Member {
  override def toString = "\n\t\tMethod " + name
}
