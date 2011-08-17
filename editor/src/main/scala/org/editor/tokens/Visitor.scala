package org.editor.tokens

/**
 * @author eav
 * Date: 14.08.11
 * Time: 18:10
 */
trait Visitor {
  def visit( v: Visitable )
}

trait Visitable {
  def accept( v: Visitor ) {
    v.visit(this)
    children foreach {_.accept(v)}
  }

  protected def children: List[Visitable]
}




















