package org.editor.gui

import javax.swing.JTextPane
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter
import java.awt.{Font, Color}
import org.editor.tokens.{Clazz, Method, Visitable, Visitor}
import org.editor.{RichString, SimpleJava}

/**
 * @author eav
 * Date: 13.08.11
 * Time: 22:31
 */
object editorPane extends JTextPane {
  setFont(new Font(Font.MONOSPACED, Font.BOLD, 15))

  getDocument.addDocumentListener(new DocumentListener {
    def changedUpdate( e: DocumentEvent ) {onChange()}

    def removeUpdate( e: DocumentEvent ) {onChange()}

    def insertUpdate( e: DocumentEvent ) {onChange()}
  })

  def onChange( ) {
    SimpleJava.tryParsing(getText) {( program, errors ) =>
      getHighlighter.removeAllHighlights()
      program.accept(programVisitor)
      errors foreach {e =>
        highlight(e.wrongString, errorColor)
      }
                                   }
  }

  object programVisitor extends Visitor {
    def visit( v: Visitable ) {
      v match {
        case c: Clazz => highlight(c.name, classColor)
        case m: Method => {
          highlight(m.name, methodColor)
          m.returnType match {
            case c: Clazz => highlight(m.returnTypeName, classColor)
            case _ => {}
          }
        }
        case _ => {}
      }
    }
  }

  val classColor  = Color.GRAY
  val methodColor = Color.LIGHT_GRAY
  val errorColor  = Color.RED

  def highlight( text: RichString, color: Color ) {
    getHighlighter.addHighlight(text.start, text.start + text.offset, new DefaultHighlightPainter(color))
  }
}
