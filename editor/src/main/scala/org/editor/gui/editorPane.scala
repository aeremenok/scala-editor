package org.editor.gui

import javax.swing.JTextPane
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter
import java.awt.{Font, Color}
import org.editor.{RichString, SimpleJava}
import java.awt.event.{MouseEvent, MouseAdapter}
import org.editor.tokens._
import swing.{Component, Dialog}

/**
 * @author eav
 * Date: 13.08.11
 * Time: 22:31
 */
object editorPane extends JTextPane {
  val thisComponent = Component wrap this
  private var currentProgram: Option[Program] = None
  setFont(new Font(Font.MONOSPACED, Font.BOLD, 15))

  getDocument.addDocumentListener(new DocumentListener {
    def changedUpdate( e: DocumentEvent ) {onChange()}

    def removeUpdate( e: DocumentEvent ) {onChange()}

    def insertUpdate( e: DocumentEvent ) {onChange()}
  })

  addMouseListener(new MouseAdapter {
    override def mouseClicked( e: MouseEvent ) {
      if ( e.getButton == MouseEvent.BUTTON3 ) tryRenaming(getSelectedText.trim())
    }
  })

  private def tryRenaming( selectedText: String ) {
    currentProgram match {
      case Some(program) if ( program.hasClass(selectedText) ) =>
        Dialog
        .showInput(parent = thisComponent,
                   message = "Enter new class name",
                   title = "Rename",
                   initial = selectedText) match {
          case Some(newClassName) if ( newClassName != selectedText ) =>
            setText(program.renameClass(selectedText, newClassName, getText))
          case _ => {}
        }
      case _ => {}
    }
  }

  private def onChange( ) {
    SimpleJava.tryParsing(getText) match {
      case Some((program, errors)) => {
        currentProgram = Some(program)

        getHighlighter.removeAllHighlights()
        program.accept(highlightProgramVisitor)

        errors foreach {e => highlight(e.wrongString, errorColor)}
      }
      case None => currentProgram = None
    }
  }

  private object highlightProgramVisitor extends Visitor {
    def visit( v: Visitable ) {
      v match {
        case c: Class => highlight(c.name, classColor)
        case m: Method => {
          highlight(m.name, methodColor)
          m.returnType match {
            case c: Class => highlight(m.returnTypeName, classColor)
            case _ => {}
          }
        }
        case _ => {}
      }
    }
  }

  private val classColor  = Color.GRAY
  private val methodColor = Color.LIGHT_GRAY
  private val errorColor  = Color.RED

  private def highlight( text: RichString, color: Color ) {
    getHighlighter.addHighlight(text.start, text.start + text.offset, new DefaultHighlightPainter(color))
  }
}
