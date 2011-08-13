package org.editor

import javax.swing.JTextPane
import swing.{Component, MainFrame, SwingApplication}
import java.awt.Dimension

/**
 * @author eav
 * Date: 13.08.11
 * Time: 22:31
 */
object app extends SwingApplication {
  def startup( args: Array[String] ) {
    appMainFrame.visible = true
  }
}

object appMainFrame extends MainFrame {
  contents = Component wrap new JTextPane()

  minimumSize = new Dimension(600, 600)
  pack()
  centerOnScreen()

  override def closeOperation( ) {
    app.quit()
  }
}
