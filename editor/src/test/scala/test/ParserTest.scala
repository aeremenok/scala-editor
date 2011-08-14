package test

import org.testng.annotations.Test
import org.editor.SimpleJava

/**
 * @author eav
 * Date: 14.08.11
 * Time: 20:26
 */
class ParserTest {
  @Test
  def test( ) {
    val p = SimpleJava.parse("""

    class Person {
      void getName(){}
    }

    """)
    println(p)
  }
}
