package test

import org.testng.annotations.Test
import org.editor.SimpleJava
import org.editor.tokens.Program

/**
 * @author eav
 * Date: 14.08.11
 * Time: 20:26
 */
class ParserTest {
  @Test
  def test( ) {
    val p: Program = SimpleJava.parse("""

    class Person {
      void getName(){}

      void getAge(){}
    }

    class Account {
      void getId(){}
    }

    """).get
    println(p)

    val personClass = p.getClassByName("Person").get
    personClass.name = "Person1"
  }
}
