package test

import org.testng.annotations.Test
import org.editor.tokens.Program
import org.editor.{RichString, VerySimple, SimpleJava}

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
    // todo
    personClass.name = RichString("Person1", 0, 0)
  }
}
