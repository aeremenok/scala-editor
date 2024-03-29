package test

import org.testng.annotations.Test
import org.editor.tokens.Program
import org.editor.{RichString, SimpleJava}

/**
 * @author eav
 * Date: 14.08.11
 * Time: 20:26
 */
class ParserTest {
  //  @Test
  def goodClasses( ) {
    val p: Program = SimpleJava.parse("""

    class Person {
      void getName(){}

      void getAge(){}

      Account getAccount(){}
    }

    class Account {
      void getId(){}
    }

    """).get
    println(p)

    val linkageErrors = p.resolve()
    assert(linkageErrors.isEmpty, linkageErrors)
  }

  //  @Test
  def linkageErrors( ) {
    val p: Program = SimpleJava.parse("""

    class Person {
      Account getAccount(){}
    }

    """).get
    println(p)

    val linkageErrors = p.resolve()
    println(linkageErrors)
    assert(linkageErrors.size == 1, linkageErrors)
  }

  @Test
  def rename( ) {
    val code = """

    class Person {
      Person getFriend(){}
    }

    """

    val p: Program = SimpleJava.parse(code).get
    println(p)

    val linkageErrors = p.resolve()
    assert(linkageErrors.isEmpty, linkageErrors)

    assert(p.hasClass("Person"))

    val newCode = p.renameClass("Person", "Dude", code)
    println(newCode)

    val newProgram = SimpleJava.parse(newCode).get
    assert(newProgram.resolve().isEmpty)
    assert(newProgram.hasClass("Dude"))
  }
}
