package test

import org.specs2.mutable.Specification
import org.editor.SimpleScala

/**
 * @author eav
 * Date: 13.08.11
 * Time: 22:58
 */
class ParserSpec extends Specification {
  "Simple Scala" should {
    "parse simple class" in {

      SimpleScala.parse("""

      class Person() {
        val age: Int = (0 + 1)
      }

      """) must_== ""
    }
  }
}
