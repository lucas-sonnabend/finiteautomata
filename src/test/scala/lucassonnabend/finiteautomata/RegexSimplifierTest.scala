package lucassonnabend.finiteautomata

import org.scalatest.{FlatSpec, Matchers}

class RegexSimplifierTest extends FlatSpec with Matchers {

  "RegexSimplifier" should
    "simplify [a-z]" in {
      val actual = RegexSimplifier.simplify("[a-z]")
      actual should be ("(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)")
    }

    it should "simplify [\\s]*" in {
      val actual = RegexSimplifier.simplify("[\\s]*")
      actual should be ("( |\t)*")
    }

    it should "respect escape correctly (\\*\\**\\(\\)\\|\\[\\])" in {
      val actual = RegexSimplifier.simplify("\\*\\**\\(\\)\\|\\[\\]")
      actual should be ("134134*132133216[]")
    }
}
