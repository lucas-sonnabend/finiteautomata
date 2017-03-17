package test

import main.{DFA, NFA}
import org.scalatest.{FlatSpec, Matchers}


class DFATest extends FlatSpec with Matchers {

  "DFA.createFromNFA" should "create a DFA for the test regexes" in {
    var index = 0
    for ((testRegex, testInputs) <- TestRegexes.TEST_INPUTS) {
      val nfa = NFA.createFromRegex(testRegex)
      val dfa = DFA.createFromNFA(nfa)
      //TikzPrinter.printToTikz(dfa, s"/home/lucas/IdeaProjects/FiniteAutomata/out/automaton_$index.latex")  //uncomment this to print the DFAs for debugging!
      for ((testInput, testResult) <- testInputs) {
        assert(dfa.accept(testInput) == testResult, s" testinput: $testInput for regex $testRegex test failed, index: $index")
      }
      index = index + 1
    }
  }

  "DFA.createFromRegex" should "create a DFA from the test regexes" in {
    for ((testRegex, testInputs) <- TestRegexes.TEST_INPUTS) {
      val dfa = DFA.createFromRegex(testRegex)
      for ((testInput, testResult) <- testInputs) {
        assert(dfa.accept(testInput) == testResult, s" testinput: $testInput for regex $testRegex test failed")
      }
    }
  }

  "DFA.equals" should "correctly check equality for DFAs with the same states and transitions" in {
    for ((testRegex, _) <- TestRegexes.TEST_INPUTS) {
      val dfa1 = DFA.createFromRegex(testRegex)
      val dfa2 = DFA.createFromRegex(testRegex)
      assert(dfa1 == dfa2, s"two regexes for $testRegex should be equal but are not")
    }
  }

  it should "correctly detect if two DFAs are not equal" in {
    val dfa1 = DFA.createFromRegex(TestRegexes.TEST_REGEX_AA_OR_AA)
    val dfa2 = DFA.createFromRegex(TestRegexes.TEST_REGEX_A_OR_B)
    val dfa3 = DFA.createFromRegex(TestRegexes.TEST_REGEX_A_OR_BS_AND_C)
    val dfa4 = DFA.createFromRegex(TestRegexes.TEST_REGEX_AB_OR_AA)

    assert(!(dfa1 == dfa2))
    assert(!(dfa1 == dfa3))
    assert(!(dfa1 == dfa4))
    assert(!(dfa2 == dfa3))
  }

  "DFA.copy" should "create an exact copy of the given dfa" in {
    for ((testRegex, testInputs) <- TestRegexes.TEST_INPUTS) {
      val dfa = DFA.createFromRegex(testRegex)
      val dfaCopy = dfa.copy
      for ((testInput, testResult) <- testInputs) {
        assert(dfa.accept(testInput) == testResult, s" testinput: $testInput for regex $testRegex test failed")
        assert(dfaCopy.accept(testInput) == testResult, s" testinput: $testInput for regex $testRegex test failed")
      }
    }
  }
}
