package test

import org.scalatest.{FlatSpec, Matchers}
import main.{NFA, TikzPrinter}

class NFATest extends FlatSpec with Matchers {

  "NFA.createFromTerminal" should "create an NFA from terminal c" in {
    val nfa = NFA.createFromTerminal('c')
    nfa.acceptingStates.size should be (1)
    val acceptingState = nfa.acceptingStates.toSeq.head
    nfa.startingState.getNextStatesAsSeq should be (Seq((Some('c'), acceptingState)))
  }

  "NFA.createFromRegex" should "create an NFA from the test regexes" in {
    for ((testRegex, testInputs) <- TestRegexes.TEST_INPUTS) {
      val nfa = NFA.createFromRegex(testRegex)
      for ((input, expected) <- testInputs) {
        assert(nfa.accept(input) == expected, s" testinput: $input for regex $testRegex test failed")
      }
    }
  }

  "remove" should "remove all epsilon transitions" in {
    for ((testRegex, _) <- TestRegexes.TEST_INPUTS) {
      val nfa = NFA.createFromRegex(testRegex)
      val epsilonFreeNfa = nfa.removeEpsilonTransitions()
      for (state <- epsilonFreeNfa.getAllStates) {
        state.getNextStates should not contain key (None)
      }
    }
  }
  it should "not change the behaviour of the NFA" in {
    for ((testRegex, testInputs) <- TestRegexes.TEST_INPUTS) {
      val nfa = NFA.createFromRegex(testRegex)
      val epsilonFreeNfa = nfa.removeEpsilonTransitions()
      for ((input, expected) <- testInputs) {
        assert(epsilonFreeNfa.accept(input) == expected, s" testinput: $input for regex $testRegex test failed")
      }
    }
  }

  "NFA.copy" should "replicate a given non-terminal" in {
    for ((testRegex, testInputs) <- TestRegexes.TEST_INPUTS) {
      val nfa = NFA.createFromRegex(testRegex)
      val nfaCopy = nfa.copy
      for ((input, expected) <- testInputs) {
        assert(nfaCopy.accept(input) == expected, s" testinput: $input for regex $testRegex test failed")
      }
    }
  }
}
