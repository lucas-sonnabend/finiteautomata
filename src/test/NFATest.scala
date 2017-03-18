package test

import org.scalatest.{FlatSpec, Matchers}
import main.NFA

class NFATest extends FlatSpec with Matchers{

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

  "toDFA" should "create a DFA from the NFA that accepts the same inputs" in {
    var index = 0
    for ((testRegex, testInputs) <- TestRegexes.TEST_INPUTS) {
      val nfa = NFA.createFromRegex(testRegex)
      val dfa = nfa.toSimpleDFA
      //dfa.printToTikz(s"/home/lucas/IdeaProjects/FiniteAutomata/out/automaton_$index.latex")  //uncomment this to print the DFAs for debugging!
      for ((testInput, testResult) <- testInputs) {
        assert(dfa.accept(testInput) == testResult, s" testinput: $testInput for regex $testRegex test failed, index: $index")
      }
      index = index + 1
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
