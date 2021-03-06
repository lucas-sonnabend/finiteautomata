package lucassonnabend.finiteautomata

import org.scalatest.{FlatSpec, Matchers}

class DFATest extends FlatSpec with Matchers {

  private class IndexedDFAState(val id: Int, isAcceptingState: Boolean) extends DFAState(isAcceptingState) {}
  private object IndexedDFAState {
    var counter = 0
    def getAndIncrementCounter(): Integer = {
      val cur = counter
      counter = cur + 1
      cur
    }
  }
  private class CharIndexedDFAState(val id: Char, isAcceptingState: Boolean) extends DFAState(isAcceptingState) {
    def getChar: Char = id
  }

  "DFA.createFromRegex" should "create a DFA from the test regexes" in {
    for ((testRegex, testInputs) <- TestRegexes.TEST_INPUTS) {
      val dfa = DFA.createSimpleFromRegex(testRegex)
      for ((testInput, testResult) <- testInputs) {
        assert(dfa.accept(testInput) == testResult, s" testinput: $testInput for regex $testRegex test failed")
      }
    }
  }

  "DFA.equals" should "correctly check equality for DFAs with the same states and transitions" in {
    for ((testRegex, _) <- TestRegexes.TEST_INPUTS) {
      val dfa1 = DFA.createSimpleFromRegex(testRegex)
      val dfa2 = DFA.createSimpleFromRegex(testRegex)
      assert(dfa1 == dfa2, s"two regexes for $testRegex should be equal but are not")
    }
  }

  it should "correctly detect if two DFAs are not equal" in {
    val dfa1 = DFA.createSimpleFromRegex(TestRegexes.TEST_REGEX_AA_OR_AA)
    val dfa2 = DFA.createSimpleFromRegex(TestRegexes.TEST_REGEX_A_OR_B)
    val dfa3 = DFA.createSimpleFromRegex(TestRegexes.TEST_REGEX_A_OR_BS_AND_C)
    val dfa4 = DFA.createSimpleFromRegex(TestRegexes.TEST_REGEX_AB_OR_AA)

    assert(!(dfa1 == dfa2))
    assert(!(dfa1 == dfa3))
    assert(!(dfa1 == dfa4))
    assert(!(dfa2 == dfa3))
  }

  "DFA.copy" should "create an exact copy of the given dfa" in {
    for ((testRegex, testInputs) <- TestRegexes.TEST_INPUTS) {
      val dfa = DFA.createSimpleFromRegex(testRegex)
      val dfaCopy = dfa.copy
      for ((testInput, testResult) <- testInputs) {
        assert(dfa.accept(testInput) == testResult, s" testinput: $testInput for regex $testRegex test failed")
        assert(dfaCopy.accept(testInput) == testResult, s" testinput: $testInput for regex $testRegex test failed")
      }
    }
  }

  "DFA.createFromRegex" should "return a DFA where all states have the class provided" in {
    val testRegex = TestRegexes.TEST_REGEX_A_OR_B_AND_C_STAR
    val dfa = DFA.createFromRegex[IndexedDFAState](testRegex, isAccepting => new IndexedDFAState(IndexedDFAState.getAndIncrementCounter(), isAccepting))
    for ((testInput, testResult) <- TestRegexes.TEST_INPUTS_A_OR_B_AND_C_STAR) {
      assert(dfa.accept(testInput) == testResult, s" testinput: $testInput for regex $testRegex test failed")
    }
    dfa.getAllStates.foreach(state => state shouldBe an [IndexedDFAState])
  }

  "DFA.union" should "return a DFA that is the union of the DFAs accepting a and b" in {
    val dfa1 = DFA.createSimpleFromRegex("a")
    val dfa2 = DFA.createSimpleFromRegex("b")
    val dfa3 = dfa1.union(dfa2)

    assert(dfa3.accept("a"))
    assert(dfa3.accept("b"))
    assert(!dfa3.accept("c"))
  }

  "DFA.union" should "return a DFA that is the union of two DFAs" in {
    val dfa1 = DFA.createSimpleFromRegex(TestRegexes.TEST_REGEX_A_OR_BS_AND_C)
    val dfa2 = DFA.createSimpleFromRegex(TestRegexes.TEST_REGEX_AB_OR_AA)

    val results = Map(
      "" -> false,
      "a" -> false,
      "ac" -> true,
      "bbbc" -> true,
      "abac" -> false,
      "ab" -> true,
      "aa" -> true,
      "ba" -> false)

    val dfa3 = dfa1.union(dfa2)
    for( (input, result) <- results) {
      assert(dfa3.accept(input) == result, s" testinput: $input test failed")
    }
  }

  "DFA.union" should "return a DFA that is the union of two DFAs using the state creator functions of both DFAs" in {
    val dfa1 = DFA.createFromRegex[CharIndexedDFAState]("abc|dd", isAccepting => new CharIndexedDFAState('a', isAccepting))
    val dfa2 = DFA.createFromRegex[CharIndexedDFAState]("acb|dd", isAccepting => new CharIndexedDFAState('b', isAccepting))

    val dfa3 = dfa1.union(dfa2)

    val charOfABC = dfa3.startingState.getNextState('a').getNextState('b').getNextState('c').asInstanceOf[CharIndexedDFAState].getChar
    val charOfACB = dfa3.startingState.getNextState('a').getNextState('c').getNextState('b').asInstanceOf[CharIndexedDFAState].getChar
    val charOfDD = dfa3.startingState.getNextState('d').getNextState('d').asInstanceOf[CharIndexedDFAState].getChar

    assert(charOfABC == 'a')
    assert(charOfACB == 'b')
    assert(charOfDD == 'a')
  }
}
