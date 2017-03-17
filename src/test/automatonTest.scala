package test

import main._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by lucas on 08/03/17.
  */
class automatonTest extends FlatSpec with Matchers {

  "DFA" should "create an automaton for (a|b*)c" in {
    val nda = NFA.createFromRegex("(a|b*)c").removeEpsilonTransitions()
    val dfa = DFA.createFromNFA(nda)

    dfa.accept("ac") should be (true)
    dfa.accept("aac") should be (false)
    dfa.accept("bbbbc") should be (true)
    dfa.accept("c") should be (true)

    TikzPrinter.printToTikz(dfa, "/home/lucas/IdeaProjects/FiniteAutomata/out/automaton.latex")
    //println(s"DFA for (a|b*)c: $dfa")
  }

  "DFA" should "create an automaton for a|b" in {
    val dfa = DFA.createFromRegex("a|b")

    dfa.accept("a") should be (true)
    dfa.accept("b") should be (true)
    dfa.accept("") should be (false)
    dfa.accept("ab") should be (false)
  }

  "DFA" should "create an automaton for (aa|aa)" in {
    val dfa = DFA.createFromRegex("(aa|aa)")
    dfa.accept("a") should be (false)
    dfa.accept("aa") should be (true)
    dfa.accept("") should be (false)
    dfa.accept("aaa") should be (false)
  }

  "parsing incorrect Regexes" should "throw an exception" in {
    an [ParsingException] should be thrownBy NFA.createFromRegex("(aab")
    an [ParsingException] should be thrownBy NFA.createFromRegex("*a")
  }

  "DFA" should "create an automaton for (a|b|c|d)" in {
    val dfa = DFA.createFromRegex("a|b|c|d")
    dfa.accept("a") should be (true)
    dfa.accept("b") should be (true)
    dfa.accept("c") should be (true)
    dfa.accept("d") should be (true)
    dfa.accept("") should be (false)
    dfa.accept("ac") should be (false)

    TikzPrinter.printToTikz(dfa, "/home/lucas/IdeaProjects/FiniteAutomata/out/automaton2.latex")
  }
}