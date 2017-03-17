package main

import java.util

class NFA(val startingState: NFAState, val acceptingStates: Set[NFAState]) {

  override def toString: String = {
    val output: StringBuilder = new StringBuilder()
    val stateQueue: util.Queue[NFAState] = new util.LinkedList[NFAState]()
    stateQueue.add(startingState)
    var visitedStates: Set[NFAState] = Set(startingState)
    while(!stateQueue.isEmpty) {
      val cur = stateQueue.poll()
      output.append(s"${cur.toVerboseString}\n")
      for ((_, elem) <- cur.getNextStatesAsSeq) {
        if (!visitedStates.contains(elem))
          stateQueue.add(elem)
        visitedStates = visitedStates + elem
      }
    }
    output.result()
  }

  def removeEpsilonTransitions(): NFA = {
    // find all states with epsilonTransitions
    val statesWithEpsilonTransition = getAllStates.filter( s =>
      s.getNextStatesAsSeq.exists(_._1.isEmpty))

    for (state <- statesWithEpsilonTransition) {
      for(nextState <- getEpsilonClosure(state)) {
        // find all the non-epsilon next states of these, and rewire them
        for((input, nextNextState) <- nextState.getNextStatesAsSeq.filter(_._1.isDefined)) {
          state.addTransition(input, nextNextState)
        }
        if(nextState.isAcceptingState) state.isAcceptingState = true
      }
      state.removeEpsilonTransitions()
    }
    //remove potentially dead accepting states
    val acceptingStates = getAllStates.filter(_.isAcceptingState)
    new NFA(this.startingState, acceptingStates)
  }

  /**
    * Given a state it returns all states that can be reached from this states via epsilon transitions.
    * This does not contain the current state!
    */
  private def getEpsilonClosure(state: NFAState): Set[NFAState] = {
    var result: Set[NFAState] = Set()
    val stateQueue: util.Queue[NFAState] = new util.LinkedList[NFAState]()
    stateQueue.add(state)
    while(!stateQueue.isEmpty) {
      val currentState = stateQueue.poll()
      // get all the next states reachable by an epsilon transition
      for(nextState <- currentState.getNextStatesAsSeq.filter(_._1.isEmpty).map(_._2)) {
        if (!result.contains(nextState)) {
          result = result + nextState
          stateQueue.add(nextState)
        }
      }
    }
    result
  }

  def getAllStates: Set[NFAState] = {
    var result: Set[NFAState] = Set(startingState)
    val stateQueue: util.Queue[NFAState] = new util.LinkedList[NFAState]()
    stateQueue.add(startingState)
    while(!stateQueue.isEmpty) {
      val cur = stateQueue.poll()
      for (unvisitedNextState <- cur.getNextStatesAsSeq.map(_._2).filter(!result.contains(_))) {
        stateQueue.add(unvisitedNextState)
        result = result + unvisitedNextState
      }
    }
    result
  }

  def copy: NFA = {
    val newStartingState: NFAState = new NFAState(startingState.isAcceptingState)

    var oldToNewMap: Map[NFAState, NFAState] = Map(startingState -> newStartingState)
    val stateQueue: util.Queue[NFAState] = new util.LinkedList[NFAState]()
    stateQueue.add(startingState)
    while(!stateQueue.isEmpty) {
      val oldState = stateQueue.poll()
      val newState: NFAState = oldToNewMap(oldState)
      for ((input, oldNextState) <- oldState.getNextStatesAsSeq) {
        val newNextState = if (!oldToNewMap.contains(oldNextState)) {
          val newNextState = new NFAState(oldNextState.isAcceptingState)
          stateQueue.add(oldNextState)
          oldToNewMap = oldToNewMap + (oldNextState -> newNextState)
          newNextState
        } else
          oldToNewMap(oldState)
        newState.addTransition(input, newNextState)
      }
    }
    val newAcceptingStates: Set[NFAState] = oldToNewMap.filter(e => acceptingStates.contains(e._1)).values.toSet
    new NFA(newStartingState, newAcceptingStates)
  }

  def accept(input: String): Boolean = {

    var currentStates: Set[NFAState] = Set(this.startingState)

    val inputChar = input.iterator
    while(inputChar.hasNext && currentStates.nonEmpty) {
      val currentInput = Some(inputChar.next())
      var newNextState: Set[NFAState] = Set()
      for(currentState <- currentStates) {
        val statesToTraverseFrom = getEpsilonClosure(currentState) + currentState
        //TODO simpify this
        val test: Set[Set[NFAState]] = statesToTraverseFrom.map(_.getNextStates.getOrElse(currentInput, Seq()).toSet)
        newNextState = newNextState ++ test.fold(Set())((a,b) => a ++ b)
      }
      currentStates = newNextState
    }
    currentStates.exists(_.isAcceptingState)
  }
}

object NFA {
  /**
    * Given a simple regex this creates an Nondeterministic finite automaton accepting this strings matching the regex
    * Valid regex operations are concat, union (|), star (*) and brackets (())
    */
  def createFromRegex(regularExpression: String): NFA = {
    var totalNFA: NFA = null
    var currentNFA: NFA = null

    var index = 0
    while(index < regularExpression.length()) {
      regularExpression.charAt(index) match {
        case '(' =>
          totalNFA = if (totalNFA == null) currentNFA else NFA.createConcat(totalNFA, currentNFA)
          val closingIndex = regularExpression.lastIndexOf(')')
          if (closingIndex == -1) throw new ParsingException('(', index, "Could not find a closing bracket")
          currentNFA = createFromRegex(regularExpression.substring(index + 1, closingIndex))
          index = closingIndex + 1
        case '*' =>
          currentNFA match {
            case null => throw new ParsingException('*', index, "Expected a valid regex expression before the *.")
            case _ => currentNFA = NFA.createStar(currentNFA)
          }
          index = index + 1
        case '|' =>
          totalNFA = if (totalNFA == null) currentNFA else NFA.createConcat(totalNFA, currentNFA)
          val otherHalf = regularExpression.substring(index + 1)
          val otherHalfNFA = createFromRegex(otherHalf)
          currentNFA = createUnion(totalNFA, otherHalfNFA)
          totalNFA = null
          index = regularExpression.length()
        case '\\' =>
          totalNFA = if (totalNFA == null) currentNFA else NFA.createConcat(totalNFA, currentNFA)
          currentNFA = createFromTerminal(regularExpression.charAt(index + 1))
          index = index + 2
        case c: Char =>
          totalNFA = if (totalNFA == null) currentNFA else NFA.createConcat(totalNFA, currentNFA)
          currentNFA = createFromTerminal(c)
          index = index + 1
      }
    }
    if (totalNFA == null) {
      currentNFA
    } else {
      val result = createConcat(totalNFA, currentNFA)
      result
    }
  }

  def createFromTerminal(singleTerminal: Char): NFA = {
    val startingState = new NFAState()
    val acceptingState = new NFAState(true)
    startingState.addTransition(singleTerminal, acceptingState)
    new NFA(startingState, Set(acceptingState))
  }

  def createStar(nfa: NFA): NFA = {
    val newStartingState = new NFAState(true)
    newStartingState.addEpsilonTransition(nfa.startingState)

    for(acceptingState <- nfa.acceptingStates) {
      acceptingState.addEpsilonTransition(newStartingState)
      acceptingState.isAcceptingState = false
    }
    new NFA(newStartingState, Set(newStartingState))
  }

  def createUnion(nfa1: NFA, nfa2: NFA): NFA = {

    val newStartingState = new NFAState()
    newStartingState.addEpsilonTransition(nfa1.startingState)
    newStartingState.addEpsilonTransition(nfa2.startingState)

    new NFA(newStartingState, nfa1.acceptingStates ++ nfa2.acceptingStates)
  }

  def createConcat(nfa1: NFA, nfa2: NFA): NFA = {
    nfa1.acceptingStates.foreach(acceptingState => {
      acceptingState.addEpsilonTransition(nfa2.startingState)
      acceptingState.isAcceptingState = false
    })
    new NFA(nfa1.startingState, nfa2.acceptingStates)
  }
}

class NFAState(var isAcceptingState: Boolean = false) {

  private var nextStates: Map[Option[Char], Seq[NFAState]] = Map()

  def addTransition(input: Option[Char], nextState: NFAState): Unit = {
    val newStateSet = nextStates.getOrElse[Seq[NFAState]](input, Seq()) :+ nextState
    nextStates = nextStates + (input -> newStateSet)
  }

  def addTransition(input: Char, nextState: NFAState): Unit = addTransition(Some(input), nextState)
  def addEpsilonTransition(nextState: NFAState): Unit = this.addTransition(None, nextState)
  def removeEpsilonTransitions(): Unit = nextStates = nextStates - None

  override def toString: String = {
    if (isAcceptingState) "State(A) " + Integer.toHexString(System.identityHashCode(this))
    else "State " + Integer.toHexString(System.identityHashCode(this))
  }

  def toVerboseString: String = {
    val output: StringBuilder = new StringBuilder()
    output.append(s" ${this.toString()}: ")
    for (elem <- nextStates) {
      output.append(s"${elem._1} -> ${elem._2}, ")
    }
    output.result()
  }

  /**
    * This returns a sequence pairs (input, nextState) of all next states and which inputs lead to it,
    * by the magic of functional programming!!!!!!!
    */
  def getNextStatesAsSeq: Seq[(Option[Char], NFAState)] = nextStates.map(e => e._2.map((e._1, _))).fold(Seq())((l, e) => l ++ e)

  def getNextStates: Map[Option[Char], Seq[NFAState]] = this.nextStates
}