package finiteautomata

import java.util

class NFA private(val startingState: NFAState, val acceptingStates: Set[NFAState]) {
  /**
    * Returns a new deep copy of this NFA.
    */
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
          oldToNewMap(oldNextState)
        newState.addTransition(input, newNextState)
      }
    }
    val newAcceptingStates: Set[NFAState] = oldToNewMap.filter(e => acceptingStates.contains(e._1)).values.toSet
    new NFA(newStartingState, newAcceptingStates)
  }

  /**
    * Returns true if the given input is accepted by the NFA.
    */
  def accept(input: String): Boolean = {
    var currentStates: Set[NFAState] = Set(this.startingState)
    val inputChar = input.iterator
    while(inputChar.hasNext && currentStates.nonEmpty) {
      val currentInput = inputChar.next()
      currentStates = findNextStatesSets(currentStates).getOrElse(currentInput, Set())
    }
    val epsilonClosureOfCurrentState = currentStates.foldLeft[Set[NFAState]](currentStates)((reachable, state) => reachable ++ getEpsilonClosure(state))
    epsilonClosureOfCurrentState.exists(_.isAcceptingState)
  }

  /**
    * Create and return a new NFA that is this NFA with the star operator applied.
    * This NFA remains unchanged.
    */
  def applyStar(): NFA = {
    val newNFA = this.copy
    val newStartingState = new NFAState(true)
    newStartingState.addEpsilonTransition(newNFA.startingState)
    for(acceptingState <- newNFA.acceptingStates) {
      acceptingState.addEpsilonTransition(newStartingState)
      acceptingState.isAcceptingState = false
    }
    new NFA(newStartingState, Set(newStartingState))
  }

  /**
    * Create and return a new NFA that is the union of this NFA and the other NFA.
    * This NFA and the other NFA remain unchanged.
    */
  def union(otherNFA: NFA): NFA = {
    val newNFA1 = this.copy
    val newNFA2 = otherNFA.copy
    val newStartingState = new NFAState()
    newStartingState.addEpsilonTransition(newNFA1.startingState)
    newStartingState.addEpsilonTransition(newNFA2.startingState)
    new NFA(newStartingState, newNFA1.acceptingStates ++ newNFA2.acceptingStates)
  }

  /**
    * Create and return a new NFA that is the concatenation of this NFA and the other NFA.
    * This NFA and the other NFA remain unchanged.
    */
  def concat(otherNFA: NFA): NFA = {
    val newNFA1 = this.copy
    val newNFA2 = otherNFA.copy
    newNFA1.acceptingStates.foreach(acceptingState => {
      acceptingState.addEpsilonTransition(newNFA2.startingState)
      acceptingState.isAcceptingState = false
    })
    new NFA(newNFA1.startingState, newNFA2.acceptingStates)
  }

  /***
    * Create and returns a DFA that accepts the same inputs as this NFA, and uses the base DFAState
    * class to represent DFA states
    */
  def toSimpleDFA: DFA[DFAState] = this.toDFA[DFAState](isAccepting => new DFAState(isAccepting))

  /***
    * Create and return a DFA that accepts the same inputs as this NFA and internally uses the
    * S class to represent DFA states
    */
  def toDFA[S <: DFAState](stateCreator: (Boolean) => S): DFA[S] = {
    val epsilonFreeNDA = this.removeEpsilonTransitions()

    val startingState = stateCreator(epsilonFreeNDA.startingState.isAcceptingState)
    var ndaToDfaMap: Map[Set[NFAState], DFAState] = Map(Set(epsilonFreeNDA.startingState) -> startingState)
    val ndaStateSetQueue: util.Queue[Set[NFAState]] = new util.LinkedList[Set[NFAState]]()
    ndaStateSetQueue.add(Set(epsilonFreeNDA.startingState))

    while (!ndaStateSetQueue.isEmpty) {
      val currentStateSet = ndaStateSetQueue.poll()
      // find corresponding DFA state
      val currentDFAState = ndaToDfaMap(currentStateSet)
      //find next state sets for each input symbol
      val nextStateSets = findNextStatesSets(currentStateSet)
      for ((input, nextStateSet) <- nextStateSets) {
        // if the next DFA state already exists get it, otherwise create it and add it to the Q
        val nextDFAState = ndaToDfaMap.getOrElse(nextStateSet, {
          val newState = stateCreator(nextStateSet.exists(_.isAcceptingState))
          ndaToDfaMap = ndaToDfaMap + (nextStateSet -> newState)
          ndaStateSetQueue.add(nextStateSet)
          newState
        })
        //add transitions from current DFA state to new DFA state
        ndaToDfaMap = ndaToDfaMap + (nextStateSet -> nextDFAState)
        currentDFAState.addTransition(input, nextDFAState)
      }
    }
    new DFA(startingState, stateCreator)
  }

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

  private def removeEpsilonTransitions(): NFA = {
    // find all states with epsilonTransitions
    val statesWithEpsilonTransition = getAllStates.filter( s =>
      s.getNextStatesAsSeq.exists(_._1.isEmpty))

    for (state <- statesWithEpsilonTransition) {
      for(nextState <- getEpsilonClosure(state) - state) {
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
    * This does contain the current state
    */
  private def getEpsilonClosure(state: NFAState): Set[NFAState] = {
    var result: Set[NFAState] = Set(state)
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

  /**
    * Given a set of current states this returns a map Symbol -> Set[NextStates]
    * where the Set[NextStates] is the set of states that can be reached by any number of
    * epsilon transitions followed by a single transition containing the input Symbol
    */
  private def findNextStatesSets(ndaStates: Set[NFAState]): Map[Char, Set[NFAState]] = {
    var transitions: Map[Char, Set[NFAState]] = Map()
    for (ndaState <- ndaStates.foldLeft[Set[NFAState]](ndaStates)((accumulatorSet, state) => accumulatorSet ++ getEpsilonClosure(state))) {
      for ((input, nextState) <- ndaState.getNextStatesAsSeq) {
        if (input.isDefined) {
          val currentNextStateSet: Set[NFAState] = transitions.getOrElse(input.get, Set())
          transitions = transitions + (input.get -> (currentNextStateSet + nextState))
        }
      }
    }
    transitions
  }

  private def getAllStates: Set[NFAState] = {
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
          totalNFA = if (totalNFA == null) currentNFA else totalNFA.concat(currentNFA)
          val closingIndex = regularExpression.lastIndexOf(')')
          if (closingIndex == -1) throw new ParsingException('(', index, "Could not find a closing bracket")
          currentNFA = createFromRegex(regularExpression.substring(index + 1, closingIndex))
          index = closingIndex + 1
        case '*' =>
          currentNFA match {
            case null => throw new ParsingException('*', index, "Expected a valid regex expression before the *.")
            case _ => currentNFA = currentNFA.applyStar()
          }
          index = index + 1
        case '|' =>
          totalNFA = if (totalNFA == null) currentNFA else totalNFA.concat(currentNFA)
          val otherHalf = regularExpression.substring(index + 1)
          val otherHalfNFA = createFromRegex(otherHalf)
          currentNFA = totalNFA.union(otherHalfNFA)
          totalNFA = null
          index = regularExpression.length()
        case '\\' =>
          totalNFA = if (totalNFA == null) currentNFA else totalNFA.concat(currentNFA)
          currentNFA = createFromTerminal(regularExpression.charAt(index + 1))
          index = index + 2
        case c: Char =>
          totalNFA = if (totalNFA == null) currentNFA else totalNFA.concat(currentNFA)
          currentNFA = createFromTerminal(c)
          index = index + 1
      }
    }
    if (totalNFA == null) {
      currentNFA
    } else {
      val result = totalNFA.concat(currentNFA)
      result
    }
  }

  /**
    * Create and return a new NFA that accepts the given single terminal input.
    */
  def createFromTerminal(singleTerminal: Char): NFA = {
    val startingState = new NFAState()
    val acceptingState = new NFAState(true)
    startingState.addTransition(singleTerminal, acceptingState)
    new NFA(startingState, Set(acceptingState))
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
