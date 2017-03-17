package main

import java.util

class DFA(var startingState: DFAState) {

  override def toString: String = {
    val output: StringBuilder = new StringBuilder()
    getAllStates.foreach(state => {
      output.append(s"${state.toVerboseString}\n")
    })
    output.result()
  }

  def getAllStates: Set[DFAState] = {
    var result: Set[DFAState] = Set()
    val stateQueue: util.Queue[DFAState] = new util.LinkedList[DFAState]()
    stateQueue.add(startingState)
    while (!stateQueue.isEmpty) {
      val cur: DFAState = stateQueue.poll()
      result = result + cur
      cur.getTransitions.values.filter(!result.contains(_)).foreach(nextState => {
        stateQueue.add(nextState)
      })
    }
    result
  }

  def accept(input: String): Boolean = {
    var currentState = startingState
    val inputChar = input.iterator
    while(inputChar.hasNext && currentState != null) {
      currentState = currentState.getNextState(inputChar.next())
    }
    currentState != null && currentState.isAcceptingState
  }

  def copy: DFA = {
    val newStartingState = new DFAState(startingState.isAcceptingState)

    var oldToNewMap: Map[DFAState, DFAState] = Map(startingState -> newStartingState)
    val stateQ: util.Queue[DFAState] = new util.LinkedList[DFAState]()
    stateQ.add(startingState)
    while(!stateQ.isEmpty) {
      val oldState = stateQ.poll()
      val newState = oldToNewMap(oldState)
      for ((input, oldNextState) <- oldState.getTransitions) {
        val newNextState = if (!oldToNewMap.contains(oldNextState)) {
          val newNextState = new DFAState(oldNextState.isAcceptingState)
          stateQ.add(oldNextState)
          oldToNewMap = oldToNewMap + (oldNextState -> newNextState)
          newNextState
        } else oldToNewMap(oldNextState)

        newState.addTransition(input, newNextState)
      }
    }
    new DFA(newStartingState)
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case otherDFA: DFA =>
        val stateQ: util.Queue[DFAState] = new util.LinkedList[DFAState]()
        stateQ.add(this.startingState)
        var visitedStates: Map[DFAState, DFAState] = Map(this.startingState -> otherDFA.startingState)
        var canBeEqual = true
        while(!stateQ.isEmpty && canBeEqual) {
          val myCurrentState = stateQ.poll()
          val otherCurrentState = visitedStates(myCurrentState)
          if (myCurrentState.isAcceptingState ^ otherCurrentState.isAcceptingState) canBeEqual = false

          // check if for every transition in my current state there is an equivalent transition for the other state
          for ((input, myNextState) <- myCurrentState.getTransitions) {
            val otherNextState = otherCurrentState.getNextState(input)
            if (otherNextState == null) canBeEqual = false
            else {
              if(!visitedStates.contains(myNextState)) {
                visitedStates = visitedStates + (myNextState -> otherNextState)
                stateQ.add(myNextState)
              }
            }
          }
          // check whether there are any transitions for the other state that the current state doesn't have
          val keyDiff = otherCurrentState.getTransitions.keySet diff myCurrentState.getTransitions.keySet
          if (keyDiff.nonEmpty) canBeEqual = false
        }

        canBeEqual
      case _ => false
    }
  }
}

object DFA {

  /**
    * This creates a DFA from a regex. The regex supports the minimal set of operators: concat, star <*> and union <|>.
    * The precedence is star <- concat <- union
    * Additional operations are plus <+> and []
    * currently the character classes are a-z, A-Z 0-9 and \s   //TODO: currently that is a lie!
    * special characters are escaped with a backslash
    */
  def createFromRegex(regex: String): DFA = {
    val simpleRegex = regex
    createFromNFA(NFA.createFromRegex(simpleRegex))
  }

  def createFromNFA(nda: NFA): DFA = {
    val epsilonFreeNDA = nda.removeEpsilonTransitions()

    val startingState = new DFAState(epsilonFreeNDA.startingState.isAcceptingState)
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
          val newState = new DFAState(nextStateSet.exists(_.isAcceptingState))
          ndaToDfaMap = ndaToDfaMap + (nextStateSet -> newState)
          ndaStateSetQueue.add(nextStateSet)
          newState
        })
        //add transitions from current DFA state to new DFA state
        ndaToDfaMap = ndaToDfaMap + (nextStateSet -> nextDFAState)
        currentDFAState.addTransition(input, nextDFAState)
      }
    }
    new DFA(startingState)
  }

  private def findNextStatesSets(ndaStates: Set[NFAState]): Map[Char, Set[NFAState]] = {
    var transitions: Map[Char, Set[NFAState]] = Map()
    for (ndaState <- ndaStates) {
      for ((input, nextState) <- ndaState.getNextStatesAsSeq) {
        val currentNextStateSet: Set[NFAState] = transitions.getOrElse(input.get, Set())
        transitions = transitions + (input.get -> (currentNextStateSet + nextState))
      }
    }
    transitions
  }
}

class DFAState(var isAcceptingState: Boolean = false) {
  private var transitions: Map[Char, DFAState] = Map()

  def addTransition(input: Char, nextState: DFAState): Unit = {
    transitions = transitions + (input ->nextState)
  }

  def getTransitions: Map[Char, DFAState] = transitions
  def getNextState(input: Char): DFAState = transitions.getOrElse(input, null)

  override def toString: String = {
    if (isAcceptingState)
      "State(A) " + getName
    else
      "State " + getName
  }

  def getName: String = Integer.toHexString(System.identityHashCode(this))

  def toVerboseString: String = {
    val output: StringBuilder = new StringBuilder()
    output.append(s" ${this.toString()}: ")
    for ((input, nextState) <- transitions) {
      output.append(s"$input -> $nextState, ")
    }
    output.result()
  }
}