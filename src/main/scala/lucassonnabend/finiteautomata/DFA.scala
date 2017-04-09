package lucassonnabend.finiteautomata

import java.io.{File, PrintWriter}
import java.util

class DFA[S <: DFAState](var startingState: S, var stateCreator: Boolean => S) {

  override def toString: String = {
    val output: StringBuilder = new StringBuilder()
    getAllStates.foreach(state => {
      output.append(s"${state.toVerboseString}\n")
    })
    output.result()
  }

  def getAllStates: Iterable[S] = new Iterable[S](){
    val iterator = new Iterator[S]() {
      val stateQueue: util.Queue[DFAState] = new util.LinkedList[DFAState]()
      var visitedSet: Set[DFAState] = Set()
      stateQueue.add(startingState)

      override def hasNext: Boolean = !stateQueue.isEmpty
      override def next(): S = {
        val cur = stateQueue.poll()
        visitedSet = visitedSet + cur
        cur.getTransitions.values.filter(!visitedSet.contains(_)).foreach(nextState => {
          stateQueue.add(nextState)
        })
        cur.asInstanceOf[S]
      }
    }
  }

  def accept(input: String): Boolean = {
    var currentState: DFAState = startingState
    val inputChar = input.iterator
    while(inputChar.hasNext && currentState != null) {
      currentState = currentState.getNextState(inputChar.next())
    }
    currentState != null && currentState.isAcceptingState
  }


  def union(otherDFA: DFA[S]): DFA[S] = {
    // have a Q of tuple of states that should be joined into a single state.
    val statesQ: util.Queue[(DFAState, DFAState)] = new util.LinkedList[(DFAState, DFAState)]()
    statesQ.add((this.startingState, otherDFA.startingState))
    val newStartingState = stateCreator(this.startingState.isAcceptingState || otherDFA.startingState.isAcceptingState)
    var visitedStates: Map[(DFAState, DFAState), DFAState] = Map((this.startingState, otherDFA.startingState) -> newStartingState)

    while(!statesQ.isEmpty) {
      val currentStates = statesQ.poll() // this is a pair of states that you can be in, starting from the combination of both starting states
      val newState = visitedStates(currentStates)

      val transitions: Map[Char, (DFAState, DFAState)] = currentStates match {
        case (state1: DFAState, null) => state1.getTransitions.map(e => (e._1, (e._2, null)))
        case (null, state2: DFAState) => state2.getTransitions.map(e => (e._1, (null, e._2)))
        case (state1:DFAState, state2: DFAState) => mergeTransitions(state1.getTransitions, state2.getTransitions)
      }

      for ((input, nextStates) <- transitions) {
        val newNextState = if (visitedStates.contains(nextStates)) {
          visitedStates(nextStates)
        } else {

          val newNextState = nextStates match {
            case (nextState1: DFAState, null) => this.stateCreator(nextState1.isAcceptingState)
            case (null, nextState2: DFAState) => otherDFA.stateCreator(nextState2.isAcceptingState)
            case (nextState1: DFAState, nextState2: DFAState)  => this.stateCreator(nextState1.isAcceptingState || nextState2.isAcceptingState)
          }
          visitedStates = visitedStates + (nextStates -> newNextState)
          statesQ.add(nextStates)
          newNextState
        }
        newState.addTransition(input, newNextState)
      }
    }
    new DFA[S](newStartingState, this.stateCreator)
  }

  def mergeTransitions(transitions1: Map[Char, DFAState], transitions2: Map[Char, DFAState]): Map[Char, (DFAState, DFAState)] = {
    var sortedTrans1: Seq[(Char, DFAState)] = transitions1.toSeq.sortBy(_._1)
    var sortedTrans2: Seq[(Char, DFAState)] = transitions2.toSeq.sortBy(_._1)
    var result: Map[Char, (DFAState, DFAState)] = Map()

    while(sortedTrans1.nonEmpty && sortedTrans2.nonEmpty) {
      val nextTrans = if (sortedTrans1.head._1 == sortedTrans2.head._1) {
        val nextChar = sortedTrans1.head._1
        val nextStates = (sortedTrans1.head._2, sortedTrans2.head._2)
        sortedTrans1 = sortedTrans1.tail
        sortedTrans2 = sortedTrans2.tail

        (nextChar, nextStates)
      } else if (sortedTrans1.head._1 < sortedTrans2.head._1) {
        val nextChar = sortedTrans1.head._1
        val nextStates = (sortedTrans1.head._2, null)
        sortedTrans1 = sortedTrans1.tail
        (nextChar, nextStates)
      } else {
        val nextChar = sortedTrans2.head._1
        val nextStates = (null, sortedTrans2.head._2)
        sortedTrans2 = sortedTrans2.tail
        (nextChar, nextStates)
      }
      result = result + nextTrans
    }
    val rest = if (sortedTrans1.nonEmpty) {
      sortedTrans1.map(elem => (elem._1, (elem._2, null))).toMap
    } else if (sortedTrans2.nonEmpty) {
      sortedTrans2.map(elem => (elem._1, (null, elem._2))).toMap
    } else Map()
    result ++ rest
  }

  def copy: DFA[S] = {
    val newStartingState = this.stateCreator(startingState.isAcceptingState)

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
    new DFA[S](newStartingState, this.stateCreator)
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case otherDFA: DFA[S] =>
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

  def printToTikz(filename: String): Unit = {
    val output: StringBuilder = new StringBuilder()
    output.append("\\documentclass{article}\n")
    output.append("\n")
    output.append("\\usepackage{pgf}\n")
    output.append("\\usepackage{tikz}\n")
    output.append("\\usetikzlibrary{positioning,automata}\n")
    output.append("\\usepackage[latin1]{inputenc}\n")
    output.append("\\begin{document}\n")
    output.append(getTikzPicture)
    output.append("\\end{document}")

    val pw = new PrintWriter(new File(filename))
    pw.write(output.result())
    pw.close()
  }

  def getTikzPicture: String = {
    val tikzPicture: StringBuilder = new StringBuilder()
    tikzPicture.append("\\begin{tikzpicture}[shorten >=1pt,node distance=2cm,on grid]\n")

    val nodeList: StringBuilder = new StringBuilder()
    nodeList.append(stateToTikzNode(startingState, 0))
    val edgeList: StringBuilder = new StringBuilder()
    val stateQ: util.Queue[(DFAState, Position)] = new util.LinkedList[(DFAState, Position)]()

    stateQ.add((startingState, new Position(1,1)))
    var visitedStates: Map[DFAState, Position] = Map()
    var nodeIndex: Int = 1
    while(!stateQ.isEmpty) {
      val (state, position) = stateQ.poll()

      // add edges
      for ((input, nextState) <- state.getTransitions) {
        val isBackwardsEdge = visitedStates.contains(nextState) && (visitedStates(nextState).x < position.x || visitedStates(nextState).y > position.y + 1)
        edgeList.append(transToTikzEdge(input, state, nextState, isBackwardsEdge = isBackwardsEdge))
      }

      // add new next States
      var prefNext: Option[DFAState] = None
      var curY = position.y
      for ((_, newNextState) <- state.getTransitions.filter(trans => {!visitedStates.keySet.contains(trans._2)})) {
        nodeList.append(stateToTikzNode(newNextState, nodeIndex, rightOf = Some(state), belowOf = prefNext))
        prefNext = Some(newNextState)
        stateQ.add((newNextState, new Position(position.x + 1, curY)))
        visitedStates = visitedStates + (newNextState -> new Position(position.x + 1, curY))
        curY = curY + 1
        nodeIndex = nodeIndex + 1
      }
    }
    tikzPicture.append(nodeList)
    tikzPicture.append("\\path[->]\n")
    tikzPicture.append(edgeList)
    tikzPicture.append(";\n")
    tikzPicture.append("\\end{tikzpicture}\n")
    tikzPicture.result
  }

  private def stateToTikzNode(state: DFAState, index: Int, rightOf: Option[DFAState] = None, belowOf: Option[DFAState] = None): String = {
    val isStarting: String = if (state.eq(startingState)) ",initial" else ""
    val isAccepting: String = if(state.isAcceptingState) ",accepting" else ""
    val position: String = if(belowOf.isDefined)
      s"[below of=${belowOf.get.getName}]"
    else if (rightOf.isDefined)
      s"[right of=${rightOf.get.getName}]"
    else ""
    s"\\node[state$isStarting$isAccepting]   (${state.getName}) $position {$$q_$index$$};\n"
  }

  private def transToTikzEdge(input: Char, source: DFAState, dest: DFAState, isBackwardsEdge: Boolean): String = {
    if (source.equals(dest)) {
      s"(${source.getName}) edge  [loop above]    node {$input} ()"
    } else {
      if (isBackwardsEdge)
        s"(${source.getName}) edge  [bend  left=45]    node [above] {$input} (${dest.getName})\n"
      else
        s"(${source.getName}) edge                     node [above] {$input} (${dest.getName})\n"
    }
  }

  private class Position(val x: Int, val y: Int)
}

object DFA {

  /**
    * This creates a DFA from a regex. The regex supports the minimal set of operators: concat, star <*> and union <|>.
    * The precedence is star <- concat <- union
    * Additional operations are plus <+> and []
    * currently the character classes are a-z, A-Z 0-9 and \s
    * special characters are escaped with a backslash
    */
  def createFromRegex[S <: DFAState](regex: String, stateCreator: Boolean => S): DFA[S] = {
    val simpleRegex = regex
    NFA.createFromRegex(simpleRegex).toDFA[S](stateCreator)

  }

  def createSimpleFromRegex(regex: String): DFA[DFAState] = {
    val simpleRegex = regex
    NFA.createFromRegex(simpleRegex).toDFA[DFAState](isAccepting => new DFAState(isAccepting))
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
