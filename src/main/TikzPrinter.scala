package main

import java.io.{File, PrintWriter}
import java.util

/**
  * This is a helper class that prints a DFA as a tikz picture within a latex file
  */
object TikzPrinter {

  def printToTikz(dfa: DFA, filename: String): Unit = {
    val output: StringBuilder = new StringBuilder()
    output.append("\\documentclass{article}\n")
    output.append("\n")
    output.append("\\usepackage{pgf}\n")
    output.append("\\usepackage{tikz}\n")
    output.append("\\usetikzlibrary{positioning,automata}\n")
    output.append("\\usepackage[latin1]{inputenc}\n")
    output.append("\\begin{document}\n")
    output.append(getTikzPicture(dfa))
    output.append("\\end{document}")

    val pw = new PrintWriter(new File(filename))
    pw.write(output.result())
    pw.close()
  }

  def getTikzPicture(dfa: DFA): String = {
    val tikzPicture: StringBuilder = new StringBuilder()
    tikzPicture.append("\\begin{tikzpicture}[shorten >=1pt,node distance=2cm,on grid]\n")

    val nodeList: StringBuilder = new StringBuilder()
    nodeList.append(stateToTikzNode(dfa, dfa.startingState, 0))
    val edgeList: StringBuilder = new StringBuilder()
    val stateQ: util.Queue[DFAState] = new util.LinkedList[DFAState]()
    stateQ.add(dfa.startingState)
    var visitedStates: Set[DFAState] = Set()
    var nodeIndex: Int = 1
    while(!stateQ.isEmpty) {
      val state = stateQ.poll()
      var prefNext: Option[DFAState] = None

      // add edges
      for ((input, nextState) <- state.getTransitions) {
        val bend = if(visitedStates.contains(nextState)) "left" else "right"
        edgeList.append(transToTikzEdge(input, state, nextState, bend = bend))
      }

      // add new next States
      for ((_, newNextState) <- state.getTransitions.filter(trans => {!visitedStates.contains(trans._2)})) {
        nodeList.append(stateToTikzNode(dfa, newNextState, nodeIndex, rightOf = Some(state), belowOf = prefNext))
        prefNext = Some(newNextState)
        stateQ.add(newNextState)
        visitedStates = visitedStates + newNextState
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

  private def stateToTikzNode(dfa: DFA, state: DFAState, index: Int, rightOf: Option[DFAState] = None, belowOf: Option[DFAState] = None): String = {
    val isStarting: String = if (state.eq(dfa.startingState)) ",initial" else ""
    val isAccepting: String = if(state.isAcceptingState) ",accepting" else ""
    val position: String = if(belowOf.isDefined)
      s"[below of=${belowOf.get.getName}]"
    else if (rightOf.isDefined)
      s"[right of=${rightOf.get.getName}]"
    else ""
    s"\\node[state$isStarting$isAccepting]   (${state.getName}) $position {$$q_$index$$};\n"
  }
  private def transToTikzEdge(input: Char, source: DFAState, dest: DFAState, bend: String = "right"): String = {
    if (source.equals(dest)) {
      s"(${source.getName}) edge  [loop above]    node {$input} ()"
    } else {
      if (bend == "right")
        s"(${source.getName}) edge  [bend $bend=45]    node [above] {$input} (${dest.getName})\n"
      else
        s"(${source.getName}) edge          node [above] {$input} (${dest.getName})\n" // try no bend so they don't meet // TODO fix this hack, need to keep proper coordinates for that!
    }
  }
}
