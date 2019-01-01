package automata

import regexParser.RegexTree

case class State(id: Int, edges: Set[Edge])
case class Edge(symbol: Char, next: Int)

abstract class Automata {
  val begin: State
  val end: Set[State]
  def getState(sid: Int): State
  def getAllState: Traversable[State]
  def simulate(input: String): Boolean
  def allSymbols: Set[Char] = {
    import scala.collection.{mutable => m}
    val visited = m.Set[Int]()
    val symbols = m.Set[Char]()
    def visit(state: State): Unit = {
      if (!visited.contains(state.id)) {
        visited += state.id
        symbols ++= state.edges map {_.symbol}
        state.edges foreach {edge => visit(getState(edge.next))}
      }
    }
    visit(begin)
    symbols.toSet
  }
}

trait AutomataFactory {
  def fromRegexTree(regexTree: RegexTree): Automata
}