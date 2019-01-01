package automata
import regexParser.{BinNode, Leaf, RegexTree, UnaryNode}

import scala.collection.mutable.ArrayBuffer

class NFA private (override val begin: State,
                   override val end: Set[State],
                   private val table: Array[State]) extends Automata {
  override def getState(sid: Int): State = table(sid)
  override val getAllState: Traversable[State] = table
  override val allSymbols: Set[Char] = super.allSymbols
  override def simulate(input: String): Boolean = {
    def helper(input: String, curStates: Set[Int]): Boolean = {
      if (curStates.isEmpty) false
      else if (input.isEmpty) {
        end & (curStates map table.apply) nonEmpty
      } else {
        helper(input.tail, move(curStates, input.head))
      }
    }
    helper(input, Set(begin.id))
  }

  def epsilonTransition(stateID: Int): Set[Int] = {
    def helper(state: State, result: Set[Int]): Set[Int] = {
      val connectedStatesID = state.edges collect {
        case elem if elem.symbol == NFA.epsilon => elem.next
      }
      val escapingFish = connectedStatesID &~ result
      if (escapingFish.isEmpty) // everything is included in result
        result + state.id
      else {
        val accumulate = result ++ escapingFish + state.id
        escapingFish flatMap { i => helper(table(i), accumulate) }
      }
    }
    helper(table(stateID), Set())
  }
  def epsilonTransition(statesID: Traversable[Int]): Set[Int] = {
    statesID.foldLeft(Set[Int]()) {_ ++ epsilonTransition(_)}
  }
  def move(curStateID: Int, symbol: Char): Set[Int] = {
    val result = epsilonTransition(curStateID) flatMap {
      table(_).edges collect {
        case edge if edge.symbol == symbol =>
          edge.next
      }
    }
    epsilonTransition(result)
  }
  def move(curStatesID: Set[Int], symbol: Char): Set[Int] = {
    curStatesID.foldLeft(Set[Int]()) {_ ++ move(_, symbol)}
  }


}

object NFA extends AutomataFactory {
  val epsilon = 'ℇ'
  import scala.collection.{mutable => m}
  private case class MState(id: Int, edges: m.Set[Edge] = m.Set()) {
    def toState: State = State(id, edges.toSet)
  }
  override def fromRegexTree(regexTree: RegexTree): NFA = {
    var id = 0
    var buffer = ArrayBuffer[MState]()

    def newState(): MState = {
      val newState = MState(id)
      buffer += newState
      id += 1
      newState
    }

    def connect(from: MState, to: MState, symbol: Char): Unit = {
      from.edges += Edge(symbol, to.id)
    }

    def helper(regexTree: RegexTree): (MState, MState) = { // return (beginState, endState)
      regexTree match {
        case Leaf(symbol) =>
          val (b, e) = (newState(), newState())
          connect(b, e, symbol)
          (b, e)
        case UnaryNode(_, child) =>
          val (ob, oe) = helper(child)
          connect(oe, ob, epsilon)
          val (nb, ne) = (newState(), newState())
          connect(nb, ob, epsilon)
          connect(nb, ne, epsilon)
          connect(oe, ne, epsilon)
          (nb, ne)
        case BinNode(operator, left, right) =>
          import regexParser.Operator._
          operator match {
            case Concat =>
              val (b, leftEnd) = helper(left)
              val (rightBegin, e) = helper(right)
              connect(leftEnd, rightBegin, epsilon)
              (b, e)
            case Union =>
              val (lb, le) = helper(left)
              val (rb, re) = helper(right)
              val (nb, ne) = (newState(), newState())
              connect(nb, lb, epsilon)
              connect(nb, rb, epsilon)
              connect(le, ne, epsilon)
              connect(re, ne, epsilon)
              (nb, ne)
          }
      }
    }
    val (begin, end) = helper(regexTree)
    new NFA(begin.toState, Set(end.toState), buffer.map(_.toState).toArray)
  }
}