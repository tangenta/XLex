package automata
import regexParser._

import scala.collection.mutable.ArrayBuffer

class DFA private (override val begin: State,
                   override val end: Set[State],
                   private val table: Array[State]) extends Automata {
  override def getState(sid: Int): State = table(sid)
  override val getAllState: Traversable[State] = table

  override def simulate(input: String): Boolean = {
    def helper(input: String, stateID: Option[Int]): Boolean = {
      stateID match {
        case None => false
        case Some(i) =>
          if (input.isEmpty) {
            end map (_.id) contains i
          } else helper(input.tail, move(i, input.head))
      }
    }
    helper(input, Some(begin.id))
  }

  def move(curStateID: Int, symbol: Char): Option[Int] = {
    table(curStateID).edges find {_.symbol == symbol} map {_.next}
  }
}

object DFA extends AutomataFactory {

  // construct dfa from regexTree directly
  override def fromRegexTree(regexTree: RegexTree): DFA = {
    trait ExNode {
      val nullable: Boolean
      val firstPos: Set[Int]
      val lastPos: Set[Int]
    }
    trait Position {
      val posId: Int
    }

    var id = -1
    val symbols = ArrayBuffer[Char]()

    def produceIdAndBind(sym: Char): Int = {
      symbols += sym
      id += 1
      id
    }

    def buildInfoTree(regexTree: RegexTree): RegexTree with ExNode = {
      regexTree match {
        case Leaf(ch) => new Leaf(ch) with Position with ExNode {
          override val posId: Int = produceIdAndBind(ch)
          override val nullable: Boolean = false
          override val firstPos: Set[Int] = Set(posId)
          override val lastPos: Set[Int] = Set(posId)
        }
        case UnaryNode(op, child) => val childTree = buildInfoTree(child)
          new UnaryNode(op, childTree) with ExNode {
            override val nullable: Boolean = true
            override val firstPos: Set[Int] = childTree.firstPos
            override val lastPos: Set[Int] = childTree.lastPos
          }
        case BinNode(op, left, right) => val (lc, rc) = (buildInfoTree(left), buildInfoTree(right))
          op match {
            case Operator.Union => new BinNode(op, lc, rc) with ExNode {
              override val nullable: Boolean = lc.nullable || rc.nullable
              override val firstPos: Set[Int] = lc.firstPos ++ rc.firstPos
              override val lastPos: Set[Int] = lc.lastPos ++ rc.lastPos
            }
            case Operator.Concat => new BinNode(op, lc, rc) with ExNode {
              override val nullable: Boolean = lc.nullable && rc.nullable
              override val firstPos: Set[Int] = if (lc.nullable) lc.firstPos ++ rc.firstPos else lc.firstPos
              override val lastPos: Set[Int] = if (rc.nullable) lc.lastPos ++ rc.lastPos else rc.lastPos
            }
          }
      }
    }

    // Array[m.Set[Int]]
    def buildFollowPosTable(regexTree: RegexTree, table: Array[Set[Int]]): Array[Set[Int]] = {
      regexTree match {
        case UnaryNode(_, child) =>
          val childEx = child.asInstanceOf[RegexTree with ExNode]
          for (i <- childEx.lastPos) {
            val newValue = table(i) ++ childEx.firstPos
            table.update(i, newValue)
          }
          buildFollowPosTable(childEx, table)
        case BinNode(Operator.Concat, left, right) =>
          val leftEx = left.asInstanceOf[RegexTree with ExNode]
          val rightEx = right.asInstanceOf[RegexTree with ExNode]
          for (i <- leftEx.lastPos) {
            val newValue = table(i) ++ rightEx.firstPos
            table.update(i, newValue)
          }
          buildFollowPosTable(leftEx, table)
          buildFollowPosTable(rightEx, table)
        case _ => table   // remain unchanged
      }
    }

    // append a termination symbol '#' to regex
    val aRegexTree = BinNode(Operator.Concat, regexTree, Leaf('#'))

    val finalTree = buildInfoTree(aRegexTree)

    val followPosTable = buildFollowPosTable(finalTree, Array.fill(id + 1){Set[Int]()})

    // construct DFA
    type Pos = Int
    type DState = Set[Pos]
    type DEdge = (Char, DState)
    def exploreEdgesOf(state: DState): Set[DEdge] = {
      state.groupBy(symbols(_)).collect { case (ch, set) if ch != '#' =>
        ch -> set.flatMap {followPosTable(_)}
      }.toSet
    }
    def collectFrom(state: DState, result: Map[DState, Set[DEdge]]): Map[DState, Set[DEdge]] = {
      val edges = exploreEdgesOf(state)
      val nextStates = edges.map(_._2)
      val updatedResult = result.updated(state, edges)
      val notIncludedStates = nextStates &~ result.keys.toSet
      if (notIncludedStates.nonEmpty) {
        notIncludedStates.foldLeft(updatedResult) {(acc, elem) => collectFrom(elem, acc)}
      } else updatedResult
    }
    val dStateTable = collectFrom(finalTree.firstPos, Map())

    // substitute DState with index, which stands for state id
    val dStateIndexMap = dStateTable.keys.zipWithIndex.toMap
    val idStateTable = dStateTable.map { case (dstate, set) =>
      (dStateIndexMap(dstate), set.map { case (ch, state) =>
        (ch, dStateIndexMap(state))
      })
    }

    // prepare arguments for DFA constructor
    val beginStateID = dStateIndexMap(finalTree.firstPos)
    val endStatesID = dStateIndexMap.collect {
      case (dState, index) if (dState & finalTree.lastPos).nonEmpty => index
    } toSet
    val table = idStateTable.map { case (sid, edges) =>
        State(sid, edges.map { case (sym, next) => Edge(sym, next)})
    } toArray

    new DFA(table(beginStateID), endStatesID map table.apply, table)
  }


  // method: subset construction
  def fromNFA(nfa: NFA): DFA = {
    import scala.collection.{mutable => m}

    type NState = Set[Int]
    type NEdge = (Char, NState)

    val initialStates = nfa.epsilonTransition(nfa.begin.id)

    val bufferMap: m.Map[NState , Set[NEdge]] = m.Map()
    val pendingStates: m.Queue[NState] = m.Queue(initialStates)

    while (pendingStates.nonEmpty) {
      val dealing = pendingStates.dequeue()

      val edgeCollecter = (nfa.allSymbols-NFA.epsilon) map { symbol =>
        (symbol, nfa.move(dealing, symbol))
      } filter {_._2.nonEmpty}   // throw empty-state away

      // put the dealing stateGroup into bufferMap
      bufferMap += (dealing -> edgeCollecter)

      // update pendingStates
      edgeCollecter.foreach { case (_, newStates) =>
          if (!bufferMap.contains(newStates))
            pendingStates enqueue newStates
      }
    }

    // represent each stateGroup with a index
    val stateIndexMap = bufferMap.keys.zipWithIndex.toMap
    val resultTable = bufferMap.values.zipWithIndex.toArray map {
      elem =>
        State(elem._2, elem._1.map(iedge =>
          Edge(iedge._1, stateIndexMap(iedge._2))
        ))
    }
    val endingStates = (stateIndexMap filter {elem =>
      elem._1 & nfa.end.map(_.id) nonEmpty}).values.toSet

    new DFA(resultTable(stateIndexMap(initialStates)), endingStates map resultTable.apply, resultTable)
  }

  // method: group splitting
  def minimize(dfa: DFA): DFA = {
    type StatesG = Set[Int]

    val endStatesId = dfa.end map {_.id}
    val initialGroups = List(endStatesId, (0 until dfa.getAllState.size).toSet &~ endStatesId).filter(_.nonEmpty)
    def helper(groups: List[StatesG]): List[StatesG] = {
      val result = groups.foldRight(List[StatesG]()) {(singleGroup, acc) => // List[StatesG]
        if (singleGroup.size == 1) singleGroup :: acc   // ignore single-state group
        else {
          // guarantee arbitrary 2 states in a group share the same transitions to some group
          val groupMap = singleGroup groupBy { singleStateID => // Set[Int]
            dfa.allSymbols map { symbol =>
              dfa.move(singleStateID, symbol) map { intElem =>
                (symbol, groups.find(_.contains(intElem)))
              }
            }
          }
          groupMap.values.toList ++ acc
        }
      }
      if (result.size == groups.size) groups    // nothing can be split
      else helper(result)
    }
    val finalGroups = helper(initialGroups)

    // (stateGroup: set[Int] -> index: Int)
    val newSetIndexMap = finalGroups.zipWithIndex.toMap
    // (oldStateId: Int -> newStateId: Int)
    val interStateMap =
      for ((setKey, index) <- newSetIndexMap;
           oldState <- setKey) yield oldState -> index

    // new DFA arguments
    val resultTable = finalGroups map { stateGroup =>
      State(newSetIndexMap(stateGroup), stateGroup flatMap { singleState =>   // produce an array of State
        dfa.getState(singleState).edges map { edge =>
          Edge(edge.symbol,interStateMap(edge.next))
        }
      })
    } toArray
    val beginningStates = resultTable(interStateMap(dfa.begin.id))
    val endingStates = endStatesId.map {oldId => resultTable(interStateMap(oldId))}
    new DFA(beginningStates, endingStates, resultTable)
  }

}