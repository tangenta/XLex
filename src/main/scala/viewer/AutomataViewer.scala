package viewer

import automata._
import automata.Edge

object AutomataViewer {
  def debugStyle(nfa: Automata): String = {
    def helper(state: State): String = {
      val charSetMap: Map[Char, Set[Edge]] = state.edges.groupBy(e => e.symbol)
      charSetMap.map { case (symbol, set) => s"$symbol: ${
        set.map {_.next}.mkString("{", ", ", "}")
      }"}.mkString(s"${state.id}  ", " ", "")
    }
    nfa.getAllState map helper mkString "\n"
  }

  def prettyStyle(atma: Automata): String = {
    case class Field(head: String, items: List[String]) {
      private def formatStringToWidth(width: Int)(str: String): String = {
        "| " + str + Array.fill(width - str.length)(' ').mkString("") + " |"
      }
      def toStringList: List[String] = {
        val width = head.length.max(items.maxBy(_.length).length)
        val border = Array.fill(width+2)('-').mkString("+", "", "+")
        border :: formatStringToWidth(width)(head) :: border :: items.map(formatStringToWidth(width)) ::: List(border)
      }
    }
    val firstCol = Field("state_id", atma.getAllState.map(_.id.toString).toList)
    val restCols = atma.allSymbols.toList map { ch =>
      Field(ch.toString, atma.getAllState.map {state =>
        state.edges.collect {
          case Edge(symbol, next) if symbol == ch => next
        }.mkString(",")
      } toList)
    }
    val result = restCols.map(_.toStringList).foldLeft(firstCol.toStringList) {(acc: List[String], elem) =>
      acc.zip(elem) map { case (first, rest) =>
        first + rest.tail
      }
    } mkString "\n"
    result + s"\nbegin state: ${atma.begin.id}\nend state: ${atma.end.map(_.id).mkString(",")}"

  }


}
