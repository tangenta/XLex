package regexParser

trait RegexTree
object Operator extends Enumeration {
  type Operator = Value
  val Concat, Union, KClosure = Value
}
import Operator._
case class BinNode(operator: Operator, left: RegexTree, right: RegexTree) extends RegexTree
case class UnaryNode(operator: Operator, child: RegexTree) extends RegexTree
case class Leaf(symbol: Char) extends RegexTree