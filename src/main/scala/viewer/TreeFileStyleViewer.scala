package viewer
import regexParser._

object TreeFileStyleViewer {
  def print(tree: RegexTree): Unit = {
    printAll(tree, "")
  }
  private def printAll(tree: RegexTree, indent: String): Unit = {
    tree match {
      case BinNode(oper, left, right) =>
        println(s"$indent($oper)")
        printAll(left, indent + "|  ")
        printAll(right, indent + "|  ")
      case UnaryNode(oper, child) =>
        println(s"$indent($oper)")
        printAll(child, indent + "|  ")
      case Leaf(ch) =>
        println(s"$indent$ch")
    }
  }

}
