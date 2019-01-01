package viewer

import regexParser._

object HorizontalTreeViewer {
  def fromRegexTree(tree: RegexTree): String = {
    "ROOT\n" + convert(tree, "", Tag.RIGHT)
  }

  private val tr = Array(Operator.Concat -> "(+)", Operator.Union -> "(|)", Operator.KClosure -> "(*)").toMap

  private object Tag extends Enumeration {
    type Tag = Value
    val LEFT, RIGHT, INHLEFT, INHRIGHT = Value
  }
  private def convert(tree: RegexTree, pad: String, tag: Tag.Value): String = {
    val blank = "      "
    val (prefix, leftPad, rightPad, passTag: Tag.Value) =
      if (tag == Tag.RIGHT) ("`---- ", "|", " ", Tag.INHRIGHT)
      else if (tag == Tag.LEFT) (".---- ", " ", "|", Tag.INHLEFT)
      else if (tag == Tag.INHLEFT) (s".---- ${tr(Operator.KClosure)}---- ", "   " + blank, "|  " + blank, Tag.INHLEFT)
      else if (tag == Tag.INHRIGHT) (s"`---- ${tr(Operator.KClosure)}---- ", "|  " + blank, "   " + blank, Tag.INHRIGHT)
    tree match {
      case Leaf(ch) => pad + prefix + ch + "\n"
      case UnaryNode(op, child) => convert(child, pad, passTag)
      case BinNode(op, left, right) =>
        convert(left, pad + leftPad + blank, Tag.LEFT) +
          pad + prefix + tr(op) + "\n" +
          convert(right, pad + rightPad + blank, Tag.RIGHT)
    }
  }


}
