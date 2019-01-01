package regexParser

object RegexRecurParser extends RegexParser {

  override def buildRegexTree(str: String): RegexTree = {
    getUnion(preprocess(str).init)
  }

  private def getUnion(str: String): RegexTree = {
    val splitIndex = lastIndexOfOper(str, '|')
    if (splitIndex == -1) {
      getConcat(str)
    }
    else {
      val (front, behind) = str.splitAt(splitIndex)
      BinNode(Operator.Union, getUnion(front), getConcat(behind.tail))
    }
  }

  private def getConcat(str: String): RegexTree = {
    val splitIndex = lastIndexOfOper(str, '@')
    if (splitIndex == -1) getKClosure(str)
    else {
      val (front, behind) = str.splitAt(splitIndex)
      BinNode(Operator.Concat, getConcat(front), getKClosure(behind.tail))
    }
  }

  private def getKClosure(str: String): RegexTree = {
    if (str.endsWith("*")) UnaryNode(Operator.KClosure, getUnit(str.init))
    else getUnit(str)
  }

  private def getUnit(str: String): RegexTree = {
    if (str.startsWith("(") && str.endsWith(")")) getUnion(str.init.drop(1))
    else if (str.length == 1) Leaf(str.head)
    else {
      throw new IllegalArgumentException
    }
  }

  private def lastIndexOfOper(str: String, ch: Char): Int = {
    var result = -1
    var insideParen = 0
    var found = false
    for (i <- str.indices.reverse) {
      if (str.charAt(i) == ')') insideParen += 1
      else if (str.charAt(i) == '(') insideParen -= 1
      else {
        if (insideParen == 0 && str.charAt(i) == ch && !found) {
          result = i
          found = true
        }
      }
    }
    result
  }

}
