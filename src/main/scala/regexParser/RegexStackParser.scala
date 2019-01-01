package regexParser

object RegexStackParser extends RegexParser {
  override def buildRegexTree(str: String): RegexTree = {
    buildParseTree(preprocess(str))
  }

  val precedence = Map(
    // symbol -> (inStack, offStack)
    '(' -> (0, 7),
    ')' -> (7, 0),
    '*' -> (6, 5),
    '@' -> (4, 3),
    '|' -> (2, 1),
    '#' -> (-1, -1)
  )

  private def buildParseTree(str: String): RegexTree = {

    var operands = List[RegexTree]()
    var operators = List[Char]()

    def evaluate(): RegexTree = {
      val oper = operators.head
      operators = operators.tail
      oper match {
        case '*' =>
          val opd = popOperands()
          UnaryNode(Operator.KClosure, opd)
        case '|' =>
          val right = popOperands()
          val left = popOperands()
          BinNode(Operator.Union, left, right)
        case '@' =>
          val right = popOperands()
          val left = popOperands()
          BinNode(Operator.Concat, left, right)
      }
    }
    def popOperands(): RegexTree = {
      val ret = operands.head
      operands = operands.tail
      ret
    }
    def prevPrecedence(): Int = {
      if (operators.isEmpty) -1
      else getPrecedenceOf(operators.head)._1
    }
    def getPrecedenceOf(ch: Char) = {
      precedence.get(ch) match {
        case Some((in, out)) => (in, out)
        case None => throw new IllegalArgumentException
      }
    }

    for (i <- str) {
      if (isAlpha(i)) operands = Leaf(i) :: operands
      else { // operator
        val outPre = getPrecedenceOf(i)._2
        val inPre = prevPrecedence()
        if (outPre > inPre) operators = i :: operators
        else { // evaluate
          while (prevPrecedence() > outPre) {
            val newTree = evaluate()
            operands = newTree :: operands
          }
          if (i == ')') operators = operators.tail
          else {
            operators = i :: operators
          }
        }
      }
    }
    operands.head
  }
}