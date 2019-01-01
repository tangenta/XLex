package regexParser

import scala.util.parsing.combinator._
object RegexCombParser extends RegexParsers with RegexParser {

  import regexParser.Operator._

  private def union: Parser[RegexTree] = concat ~ rep("|" ~> concat) ^^ {
    case first ~ tail => (first /: tail) {
      BinNode(Union, _, _)
    }
  }

  private def concat: Parser[RegexTree] = kclosure ~ rep(kclosure) ^^ {
    case first ~ tail => (first /: tail) {
      BinNode(Concat, _, _)
    }
  }

  private def kclosure: Parser[RegexTree] = symbol ~ rep("*") ^^ {
    case first ~ tail => (first /: tail) { (tree, _) =>
      UnaryNode(KClosure, tree)
    }
  }

  private def symbol: Parser[RegexTree] = (
    "(" ~> union <~ ")"
      |
      """[a-zA-Z]""".r ^^ { str => Leaf(str.charAt(0)) }
    )

  override def buildRegexTree(str: String): RegexTree =
    parseAll(union, str).get
}
