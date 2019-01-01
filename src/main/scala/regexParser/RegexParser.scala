package regexParser


trait RegexParser {
  protected def isAlpha(ch: Char): Boolean = {
    (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
  }
  protected def preprocess(str: String): String = {
    // insert '@' between, [letter letter], [letter '('], [')' letter]
    (str :\ List('#')) {(x: Char, carry: List[Char]) =>
      if ((isAlpha(x) && isAlpha(carry.head)) ||
          (isAlpha(x) && carry.head == '(') ||
          ((x == ')' || x == '*') && (isAlpha(carry.head) || carry.head == '('))) {
        x :: '@' :: carry
      } else x :: carry
    }.mkString
  }
  def getSymbols(regex: String): Traversable[Char] = {
    regex filter {ch => !"()|*".contains(ch)} toSet
  }
  def buildRegexTree(str: String): RegexTree
}


