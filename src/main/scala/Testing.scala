import regexParser.{RegexCombParser, RegexRecurParser, RegexStackParser}
import viewer.{AutomataViewer, TreeFileStyleViewer}
import automata.{DFA, NFA}
import xlex.{CppConverter, Xlexer}

object Testing extends App {
  val regexStr = "(a|b)*abb"
  val treeComb = RegexCombParser.buildRegexTree(regexStr)
  //println(RegexCombParser.getSymbols(regexStr))
//  val treeStack = RegexStackParser.buildRegexTree(regexStr)
//  val treeRecur = RegexRecurParser.buildRegexTree(regexStr)
//  println("\n\ntreeComb: ")
//  TreeFileStyleViewer.print(treeComb)
//  println("\n\ntreeStack: ")
//  TreeFileStyleViewer.print(treeStack)
//  println("\n\ntreeRecur: ")
//  TreeFileStyleViewer.print(treeRecur)

  val atma = NFA.fromRegexTree(treeComb)

  //AutomataViewer.print(atma)

  //println(atma.epsilonTransition(atma.begin.id))

//  val newDfa = DFA.fromRegexTree(treeComb)

//  println("This is a DFA...")
  val dfa = DFA.fromNFA(atma)
//  AutomataViewer.print(dfa)
//  println(s"begin: ${dfa.begin.id}")
//  println(s"ends: ${dfa.end map {_.id} mkString ", "}")
//
//  println("---------------------------")
//
//  AutomataViewer.print(newDfa)
//  println(s"begin: ${newDfa.begin.id}")
//  println(s"ends: ${newDfa.end map {_.id} mkString ", "}")
//
//  println("---------------------------")

  val minimizeDFA = DFA.minimize(dfa)
//  AutomataViewer.print(minimizeDFA)
//  println(s"begin: ${minimizeDFA.begin.id}")
//  println(s"ends: ${minimizeDFA.end map {_.id} mkString ", "}")

  //println(CppConverter.convert(minimizeDFA))
  println(AutomataViewer.prettyStyle(minimizeDFA))

}

object Test3 extends App {
  val lexer = new Xlexer.Builder("(a|b)*abb").setParser(RegexStackParser).build()
  val testCase = Array("abbbbabb", "bbabb", "bbabbb", "babaabb")
  testCase map lexer.simulate foreach println
}
