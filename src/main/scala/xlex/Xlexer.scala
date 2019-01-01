package xlex

import automata.{DFA, NFA}
import regexParser.{RegexCombParser, RegexParser, RegexTree}

class Xlexer private (val regex: String,
                      val parser: RegexParser,
                      private val dfaEnabled: Boolean,
                      private val nfaEnabled: Boolean,
                      private val minimizeEnabled: Boolean) {
  val regexTree: RegexTree = parser.buildRegexTree(regex)
  val nfa: Option[NFA] = if (nfaEnabled) Some(NFA.fromRegexTree(regexTree)) else None
  val dfa: Option[DFA] =
    if (dfaEnabled) {
      if (nfaEnabled) {
        val tmp = nfa.map(DFA.fromNFA)
        if (minimizeEnabled) tmp.map(DFA.minimize) else tmp
      } else Some(DFA.fromRegexTree(regexTree))
    } else None

  def simulate(input: String): Boolean = {
    if (dfaEnabled) dfa.get.simulate(input)
    else nfa.get.simulate(input)
  }
}

object Xlexer {
  class Builder(val regex: String) {
    private var dfaEnabled: Boolean = true
    private var nfaEnabled: Boolean = true
    private var minimizeEnabled: Boolean = true
    private var parser: RegexParser = RegexCombParser

    def skipDFA(bool: Boolean = true): Builder = {
      dfaEnabled = !bool
      this
    }
    def skipNFA(bool: Boolean = true): Builder = {
      nfaEnabled = !bool
      this
    }
    def skipMinimizeDFA(bool: Boolean = true): Builder = {
      minimizeEnabled = !bool
      this
    }
    def setParser(parser: RegexParser): Builder = {
      this.parser = parser
      this
    }
    def build(): Xlexer = {
      if (!dfaEnabled && !nfaEnabled) throw new IllegalArgumentException
      else new Xlexer(regex, parser, dfaEnabled, nfaEnabled, minimizeEnabled)
    }
  }
}

