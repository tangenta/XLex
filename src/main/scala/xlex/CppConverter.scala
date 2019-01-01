package xlex

import automata.DFA

object CppConverter {
  private val indent = "      "
  def convert(dfa: DFA, funcName: String = "recognize", getSymbolFuncName: String = "getSymbol",
              retractFuncName: String = "retract"): String = {
    // initialize
    val code = s"void $funcName() {\n\tint stateId = ${dfa.begin.id};\n\tint timeToExit = 0;"
    def caseTemplate(stateId: Int, edgeNextPairList: List[(Char, Int)]): String = {
      if (edgeNextPairList.isEmpty) s"${indent}case $stateId: $retractFuncName(); timeToExit = 1; break;" else {
        edgeNextPairList.map(elem => s"(ch == '${elem._1}') stateId = ${elem._2};").
          mkString(s"${indent}case $stateId:\nif ", "\nelse if ",
            s"""
               |else {
               |  $retractFuncName();
               |  timeToExit = 1;
               |}
               |break;
          """.stripMargin).foldLeft("") { (result, ch) => if (ch == '\n') result + ch + indent else result + ch }
      }
    }

    s"""
       |void $funcName() {
       |  int stateId = ${dfa.begin.id};
       |  int timeToExit = 0;
       |  while (!timeToExit) {
       |    char ch = $getSymbolFuncName();
       |    switch (stateId) {
       |${
      dfa.getAllState.map { state =>
        caseTemplate(state.id, state.edges.toList.map { edge =>
          (edge.symbol, edge.next)
        })
      } mkString "\n"
    }
       |    }
       |  }
       |}
    """
      .stripMargin
  }

}
