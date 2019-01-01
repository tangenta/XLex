## XLex——词法分析程序生成器

目录结构：
```
.
├── demo.c  # 结果演示程序：利用生成的代码段，识别正则表达式的交互小程序
├── lab2.txt  # 项目要求
├── LICENSE
├── main
│   ├── java
│   │   ├── MainWin.form  # IDEA图形界面
│   │   └── MainWin.java  # 程序入口
│   └── scala
│       ├── automata
│       │   ├── Automata.scala  # 自动状态机抽象基类
│       │   ├── DFA.scala  # DFA类及单例
│       │   └── NFA.scala  # NFA类及单例
│       ├── regexParser  # 正则表达式->语法树 解析器
│       │   ├── RegexCombParser.scala  # 组合子法解析
│       │   ├── RegexParser.scala  # 解析器接口
│       │   ├── RegexRecurParser.scala  # 字符串切割法解析
│       │   ├── RegexStackParser.scala  # 栈方法解析
│       │   └── RegexTree.scala  # 正则语法树及状态机的基本元素State和Edge
│       ├── Testing.scala
│       ├── viewer  # 查看器
│       │   ├── AutomataViewer.scala  # 自动状态机查看
│       │   ├── HorizontalTreeViewer.scala  # 语法树查看
│       │   └── TreeFileStyleViewer.scala
│       └── xlex
│           ├── CppConverter.scala  # C程序转换
│           └── Xlexer.scala  # 状态机模拟
├── README
└── Xlex-introduction.pdf  #  项目介绍

7 directories, 21 files

```
