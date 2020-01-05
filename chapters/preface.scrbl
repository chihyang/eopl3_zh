#lang scribble/book
@(require "style.rkt")
@(require latex-utils/scribble/utils)
@(require scribble-math)
@(require scribble-math/asymptote)
@(require scriblib/footnote)
@(require scribble-math/dollar)
@(use-mathjax)

@title[#:style '(toc unnumbered) #:tag "pf"]{前言}

@section[#:style 'unnumbered #:tag "pf-obj"]{目标}

本书是对程序语言的分析性研究。我们的目标是对程序语言的基本概念提供深入有效的理解。
这些概念的重要性久经证实。它们是理解程序语言未来发展的基础。

这些概念多关乎程序元素的语义，或称含义。含义反映了元素在程序执行时应当如何解释。
名为解释器的程序最直接地表现了程序语义，且能执行。它们直接分析程序文本的抽象表示，
从而处理程序。因此我们以解释器为主要手段，以便表达程序语言元素的语义。

程序作为对象，最富趣味的话题是：“它做什么？”解释器研究向我们阐明这点。解释器至
关重要，因为它发隐抉微，又是更高效编译和其他程序分析的不二法门。

有一大类系统，根据文法将信息从一种形式转换为另一形式，解释器也是它们的示例。例如，
编译器将程序转换为适合硬件或虚拟机解读的形式。虽然通常的编译技术超出本书范围，但
我们确实开发出了一些基本的程序翻译系统。它们反映了编译常用的程序分析形式，比如控
制转换，变量绑定解析，以及类型检查。

以下是我们采用的一些独特技巧：

@itemlist[

 @item{每一新概念都用一种小型语言解释。这些语言通常是递进的：后出语言依赖先出语
       言的特性。}

 @item{用语言处理器——如解释器和类型检查器——解释指定语言所写程序的行为。它们以形
       式化（无歧并且完备）和可执行的方式表现语言设计中的决定。}

 @item{适当的时候，我们用接口和规范建立数据抽象。如此，我们可以改变数据的表示而
       不必更改程序。我们以此来研究不同的实现策略。}

 @item{我们的语言处理器在较高层次写就，用以简括地表示语义；同时也在极低层次写就，
       用以理解实现技巧。}

 @item{我们展示了如何用简单的代数操作预测程序行为，并获得它们的属性。但是通常，
       我们很少采用数学符号，而是偏爱研究程序的行为，它们是我们语言实现的组成部
       分。}

 @item{正文解释关键概念，习题探讨备选设计和其它问题。例如，正文阐述静态绑定，习
       题讨论动态绑定。有一支习题将词法定界的概念用于本书设计的各种语言。}

]

应用多层次抽象，我们提出了看待程序语言的不同观点。我们的解释器常常提供高层次观点，
用十分简洁的方式表现语言的语义，这与形式数学语义相去不远。另一个极端是，我们展示
了程序如何转换为汇编语言特有的极低层次形式。通过一些小的步骤完成这一转换，我们保
证了高低层次观点之间的清晰联系。

我们对本版做了一些重大改动。对所有值得一书的定义，我们都引入了非正式合约。这有助
于阐明所做的抽象。此外，新增关于模块的章节。为了使实现更容易，第3, 4, 5, 7和第8
章的源语言假定只能给函数传递一个参数；我们增加了支持多参数过程的习题。第6章是新
增的，@elem[#:style question]{因为我们选择了一阶组合式续文传递风格变换而不是相关
的另一种}。同时，由于@elem[#:style question]{曳尾}（@emph{tail-form}）表达式的性
质，我们在这一章使用多参数过程。在对象和类一章同样如此，但这并非必然。每章都经过
修订，新增了很多习题。

@section[#:style 'unnumbered #:tag "pf-org"]{组织}

前两章为细致研究程序语言奠定了基础。@secref{isd}强调数据的归纳式定义法和递归编程
之间的联系，介绍了关于变量作用域的一些思想。@secref{da}介绍了数据类型工具，由此
引出关于数据抽象的讨论和表达形式转换的例子，这种转换将在后面的章节中使用。

第3章使用这些基础描述程序语言的行为。本章介绍了解释器，作为解释语言运行时行为的
机制，并为一门简单语言设计了解释器，该语言支持静态定界、一等过程和递归。这个解释
器是本书其余许多材料的基础。本章结束时，细致讨论了一种用索引代替变量的语言，因此，
这语言可通过列表引用来查询变量。

第4章介绍了新的部分，状态，用以将位置对应到值。一旦引入状态，我们可以研究关于表
示的各种问题。此外，它使我们能够探索参数传递机制：按址调用、按名调用、以及按需调
用。

第5章用续文传递风格重写我们的基础解释器。运行解释器所需的控制结构随之从递归转变
为迭代。这揭示了解释性语言的控制机制，强化了对控制问题的整体直觉。它也使我们能够
用蹦床, 异常处理和多线程机制扩展语言。

第6章是上一章的姊妹篇。上一章我们展示了如何把我们熟悉的解释器转换为续文传递风格；
本章我们展示了如何为更大的一类程序完成转换。续文传递风格是一种强大的编程工具，因
为它使所有顺序控制机制能在几乎任何语言中实现。这一算法也是源到源程序转换抽象描述
的例子。

第7章将第3章的语言转换为有类型的语言。首先我们实现一个类型检查器。然后我们展示如
何用基于合一的类型推导算法推断程序中的类型。

第8章建立类型模块，这极度依赖对前一章的理解。模块既使我们能够建立和强化抽象边界，
又提供了新型的定界方式。

第9章以类为中心，展示了面向对象程序语言的基本概念。我们首先设计了高效的运行时结
构，作为本章第二部分内容的基础。第二部分将第7章的类型检查器思想和第一部分的面向
对象程序语言结合起来，结果便是传统的有类型面向对象语言。这要求引入新的概念，包括
接口，抽象方法和转换。

“阅读更多”解释了本书的每一思想源于何处。虽然有时我们只列出了能接触到的来源，读
者泛览本章，仍能窥源访本。

最后，附录 B 介绍了我们的 SLLGEN 解析系统。

各章的依赖关系如下图所示。

@centered{
@(image "../images/structure"
  #:suffixes (list ".pdf" ".svg")
  "章节依赖关系")
}

@section[#:style 'unnumbered #:tag "pf-use"]{使用}

本书在本科生和研究生课程中均已使用，业已在职业程序员的持续教育课程中使用。我们假
定的背景知识有数据结构，过程式语言（如C, C++, Java）和Scheme, ML, Python或
Haskell的编程经验。

习题是文本的重要部分，散见于各处。它们难易有别，简单者，理解相关材料便轻而易举
@; @difficulty[1]
（标为[@${\star}]），困难者，须花费大量思考和编程工作（标为
@; @difficulty[3]
[@${\star\star\star}]）。大量关于应用，历史以及理论的材料潜藏其间。我们建议读一
读每道习题，想一想如何解决它们。虽然我们用Scheme编写解释程序和转换系统，任何支持
一等过程和赋值的语言（ML, Common Lisp, Python, Ruby等等）都足以完成本书练习。
@linebreak{}

@exercise[#:level 1 #:tag "ex0.1"]{

 我们常说“某语言具有某属性”。为每种说法找出一种或多种具有该属性的语言，以及一
 种或多种不具有该属性的语言。请随意搜索这些信息，不拘任何程序语言的介绍性书籍
 （比如Scott(2005)，Sebesta (2007)，或者Pratt \& Zelkowitz(2001)）。@linebreak[]
}

这是本手册：本书讨论的一切都可在通常的大学课程限度内完成。因为函数式语言的抽象特
性尤其适合这类编程，我们可以写出大量语言处理系统，既简洁，又能以适当的努力掌握。

网站由出版者提供，包含本书所有解释器和分析器的完整Scheme代码。代码用PLT Scheme写
成。@pf-note{本书网站已迁移至@url{http://www.eopl3.com}，代码改用Racket实现，网
址为@url{https://github.com/mwand/eopl3}。}我们选择这种Scheme实现，因为它的模块
系统和编程环境对学生助益良多。代码多半兼容于R@superscript{5}RS，当能轻易移植到任
何功能完整的Scheme实现。

@define-footnote[pf-note pf-nt]
@pf-nt[]
