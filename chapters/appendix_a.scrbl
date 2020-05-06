#lang scribble/book
@(require "style.rkt"
          "bibliography.rkt"
          latex-utils/scribble/math
          latex-utils/scribble/utils
          (only-in scribble-abbrevs
                   appendix)
          scribble/manual
          scribble-math
          scribble/example
          scribble/core
          scribble/example
          scriblib/footnote
          racket/sandbox)

@appendix

@title[#:tag "further-reading"]{更多阅读}

这里的一些阅读材料教会，影响，或者启发了我们创作本书。希望你能像我们一样，至少喜
欢其中的一部分。

不熟悉递归编程和符号计算的读者可以看看 @emph{The Little Schemer}
@~cite["the-little-schemer"]，或 @emph{The Little MLer}
@~cite["the-little-mler"]，或者有考据癖，看看@emph{The Little LISPer} (Friedman,
1974)。作为计算第一课，@emph{How to Design Programs} (Felleisen et al., 2001)深
入探讨了如何递归编程。

用归纳法定义集合和关系，是数理逻辑中久已存在的技术。我们的自底向上和推理规则式归
纳大致效仿 Plotkin (1975, 1981) 的工作。我们的“自顶向下”式归纳效仿另一种技术，
名为@emph{余归纳} (@emph{coinduction})（参见 Gordon, 1995; Jacobs & Rutten,
1997），Felleisen et al. (2001) 也使用了这种技术。

上下文无关语法是语言学和计算机科学的标准工具。大多数编译器书籍，比如 Aho et
al. (2006)，都对语法和解析算法进行了大篇幅的讨论。将具体语法和抽象语法分开的思想
通常归功于 McCarthy (1962)。他强调了用接口来抽象语法树。

我们的口号@emph{遵循语法}基于@emph{结构化归纳法}，由 Brustall (1969) 提出。
@emph{子目标归纳} (@emph{subgoal induction}) (Morris &Wegbreit, 1977) 是证明递归
过程正确性的有效方法，即使它们不@bold{遵循语法}。不变式约束了过程的可能输入时，
子目标归纳也有效。

@emph{泛化} (@emph{generalization}) 是源自数学的标准技术，常用来证明某个特定陈述
是某个更通用陈述的特例。我们把额外参数描述为上下文的抽象，是受到属性语法 (Knuth,
1968) 中的@emph{继承属性} (@emph{inherited attributes}) 启发。

我们的构造器 @tt{define-datatype} 和 @tt{cases} 是受 ML 的 @tt{datatype} 和模式
匹配工具启发，详见 Milner et al. (1989) 及其修订版 Milner et al. (1997)。

Lambda 演算由邱奇发明 (Church, 1941)，用于研究数理逻辑，但已成为诸多现代编程语言
理论的灵感。Lambda 演算的介绍参见 Hankin (1994)、Peyton Jones (1987)或 Stoy
(1977)。Barendregt (1981, 1991) 提供了百科全书式的参考。

图3.13那样的等深线用来解释词法作用域，首先由 Johnston (1971) 提出。无名解释器和
翻译器基于德布鲁金索引 (de Bruijn, 1972)。

Scheme 由 Sussman & Steele (1975) 发明。其开发过程记录在 Steele & Sussman
(1978); Clinger et al. (1985a); Rees et al. (1986); Clinger et al. (1991);
Kelsey et al. (1998)。Scheme 标准由 IEEE standard (IEEE, 1991) 和
@emph{@${\textit{Revised}^6} Report on the Algorithmic Language Scheme} (Sperber
et al., 2007) 制定。

Dybvig (2003) 简短介绍了 Scheme，加入了许多富有洞见的例子。

解释器思想至少能追溯到图灵，他定义了能够模拟任何图灵机的“通用”机器。这种通用机
器实际上是一个解释器，取一描述图灵机的编码，模拟解码机器 (Turing, 1936)。经典的
冯诺依曼机 (von Neumann, 1945) 同样是硬件实现的解释器，用来解释机器语言程序。

对解释器的现代应用可追溯到 McCarthy (1960)，他提出了@emph{自循环解释器}
(@emph{metacircular interpreter})（用待定语言本身写就的解释器），用来解释 Lisp
的能力。当然，这样的解释器带来一大难题：如果待定语言由自身定义，我们要理解语言的
定义，就要先理解这种语言。确实，即使解释器不是自循环的，也会面临同样的问题。读者
理解要定义的事物之前，仍需理解书写定义用的语言。

这些年来，大量技术用来解决这一难题。我们把解释器视为方程定义的转写 (Goguen et
al., 1977) 或者 Plotkin (1975, 1981) 式的大步操作语义。这依赖相当直观的数学。

指称语义是另一种根据数学定义语言的技术。这种方法将解释器替换为一个函数，该函数把
每个待定语言的程序翻译为定义其行为的数学对象。Plotkin (1977) 对这种技术做了无可
替代的介绍，Winskel (1993) 做了更宽泛的探讨。Milne & Strachey (1976) 是本百科全
书，研究了如何用这种技术建模大量语言特性，

另一种方式是写出待定语言子集的解释器。例如，@secref{state}的解释器依靠 Scheme 的
存储器来解释存储器的概念，但它们只用了一个全局可变对象，而不是 Scheme 可变变量的
所有能力。

将计算视为操作存储器的思想可追溯到现代计算（参见 von Neumann, 1945）的开端。
EXPLICIT-REFS 的设计基于 ML (Milner et al., 1989) 的存储器模型，而它与 Bliss
(Wulf, 1971) 类似。IMPLICIT-REFS 的设计类似于大多数具有可变局部值的标准编程语言，
诸如 Pascal、Scheme 和 Java。

术语“左值”和“右值”，以及内存的环境-存储器模型源自 Strachey (1967)。

Fortran (Backus et al., 1957) 是第一种使用按址调用的语言，Algol 60 (Naur et al.,
1963) 是第一种使用按名调用的语言。Friedman & Wise (1976) 较早介绍了广泛使用懒求
值的威力。Haskell (Hudak et al., 1990) 是第一种使用按需调用的实际语言。为了建模
按名调用，Ingerman (1961) 发明了@emph{值箱} (@emph{thunk})。我们用它们和效果建模
按需调用。这与@emph{助记法} (@emph{memoization}) (Michie, 1968) 类似。

@emph{Monads}由 Moggi (1991) 提出，因 Wadler (1992) 流行。它提供了编程语言效果的
系统性模型。在函数式语言 Haskell (Peyton Jones, 2001) 中，monads 提供了非函数式
行为的组织原则。

续文由多人独立发现，Reynolds (1993) 介绍了这一迷人历史。Strachey & Wadsworth
(1974) 或许是其中影响最大的。Reynolds (1972) 将一个自循环解释器做了 CPS 变换，并
展示了这样做如何避免自循环的某些问题。将曳尾式程序翻译为命令式可追溯到 McCarthy
(1962)，Abelson & Sussman (1985, 1996) 强调了将其作为一种编程技术的重要性。

Plotkin (1975) 给出了相当清晰的 CPS 变换，发现了它的理论性质。Fischer (1972) 提
出了非常类似的变换。Wand (1980b) 首先探讨了续文和累加器之间的联系，像
@secref{s6.1}结尾的例子 @tt{fact} 那样。

在程序中直接使用续文的思想源自于 Landin (1965a) （另见 Landin 1965b），在 Lisp
和早期版本的 Scheme (Steele & Sussman, 1978) 中广泛使用。我们的 @tt{letcc} 基于
Scheme 的 @tt{call-with-current-continuation}，始见于 Clinger et al. (1985b)。

Wand (1980a) 展示了如何用续文建模轻量级进程或线程。续文用途广泛，超出本书讨论范
围，诸如@emph{协程} (@emph{coroutine}) (Haynes et al., 1986)。

我们对线程的讨论模拟了 POSIX 线程 （例如，参见 Lewis & Berg, 1998）。练习 5.56
基于 Erlang 的消息传递并发模型 (Armstrong, 2007)。

Steele 的 RABBIT 编译器 (Steele, 1978) 以 CPS 变换为基础。这一编译器先对源程序做
CPS 变换，然后以数据结构表示法表示续文。得出的程序像我们的寄存程序一样，很容易编
译。这条线发展出了 ORBIT 编译器 (Kranz et al., 1986) 和 Standard ML 的 New
Jersey 编译器 (Appel & Jim, 1989)。

@secref{cps}的 CPS 算法基于 Danvy & Nielsen (2003) 提出的一阶组合式算法。CPS 翻
译历史悠久，包括 Sabry & Wadler (1997)，他们改进了 Sabry & Felleisen (1993)，而
后者又是受本书初版第八章的 CPS 算法启发。练习 6.30 基于 Danvy & Filinski (1992)
提出的高阶组合式 CPS 算法。A-normal form （练习 6.34）是 CPS 的另一选项，由
Sabry & Felleisen (1992); Flanagan et al. (1993) 提出。

当前的大多数与有类型编程语言相关的工作都能追溯到 Milner (1978)，他在 ML 中引入了
类型，作为保证计算机生成证明可靠性的工具。Ullman (1998) 对此做了精辟的介绍。更多
讨论参见 Felleisen & Friedman (1996)，另见 Paulson (1996); Smith (2006)。

人们多次发现了类型推导。标准参考书是 Hindley (1969)，但 Hindley 提到，Curry 在
1950 年代已经知道了这些结论。Morris (1968) 也提出了类型推导，但在 Milner 1978 年
的论文发表之前，类型推导从未广泛应用。

Wand (1987) 率先阐明了如何将类型推导分为方程构建和求解。名为 Hindley-Milner 多态
的 Milner (1978) 系统与练习 7.28 中的系统基本相同。Pierce (2002, 2004) 的两卷著
作对类型做了百科全书式的讨论。

数据抽象思想是 1970 年代的一大创举，且广为讨论。这里我们仅仅提及 Parnas (1972)，
他强调了以接口作为信息隐藏边界的重要性。数据类型的实现是满足该类型定义的任意值和
操作的集合。Goguen et al. (1977) 证明，任意数据类型都能以记录值如何构建的树的集
合实现，且从一个树的集合到该数据类型另一实现的集合具有唯一映射。相对地，任意数据
类型都能以过程表示法实现，且从该数据类型的任何其他实现到过程表示法都有唯一映射
(Giarratana et al., 1976; Wand, 1979; Kamin, 1980)。

用类型强制数据抽象始见于 Reynolds (1975)，类型应用于 CLU (Liskov et al., 1977)。
这发展为 Standard ML (Milner et al., 1989) （另见 Paulson, 1996; Ullman, 1998）
的模块系统。我们的模块系统基于 Leroy (1994)，应用于 CAML（参见 Smith, 2006），
CAML 是 ML 的另一变体。

通常视 Simula 67 (Birtwistle et al., 1973) 为第一种面向对象语言。面向对象的类比
由 Smalltalk (Goldberg & Robson, 1983) 和 Actors (Hewitt, 1977) 扩充。二者都使用
人类交互以及收发消息的类比来解释他们的思想。Sussman 和 Steele 原打算用 Scheme 理
解 Hewitt 的 actor 模型，Scheme 远超其原意。Abelson & Sussman (1985, 1996) 和
Springer & Friedman (1989) 给出了更多用 Scheme 进行面向对象编程的例子，讨论了函
数式和命令式编程在哪些时候最合适。Steele (1990) 和 Kiczales et al. (1991) 描述了
Common Lisp 中强大的面向对象编程组件 CLOS。

@secref{oac}的语言基于Java的对象模型。Java 的标准参考书是 Arnold & Gosling
(1998)，而对于严肃的读者，Gosling et al. (1996)是其规范。

Ruby （参见 Thomas et al., 2005）、Python （van Rossum & Drake, 2006）和
Perl（Wall et al., 2000; Dominus, 2005）是具有对象和过程的无类型语言，大致类似我
们的CLASSES。C# 是一种有类型的语言，相较Java添加了很多特性，最著名的是与过程类似
的@emph{委托} (@emph{delegates})，它还允许程序员指定某些调用应该是尾调用。

Abadi & Cardelli (1996) 定义了一种简单的对象演算，成为面向对象系统中类型研究的有
效基础。Flatt et al. (1998) 形式化了Java的一个子集。另一有用的子集是
@emph{Featherweight Java} (Igarashi et al., 1999)。

Gamma et al. (1995) 在一本手册中，介绍了组织面向对象程序的有益原则。

ACM于 1978 年 (Wexelblatt, 1978)、1996年 (Bergin & Gibson, 1996) 和 2007 年
(Hailpern, 2007) 举办了三次会议，专门讨论编程语言的历史。这些会议囊括了讨论各种
编程语言历史的论文。IEEE Annals of the History of Computing囊括了介绍方方面面计
算历史的学术性文章，也包括编程语言。Knuth & Pardo (1977) 介绍了初期编程语言的迷
人历史。

不计其数的会议在汇报编程语言的新进展。至少就本书讨论的话题来说，三个顶级会议是
@emph{ACM Symposium on Principles of Programming Languages} (POPL)、@emph{ACM
SIGPLAN International Conference on Functional Programming} (ICFP) 和 @emph{ACM
SIGPLAN Conference on Programming Language Design and Implementation} (PLDI)。编
程语言的主要学术期刊包括@emph{ACM Transactions on Programming Languages and
Systems}、@emph{Journal of Functional Programming} 和@emph{Higher-Order and
Symbolic Computation}。除此之外，还有些网站专注于编程语言的方方面面。
