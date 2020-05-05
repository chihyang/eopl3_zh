#lang scribble/book
@(require "style.rkt"
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

@; TODO
长久以来，数理逻辑都用归纳法定义集合和关系。

@; TODO: two pages

指称语义是根据数学定义语言的另一技术。这种方法将解释器替换为一个函数，该函数把每
个待定语言的程序翻译为定义该程序行为的数学对象。@; TODO

另一种方式是写出待定语言子集的解释器。例如，@secref{state}的解释器依靠 Scheme 的
存储器来解释存储器的概念，但它们只用了一个全局可变对象，而不是 Scheme 可变变量的
所有能力。

将计算视为操作存储器的思想可追溯到现代计算（参见 von Neumann, 1945）的开端。
EXPLICIT-REFS 的设计基于 ML (Milner et al., 1989) 的存储器模型，而它与 Bliss
(Wulf, 1971) 类似。IMPLICIT-REFS 的设计近似与大多数具有可变局部值的标准编程语言，
诸如 Pascal、Scheme 和 Java。

术语“左值”和“右值”，以及内存的环境-存储器模型来自于 Strachey (1967)。

Fortran (Backus et al., 1957) 是第一种使用按址调用的语言，Algol 60 (Naur et al.,
1963) 是第一种使用按名调用的语言。Friedman & Wise (1976) 较早介绍了广泛使用懒求
值的威力。Haskell (Hudak et al., 1990) 是第一种使用按需调用的实际语言。为了建模
按名调用，Ingerman (1961) 发明了 @emph{值箱} (@emph{thunk})。我们用它们和效果建
模按需调用。这与 @emph{助记法} (@emph{memoization}) (Michie, 1968) 类似。

@emph{Monads}，由 Moggi (1991) 提出，因 Wadler (1992) 而流行。它提供了编程语言效
果的系统性模型。在函数式语言 Haskell (Peyton Jones, 2001) 中，monads 提供了非函
数式行为的组织原则。

续文由多人独立发现，Reynolds (1993) 介绍了这一迷人历史。Strachey & Wadsworth
(1974) 或许是其中影响最大的。Reynolds (1972) 将一个自循环解释器做了 CPS 变换，并
展示了这样做如何避免自循环的某些问题。将曳尾式程序翻译为命令式可追溯到 McCarthy
(1962)，Abelson & Sussman (1985, 1996) 强调了将其作为一种编程技术的重要性。

Plotkin (1975) 给出了相当清晰的 CPS 变换，发现了它的理论性质。Fischer (1972) 提
出了非常类似的变换。Wand (1980b) 首先探讨了续文和累加器之间的联系，像
@secref{s6.1} 结尾的例子 @tt{fact} 那样。

在程序中直接使用续文的思想源自于 Landin (1965a) （另见 Landin 1965b），在 Lisp
和早期版本的 Scheme (Steele & Sussman, 1978) 中广泛使用。我们的 @tt{letcc} 基于
Scheme 的 @tt{call-with-current-continuation}，始见于 Clinger et al. (1985b)。

Wand (1980a) 展示了如何用续文建模轻量级进程或线程。续文用途广泛，超出本书讨论范
围，诸如@emph{协程} (@emph{coroutine}) (Haynes et al., 1986)。

我们对线程的讨论模拟了 POSIX 线程 （例如，参见 Lewis & Berg, 1998）。练习 5.56
基于 Erlang 的消息传递并发模型 (Armstrong, 2007)。

Steele 的 RABBIT 编译器 (Steele, 1978) 以 CPS 变换为编译器的基础。这一编译器先对
源程序做 CPS 变换，然后以数据结构表示法表示续文。得出的程序像我们的寄存程序一样，
很容易编译。这条线发展出了 ORBIT 编译器 (Kranz et al., 1986) 和 Standard ML 的
New Jersey 编译器 (Appel & Jim, 1989)。

@secref{cps}的 CPS 算法基于 Danvy & Nielsen (2003) 提出的一阶组合式算法。CPS 翻
译历史悠久，包括 Sabry & Wadler (1997)，他们改进了 Sabry & Felleisen (1993)，而
后者又是受本书初版第八章的 CPS 算法的启发。练习 6.30 基于 Danvy & Filinski
(1992) 提出的高阶组合式 CPS 算法。A-normal form （练习 6.34）是 CPS 的另一选项，
由 Sabry & Felleisen (1992); Flanagan et al. (1993) 提出。

当前大多数与有类型编程语言相关的工作都能追溯到 Milner (1978)，他在 ML 中引入了类
型，作为保证计算机生成证明可靠性的工具。Ullman (1998) 对此做了精辟的介绍。更多讨
论参见 Felleisen & Friedman (1996)，另见 Paulson (1996); Smith (2006)。

人们多次发现了类型推导。标准参考书是 Hindley (1969)，但 Hindley 提到，Curry 在
1950 年代已经知道了这些结论。Morris (1968) 也提出了类型推导，但在 Milner 1978 年
的论文发表之前，类型推导从未广泛应用。

Wand (1987) 率先阐明了如何将类型推导分为方程构建和求解。名为 Hindley-Milner 多态
的 Milner (1978) 系统与练习 7.28 中的系统基本相同。Pierce (2002, 2004) 的两卷著
作对类型做了百科全书式的讨论。

数据抽象思想是 1970 年代的一大创举，且广为讨论。这里我们仅仅提及 Parnas (1972)，
他强调了以接口作为隐藏信息的边界的重要性。数据类型的实现是满足该类型定义的任意值
和操作的集合。Goguen et al. (1977) 证明任意数据类型都能以记录值如何构建的树的集
合实现，且从一个树的集合到该数据类型另一实现的集合具有唯一映射。相反，任意数据类
型都能以过程表示法实现，且从该数据类型的任何其他实现到过程表示法都有唯一映射
(Giarratana et al., 1976; Wand, 1979; Kamin, 1980)。

用类型强制数据抽象始见于 Reynolds (1975)，类型应用于 CLU (Liskov et al., 1977)。
这发展为 Standard ML (Milner et al., 1989) （另见 Paulson, 1996; Ullman, 1998）
的模块系统。我们的模块系统基于 Leroy (1994)，应用于 CAML (参见 Smith, 2006)，
CAML 是 ML 的另一变体。

通常视 Simula 67 (Birtwistle et al., 1973) 为第一种面向对象语言。面向对象的类比
由 Smalltalk (Goldberg & Robson, 1983) 和 Actors (Hewitt, 1977) 扩充。二者都使用
人类交互以及收发消息的类比来解释他们的思想。Scheme 超越了 Sussman 和 Steele 试图
理解 Hewitt 的 actor 模型的原意。Abelson & Sussman (1985, 1996) 和 Springer &
Friedman (1989) 给出了更多用 Scheme 面向对象编程的例子，讨论了函数式和命令式编程
在哪些时候最合适。Steele (1990) 和 Kiczales et al. (1991) 描述了 CLOS，Common
Lisp 中的强大面向对象编程组件。

@secref{oac}的语言基于Java的对象模型。标准参考书是 Arnold & Gosling (1998)，但对
于严肃的读者，Gosling et al. (1996)是其规范。

Ruby （参见 Thomas et al., 2005）、Python （van Rossum & Drake, 2006）和
Perl（Wall et al., 2000; Dominus, 2005）是具有对象和过程的无类型语言，大致类似我
们的CLASSES。C# 是一种有类型的语言，相较Java添加了很多特性，最著名的是与过程类似
的 @emph{委托} (@emph{delegates})，它还允许程序员指定某些调用应该是尾调用。

Abadi & Cardelli (1996) 定义了一种简单的对象演算，成为了面向对象系统中类型研究的
有益基础。Flatt et al. (1998) 形式化了Java的一个子集。另一有用的子集是
@emph{Featherweight Java} (Igarashi et al., 1999)。

Gamma et al. () 在一本手册中，介绍了组织面向对象程序的有益原则。

ACM于1978年 ()、1996年 ()和2007 ()年举办了三次会议，专门讨论编程语言的历史。这些
会议囊括了讨论各种编程语言历史的论文。IEEE Annals of the History of Computing囊
括了介绍方方面面计算历史的学术性文章，也包括编程语言。Knuth & Pardo () 介绍了初
期编程语言的迷人历史。

不计其数的会议在汇报编程语言的新发展。至少就本书讨论的话题来说，三个顶级会议是
@emph{ACM Symposium on Principles of Programming Languages} (POPL)、@emph{ACM
SIGPLAN International Conference on Functional Programming} (ICFP)、以及
@emph{ACM SIGPLAN Conference on Programming Language Design and Implementation}
(PLDI)。编程语言的主要学术期刊包括@emph{ACM Transactions on Programming
Languages and Systems}、@emph{Journal of Functional Programming}、以及
@emph{Higher-Order and Symbolic Computation}。除此之外，有些网站专注于编程语言的
方方面面。
