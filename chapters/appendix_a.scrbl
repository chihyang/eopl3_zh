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

@elemtag["readings"]{}

@title[#:style part-title-style-numbered #:tag "further-reading"]{扩展阅读}

这里的一些阅读材料教会，影响，或者启发了我们创作本书。希望你能像我们一样，至少喜
欢其中的一部分。

不熟悉递归编程和符号计算的读者可以看看 @emph{The Little Schemer}
(@author-ref{Friedman} & @author-ref{Felleisen}, 1996)，或 @emph{The Little
MLer} (@author-ref{Felleisen} & @author-ref{Friedman}, 1996)，或者有考据癖，看看
@emph{The Little LISPer} (@author-ref{Friedman}, 1974)。作为计算第一课，
@emph{How to Design Programs} (@author-ref{Felleisen} et al., 2001) 深入探讨了如
何递归编程。

@eopl-index[(eopl-index-entry "Bottom-up definition" "Bottomupdefinition")]
@eopl-index["Induction, proof by"]
@eopl-index["Inductive specifications"]
用归纳法定义集合和关系，是数理逻辑中久已存在的技术。我们的自底向上和推理规则式归
纳大致效仿 @author-ref{Plotkin} (1975, 1981) 的工作。我们的@exact-elem{“}自顶向下
@exact-elem{”}式归纳效仿另一种技术，名为@term["coinduction"]{余归纳}
@eopl-index["Coinduction"]
（参见@author-ref{Gordon}, 1995; @author-ref{Jacobs} & @author-ref{Rutten},
1997），@author-ref{Felleisen} et al. (2001) 也使用了这种技术。

@eopl-index{Context-free grammar}
@eopl-index["Grammars"]
上下文无关语法是语言学和计算机科学的标准工具。大多数编译器书籍，比如
@author-ref{Aho} et al. (2006)，都对语法和解析算法进行了大篇幅的讨论。
@eopl-index{Abstract syntax}
@eopl-index{Concrete syntax}
将具体语法和抽象语法分开的思想通常归功于@author-ref{McCarthy} (1962)。他强调用接
口抽象语法树。

@eopl-index["Follow the Grammar"]
我们的口号@emph{遵循语法}基于@emph{结构化归纳法}，由 @author-ref{Burstall}
(1969) 提出。即使过程没有@bold{遵循语法}，@term["subgoal induction"]{子目标归纳}
(@author-ref{Morris} & @author-ref{Wegbreit}, 1977) 仍是证明递归过程正确性的有效
方法。过程的可能输入受不变式约束时，子目标归纳也有效。
@eopl-index["Context-sensitive constraint"]
@eopl-index["Invariant"]

@eopl-index["Generalization"]
@term["generalization"]{泛化} 是源自数学的标准技术，常用来证明某个特定陈述是某个
更通用陈述的特例。我们把额外参数描述为上下文的抽象，是受到属性语法
(@author-ref{Knuth}, 1968)中的@emph{继承属性} 启发。

@eopl-index[(eopl-index-entry @elem{@tt{cases} form} "Casesform")]
@eopl-index[(eopl-index-entry @elem{@tt{define-datatype} form} "definedatatypeform")]
我们的构造器 @tt{define-datatype} 和 @tt{cases} 是受 ML 的 @tt{datatype} 和模式
匹配工具启发，详见 @author-ref{Milner} et al. (1989) 及其修订版
@author-ref{Milner} et al. (1997)。

Lambda 演算由邱奇发明 (@author-ref{Church}, 1941)，用于研究数理逻辑，但已成为诸
多现代编程语言理论的灵感来源。Lambda 演算的介绍参见 @author-ref{Hankin} (1994)、
@author-ref{Peyton Jones} (1987) 或 @author-ref{Stoy} (1977)。
@author-ref{Barendregt} (1981, 1991) 提供了百科全书式的参考。

@eopl-index["Contour diagrams"]
@figure-ref{fig-3.13} 那样的等深线用来解释词法作用域，首先由
@author-ref{Johnston} (1971) 提出。无名解释器和翻译器基于德布鲁金索引
(@author-ref{de Bruijn}, 1972)。
@eopl-index[(eopl-index-entry "de Bruijin indices" "Bruijinindices")]

Scheme 由 @author-ref{Sussman} & @author-ref{Steele} (1975) 发明。其开发过程记录
在 @author-ref{Steele} & @author-ref{Sussman} (1978); @author-ref{Clinger} et
al. (1985a); @author-ref{Rees} et al. (1986); @author-ref{Clinger} et
al. (1991); @author-ref{Kelsey} et al. (1998)。Scheme 标准由 IEEE standard
(IEEE, 1991) 和 @emph{@${\textit{Revised}^6} Report on the Algorithmic Language
Scheme} (@author-ref{Sperber} et al., 2007) 制定。

@author-ref{Dybvig} (2003) 简短介绍了 Scheme，加入了许多富有洞见的例子。

@eopl-index["Interpreter"]
解释器思想至少能追溯到图灵，他定义了能够模拟任何图灵机的@exact-elem{“}通用
@exact-elem{”}机器。这种通用机器实际上是一个解释器，取一套描述图灵机的编码，模
拟解码机器 (Turing, 1936)。经典的冯诺依曼机 (@author-ref{von Neumann}, 1945) 同
样是硬件实现的解释器，用来解释机器语言程序。

@eopl-index["Defined language"]
@eopl-index["Defining language"]
对解释器的现代应用可追溯到 @author-ref{McCarthy} (1960)，他提出了@term["metacircular
interpreter"]{自循环解释器}（用被定语言本身写就的解释器），用来解释 Lisp 的能力。
当然，这样的解释器带来一大难题：如果被定语言由自身定义，我们要理解语言的定义，就
要先理解这种语言。确实，即使解释器不是自循环的，也会面临同样的问题。读者理解被定
义的事物之前，仍需理解书写定义用的语言。

这些年来，大量技术用来解决这一难题。我们把解释器视为方程式规范的转写
@eopl-index["Equational specification"]
(@author-ref{Goguen} et al., 1977) 或者 @author-ref{Plotkin} (1975, 1981) 式的大
步操作语义。这只需要非常直观的数学。

@eopl-index["Denotational semantics"]
指称语义是另一种用数学定义语言的技术。这种方法将解释器替换为一个函数，该函数把每
个被定语言的程序翻译为定义其行为的数学对象。@author-ref{Plotkin} (1977) 对这种技
术做了无可替代的介绍，@author-ref{Winskel} (1993) 做了更宽泛的探讨。
@author-ref{Milne} & @author-ref{Strachey} (1976) 是本百科全书，研究了如何用这种
技术建模大量语言特性，

另一种方式是写出被定语言子集的解释器。例如，@secref{state}的几个解释器依靠
Scheme 的存储器来解释存储器的概念，但它们只用了一个全局可变对象，而不是 Scheme
可变变量的所有能力。

@eopl-index["EXPLICIT-REFS"]
@eopl-index["IMPLICIT-REFS"]
将计算视为操作存储器的思想可追溯到现代计算（参见 @author-ref{von Neumann}, 1945）
的开端。EXPLICIT-REFS 的设计基于 ML (@author-ref{Milner} et al., 1989) 的存储器
模型，而后者与 Bliss (@author-ref{Wulf}, 1971) 类似。IMPLICIT-REFS 的设计类似于
大多数具有可变局部值的标准编程语言，诸如 Pascal、Scheme 和 Java。

术语@exact-elem{“}左值@exact-elem{”}和@exact-elem{“}右值@exact-elem{”}，以及
内存的环境-存储器模型源自 @author-ref{Strachey} (1967)。

@eopl-index[(eopl-index-entry "Call-by-reference" "Callbyreference")]
Fortran (@author-ref{Backus} et al., 1957) 是第一种使用按指调用的语言，Algol 60
(@author-ref{Naur} et al., 1963) 是第一种使用按名调用的语言。
@eopl-index[(eopl-index-entry "Call-by-name" "Callbyname")]
@author-ref{Friedman} & @author-ref{Wise} (1976) 较早介绍了全面使用懒求值的威力。
@eopl-index[(eopl-index-entry "Call-by-need" "Callbyneed")]
Haskell (@author-ref{Hudak} et al., 1990) 是第一种使用按需调用的实际语言。为了建
模按名调用，@author-ref{Ingerman} (1961) 发明了@term["thunk"]{值箱}。我们用它们
和效果建模按需调用。这与@term["memoization"]{助记法} (@author-ref{Michie}, 1968) 类似。
@eopl-index["Effects, computational"]

@emph{Monads} 由 @author-ref{Moggi} (1991) 提出，因 @author-ref{Wadler} (1992)
流行。它提供了编程语言效果的通用模型。在函数式语言 Haskell (@author-ref{Peyton
Jones}, 2001) 中，monads 提供了非函数式行为的组织原则。

@eopl-index["Continuation-passing style" "transformation to"]
@eopl-index["Continuations"]
续文由多人独立发现，@author-ref{Reynolds} (1993) 介绍了这一迷人历史。
@author-ref{Strachey} & @author-ref{Wadsworth} (1974) 或许是其中影响最大的。
@author-ref{Reynolds} (1972) 将一个自循环解释器做了 CPS 变换，并展示了这样做如何
避免自循环的某些问题。将尾式程序翻译为命令式可追溯到 @author-ref{McCarthy}
(1962)，@author-ref{Abelson} & @author-ref{Sussman} (1985, 1996) 强调了将其作为
一种编程技术的重要性。
@eopl-index["Expressions" "tail form"]

@author-ref{Plotkin} (1975) 给出了相当清晰的 CPS 变换，发现了它的理论性质。
@author-ref{Fischer} (1972) 提出了非常类似的变换。@author-ref{Wand} (1980b) 率先
探讨了续文和@eopl-index{Accumulator}累加器之间的联系，像@secref{s6.1}结尾的例子
@tt{fact} 那样。

在程序中直接使用续文的思想源自于 @author-ref{Landin} (1965a) （另见
@author-ref{Landin} 1965b），在 Lisp 和早期版本的 Scheme (@author-ref{Steele} &
@author-ref{Sussman}, 1978) 中广泛使用。我们的 @tt{letcc} 基于 Scheme 的
@tt{call-with-current-continuation}，始见于 @author-ref{Clinger} et al. (1985b)。
@eopl-index[(eopl-index-entry @tt{call-with-current-continuation} "Callwithcurrentcontinuation")]

@author-ref{Wand} (1980a) 展示了如何用续文建模轻量级进程或线程。续文用途广泛，远
超本书讨论范围，如@term["coroutine"]{协程} (@author-ref{Haynes} et al., 1986)。

我们对线程的讨论模拟了 POSIX 线程 （例如，参见 @author-ref{Lewis} &
@author-ref{Berg}, 1998）。@exercise-ref{ex5.56} 基于 Erlang 的消息传递并发模型
(@author-ref{Armstrong}, 2007)。

@author-ref{Steele} 的 RABBIT 编译器 (@author-ref{Steele}, 1978) 以 CPS 变换为基
础。这一编译器首先对源程序做 CPS 变换，然后以数据结构表示续文。得出的程序像我们
的寄存程序一样，很容易编译。这条线发展出了 ORBIT 编译器 (@author-ref{Kranz} et
al., 1986) 和 Standard ML of New Jersey 编译器 (@author-ref{Appel} &
@author-ref{Jim}, 1989)。

@secref{cps}的 CPS 算法基于 @author-ref{Danvy} & @author-ref{Nielsen} (2003) 提
出的一阶组合式算法。CPS 翻译历史悠久，包括 @author-ref{Sabry} &
@author-ref{Wadler} (1997)，他们改进了 @author-ref{Sabry} &
@author-ref{Felleisen} (1993)，而后者又是受本书初版第 8 章的 CPS 算法启发。
@exercise-ref{ex6.30} 基于 @author-ref{Danvy} & @author-ref{Filinski} (1992) 提
出的高阶组合式 CPS 算法。CPS 之外还有@eopl-index[@eopl-index-entry["A-normal
form (ANF)" "Anormalform"]] A-normal form（@exercise-ref{ex6.34}），由
@author-ref{Sabry} & @author-ref{Felleisen} (1992); @author-ref{Flanagan} et
al. (1993) 提出。
@eopl-index["Continuation-passing style" "transformation to"]

当前大多数关于有类型编程语言的工作都能追溯到 @author-ref{Milner} (1978)，他在 ML
中引入了类型，作为保证计算机生成证明可靠性的工具。@author-ref{Ullman} (1998) 对
此做了精辟的介绍。更多讨论参见 @author-ref{Felleisen} & @author-ref{Friedman}
(1996)，另见 @author-ref{Paulson} (1996); @author-ref{Smith} (2006)。

人们多次发现了类型推导。标准参考书是 @author-ref{Hindley} (1969)，但
@author-ref{Hindley} 提到，@eopl-index{Curry, Haskell}Curry 在 1950 年代已经知道
了这些结论。@author-ref{Morris} (1968) 也提出了类型推导，但在@author-ref{Milner}
1978 年的论文发表之前，类型推导从未广泛应用。

@author-ref{Wand} (1987) 率先阐明了如何将类型推导分为方程构建和求解。名为
Hindley-Milner 多态的 @author-ref{Milner} (1978) 系统与@exercise-ref{ex7.28} 中
的系统基本相同。@author-ref{Pierce} (2002, 2004) 的两卷著作对类型做了百科全书式
的讨论。

广为论述的数据抽象思想是 1970 年代的一大创举。这里我们仅仅提及
@author-ref{Parnas} (1972)，他强调了以接口作为信息隐藏边界的重要性。数据类型的实
现是满足该类型定义的任意值和操作的集合。@author-ref{Goguen} et al. (1977) 证明，
任意数据类型都能以树的集合实现，树中记录了值如何构建，且从一个树的集合到该数据类
型另一实现的集合具有唯一映射。相对地，任意数据类型都能以过程表示法实现，且从该数
据类型的任何其他实现到过程表示法都有唯一映射 (@author-ref{Giarratana} et al.,
1976; @author-ref{Wand}, 1979; @author-ref{Kamin}, 1980)。
@eopl-index{Abstract data types (ADTs)}
@eopl-index{Abstraction boundary}
@eopl-index{Data abstraction}
@eopl-index["Implementation" "of ADT"]

用类型强制数据抽象始见于 @author-ref{Reynolds} (1975)，类型应用于 CLU
(@author-ref{Liskov} et al., 1977)。这发展为 Standard ML (@author-ref{Milner} et
al., 1989) （另见 @author-ref{Paulson}, 1996; @author-ref{Ullman}, 1998）的模块
系统。我们的模块系统基于 @author-ref{Leroy} (1994)，它应用于 CAML（参见
@author-ref{Smith}, 2006），而这是 ML 的另一变体。

通常视 Simula 67 (@author-ref{Birtwistle} et al., 1973) 为第一种面向对象语言。面
向对象的类比由 Smalltalk (@author-ref{Goldberg} & @author-ref{Robson}, 1983) 和
Actors (@author-ref{Hewitt}, 1977) 扩充。二者都使用人类互动以及收发消息的类比来
解释他们的思想。@author-ref{Sussman} 和 @author-ref{Steele} 原打算用 Scheme 理解
Hewitt 的 actor 模型，但 Scheme 超越其原意。@author-ref{Abelson} &
@author-ref{Sussman} (1985, 1996) 和 @author-ref{Springer} &
@author-ref{Friedman} (1989) 给出了更多用 Scheme 进行面向对象编程的例子，讨论了
函数式编程和命令式编程在哪些时候最适合。@author-ref{Steele} (1990) 和
@author-ref{Kiczales} et al. (1991) 描述了 Common Lisp 中强大的面向对象编程组件
CLOS。

@secref{oac}的语言基于 Java 的对象模型。Java 的标准参考书是 @author-ref{Arnold}
& @author-ref{Gosling} (1998)，有意精研的读者可以阅读其规范 @author-ref{Gosling}
et al. (1996)。

Ruby （参见 @author-ref{Thomas} et al., 2005）、Python （@author-ref{van Rossum}
& @author-ref{Drake}, 2006）和 Perl（@author-ref{Wall} et al., 2000;
@author-ref{Dominus}, 2005）是具有对象和过程的无类型语言，大致类似我们的 CLASSES。
C# 是一种带类型的语言，相较 Java 添加了很多特性，最著名的是与过程类似
的@term["delegates"]{委托}，它还允许程序员指定某些调用应该是尾调用。
@eopl-index["Delegates"]

@author-ref{Abadi} & @author-ref{Cardelli} (1996) 定义了一种简单的对象演算，为面
向对象系统中的类型研究奠定了基础。@author-ref{Flatt} et al. (1998) 形式化了 Java
的一个子集。另一有用的子集是 @emph{Featherweight Java} (Igarashi et al., 1999)。

@author-ref{Gamma} et al. (1995) 编写了一本备受关注的手册，专论编写面向对象程序
的有效组织原则。

ACM 于 1978 年 (@author-ref{Wexelblatt}, 1978)、1996 年 (@author-ref{Bergin} &
@author-ref{Gibson}, 1996) 和 2007 年(@author-ref{Hailpern}, 2007) 三次举办会议，
专门讨论编程语言的历史。这些会议囊括了讨论各种编程语言历史的论文。IEEE Annals of
the History of Computing 囊括了介绍方方面面计算历史的学术性文章，也包括编程语言。
@author-ref{Knuth} & @author-ref{Pardo} (1977) 介绍了初期编程语言的迷人历史。

不计其数的会议在汇报编程语言的新进展。至少就本书讨论的话题来说，三个顶级会议是
@emph{ACM Symposium on Principles of Programming Languages} (POPL)、@emph{ACM
SIGPLAN International Conference on Functional Programming} (ICFP) 和 @emph{ACM
SIGPLAN Conference on Programming Language Design and Implementation} (PLDI)。编
程语言的主要学术期刊包括 @emph{ACM Transactions on Programming Languages and
Systems}、@emph{Journal of Functional Programming} 和 @emph{Higher-Order and
Symbolic Computation}。除此之外，还有些网站专注于编程语言的方方面面。
