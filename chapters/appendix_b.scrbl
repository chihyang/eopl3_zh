#lang scribble/book
@(require "style.rkt"
          latex-utils/scribble/math
          latex-utils/scribble/utils
          scribble/manual
          scribble-math
          scribble/example
          scribble/core
          scribble/example
          scriblib/footnote
          racket/sandbox)

@elemtag["sllgen"]{}

@title[#:style part-title-style-numbered #:tag "sllgen-parsing-system"]{SLLGEN解析系统}

程序只是字符串。要处理程序，需要将这些字符归类为有意义的单元。这种归类通常分为两
个阶段：@term[#:full #f "scanning"]{扫描} 和@term[#:full #f "parsing"]{解析}。

扫描过程将字符序列分为单词，标点等等。这些单元叫做@term[#f]{词条}、@term[#f]{词
素}，或者最常见的@term[#f]{词牌}。解析过程将词牌序列组织成有层次的语法结构，如表
达式、语句和块。这就像用从句组织句子。

SLLGEN 是一个 Scheme 包，用来生成解析器和扫描器。在本附录中，我们首先介绍扫描和
解析的基础，然后考虑如何用 SLLGEN 实现这些功能。

@section[#:style section-title-style-numbered #:tag "B.1"]{扫描}

扫描问题如@figure-ref{fig-B.1} 所示。我们在其中展示了一小段程序，以及应如何将其
分割为基本单元。

字符流应如何分割为词条是语言规范的一部分。语言的这部分规范有时称为@term["lexical
specification"]{词法规范}。典型的词法规范可能包括：

@itemlist[

 @item{任何空格和换行序列都等价于单个空格。}

 @item{注释以 @tt{%} 开头，到行尾截止。}

 @item{标识符是以字母开头的字母和数字序列。}

]

@eopl-figure{
@(image "../images/task-of-scanner"
  #:suffixes (list ".pdf" ".svg")
  "扫描器的任务")

@eopl-caption["fig-B.1"]{扫描器的任务}
}

扫描器的任务是遍历和分析输入，产生含有这些词条的数据结构。通常的语言中，扫描器可
能是一个过程，在调用时，由输入产生@exact-elem{“}下一个@exact-elem{”}词牌。

可以从头写出一个扫描器，但那又麻烦，又易错。更好的方式是写出指定语言的词法规范。
这一任务最常用的语言是@term["regular expressions"]{正则表达式}。正则表达式语言定
义如下：

@$${\mathit{R} ::= \mathit{Character} \mid \mathit{RR} \mid \mathit{R} \cup \mathit{R} \mid \neg\mathit{Character}}

每个正则表达式匹配一些字符串。我们可以用归纳法定义每个正则表达式匹配的字符串集合。

@itemlist[

 @item{匹配字符 @${c} 的字符串只含字符 @${c}。}

 @item{匹配 @${\neg c} 的字符串只含一个 @${c} 之外的字符。}

 @item{匹配 @${\mathit{RS}} 的字符串由匹配 @${\mathit{R}} 和匹配 @${\mathit{S}}
 的字符串相接而得。这叫做@term["concatenation"]{串联}。@eopl-index["Concatenation"]}

 @item{匹配 @${\mathit{R} \cup \mathit{S}} 的字符串匹配 @${\mathit{R}} 或
 @${\mathit{S}}。这有时写作 @${\mathit{R} \mid \mathit{S}}，
 叫做@term["alternation"]{并联}。@eopl-index{Alternation}}

 @item{匹配 @${\mathit{R}^{*}} 的字符串由 @${n} (@${n \geq 0}) 个匹配
 @${\mathit{R}} 的字符串串联而得。这叫做 @${\mathit{R}} 的@term["Kleene
 closure"]{克莱尼闭包}。}

]

看些例子更有帮助：

@itemlist[

 @item{@${ab} 只匹配字符串 @tt{ab}。}

 @item{@${ab \cup cd} 匹配字符串 @tt{ab} 或 @tt{cd}。}

 @item{@${(ab \cup cd)(ab \cup cd \cup ef)} 匹配字符串 @tt{abab}、@tt{abcd}、
 @tt{abef}、@tt{cdab}、@tt{cdcd} 和 @tt{cdef}。}

 @item{@${(ab)^{*}} 匹配空字符串、@tt{ab}、@tt{abab}、@tt{ababab}、@tt{abababab}、
 @tt{...}。}

 @item{@${(ab \cup cd)^{*}} 匹配空字符串、@tt{ab}、@tt{cd}、@tt{abab}、@tt{abcd}、
 @tt{cdab}、@tt{cdcd}、@tt{ababab}、@tt{...cdcdcd}、@tt{...}。}

]

上面的例子解释了不同操作的优先级，所以，@${{ab}^{*} \cup cd} 表示 @${(a(b^{*}))
\cup (cd)}。

我们例子中的规范可用正则表达式写作

@eopl-code{
@verbatim|{
|@${whitespace = (space \cup newline)(space \cup newline)^{*}}
|@${comment = @tt{%}(\neg newline)^{*}}
|@${identifier = letter(letter \cup digit)^{*}}
}|
}

扫描器用正则表达式获取词牌时，规则总是取@emph{最长}匹配。所以 @tt{xyz} 扫描为一
个标识符，而非三个。

扫描器找到一个词牌时，它返回的数据结构至少包含下列数据：

@itemlist[

 @item{一个@term["class"]{类别}，描述词牌的种类。类别的集合是词法规范的一部分。
 SLLGEN 使用 Scheme 符号区分这些类别；其他语法分析器可能使用其他数据结构。}

 @item{一段数据，描述特定词牌。这段数据的性质也是词法规范的一部分。在我们的系统
 中，数据如下：标识符的数据是由词牌字符串产生的 Scheme 符号；数字的数据是由数字
 字面值描述的数值；字符串字面值的数据就是字符串。字符串数据用作关键字和标点。在
 没有符号的实现语言中，可以改用字符串（标识符的名字），或者以标识符为索引的哈希
 表（@term["symbol table"]{符号表}）条目。}

 @item{一段数据，描述该词牌在输入中的位置。解析器用这一信息帮助程序员定位语法错
 误。}

]

通常，词牌的内部结构只与扫描器和解析器相关，所以我们不再详加介绍。

@section[#:style section-title-style-numbered #:tag "B.2"]{解析}

解析过程将词牌序列组织成有层次的语法结构，如表达式，语句和块。这就像用从句组织句
子。语言的语法结构通常由 BNF 定义，也叫做@term["context-free grammar"]{上下文无
关语法}（@secref{s1.1.2}）。
@eopl-index["Context-free grammar"]
@eopl-index["Grammars"]

@eopl-index["Abstract syntax tree"]解析器输入为词牌序列，输出为一棵抽象语法树
（@secref{s2.5}）。SLLGEN 生成的抽象语法树可用 @tt{define-datatype} 描述。对给定
的语法，每个非终止符都对应一个数据类型。以每个非终止符为左边内容的生成式都对应一
个变体。式子右边出现的每个非终止符、标识符和数字都对应变体中的一个字段。
@secref{s2.5}有一个简单示例。当语法中有多个非终止符时，可以考虑
@exercise-ref{ex4.22} 中的语法。

@nested[#:style small]{
@envalign*{
      \mathit{Statement} &::= @tt{{ @m{\mathit{Statement}} ; @m{\mathit{Statement}} }} \\[-3pt]
                         &::= @tt{while @m{\mathit{Expression}} do @m{\mathit{Statement}}} \\[-3pt]
                         &::= @tt{@m{\mathit{Identifier}} := @m{\mathit{Expression}}} \\[-3pt]
     \mathit{Expression} &::= \mathit{Identifier} \\[-3pt]
                         &::= @tt{(@m{\mathit{Expression}} - @m{\mathit{Expression}})}
                         }}

这个语法产生的树由如下数据类型描述：

@nested{
@eopl-code{
@racketblock[
(define-datatype statement statement?
  (compound-statement
    (stmt1 statement?)
    (stmt2 statement?))
  (while-statement
    (test expression?)
    (body statement?))
  (assign-statement
    (lhs symbol?)
    (rhs expression?)))

(define-datatype expression expression?
  (var-exp
    (var symbol?))
  (diff-exp
    (exp1 expression?)
    (exp2 expression?)))
]}

式子右边的每个非终止符对应的树作为一个字段；标识符对应的符号作为一个字段。变体名
字在用 SLLGEN 写语法时指定。字段名是自动生成的；这里，我们给字段起了一些便于记忆
的名字。例如，输入

@eopl-code{@verbatim|{{x := foo; while x do x := (x - bar)}}|}

产生输出

@eopl-code{
@racketblock[
#(struct:compound-statement
   #(struct:assign-statement x #(struct:var-exp foo))
   #(struct:while-statement
      #(struct:var-exp x)
      #(struct:assign-statement x
         #(struct:diff-exp
            #(struct:var-exp x)
            #(struct:var-exp bar)))))
]}

}

@section[#:style section-title-style-numbered #:tag "B.3"]{SLLGEN 中的扫描器和解析器}

@subsection[#:style section-title-style-unumbered #:tag "B.3-scanners"]{定义扫描器}

在 SLLGEN 中，扫描器用正则表达式定义。我们的例子用 SLLGEN，要写成下面这样：

@eopl-code{
@racketblock[
(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))
]}

如果扫描器要和处理关键字或标点（如 @tt{while} 或 @tt{=}）的解析器共用，不需要手
动将这些放入扫描器中；解析器生成器会自动添加它们。

SLLGEN 中的扫描器定义是满足如下语法的列表：

@nested[#:style small]{
@envalign*{
            \mathit{Scanner\mbox{-}spec} &::= @tt{(@m{\{\mathit{Regexp\mbox{-}and\mbox{-}action}\}^{*}})} \\[-3pt]
\mathit{Regexp\mbox{-}and\mbox{-}action} &::= @tt{(@m{\mathit{Name}} (@m{\{\mathit{Regexp}\}^{*}}) @m{\mathit{Action}})} \\[-3pt]
                           \mathit{Name} &::= \mathit{Symbol} \\[-3pt]
                         \mathit{Regexp} &::= \mathit{String} \mid @tt{letter} \mid @tt{digit} \mid @tt{whitespace} \mid @tt{any} \\[-3pt]
                                         &::= @tt{(not @m{\mathit{Character}})} \mid @tt{(or @m{\{\mathit{Regexp}\}^{*}})} \\[-3pt]
                                         &::= @tt{(arbno @m{\mathit{Regexp}})} \mid @tt{(concat @m{\{\mathit{Regexp}\}^{*}})} \\[-3pt]
                         \mathit{Action} &::= @tt{skip} \mid @tt{symbol} \mid @tt{number} \mid @tt{string}
                         }}

列表中的每一项都定义了一个正则表达式，定义包含名字、一系列正则表达式，以及匹配成
功时的动作。名字是一个 Scheme 符号，表示词牌的类别。

由于扫描器中的顶层@term["regexp"]{正则表达式} 几乎总是串联而得，定义的第二部分是
一系列正则表达式。正则表达式可以是一个字符串；可以是预先定义的四个测试器之一：
@tt{letter}（匹配任何字母），@tt{digit}（匹配任何数字），@tt{whitespace}（匹配任
何 Scheme 空白字符），以及 @tt{any}（匹配任意字符）；可以是一个去反字符；也可以
是组合而得的正则表达式，采用 Scheme 式的语法，以 @tt{or} 表示并联，以
@tt{concat} 表示串联，以 @tt{arbno} 表示克莱尼星号。

扫描器工作时，把字符收集到一个缓存中。当扫描器断定找出了定义中所有正则表达式的最
长匹配时，它执行对应正则表达式的@emph{动作}。

动作为下列之一：

@itemlist[

 @item{符号 @tt{skip}。这表示词牌结束，但不产生任何词牌。扫描器继续处理字符串，
 找出下一个词牌。这一动作用于空白字符和注释。}

 @item{符号 @tt{symbol}。缓存中的字符转换为一个 Scheme 符号，并产生一个词牌，以
 指定类别名为其类别，以对应符号为其数据。}

 @item{符号 @tt{number}。缓存中的字符转换为一个 Scheme 数值，并产生一个词牌，以
 指定类别名为其类别，以对应数值为其数据。}

 @item{符号 @tt{string}。缓存中的字符转换为一个 Scheme 字符串，并产生一个词牌，
 以指定类别名为其类别，以对应字符串为其数据。}

]

如果两个正则表达式同时为最长匹配，@tt{string} 优先于 @tt{symbol}。这条规则意味着
关键字会按关键字处理，而非标识符。

@subsection[#:style section-title-style-unumbered #:tag "B.3-grammars"]{定义语法}

SLLGEN 还包含一种定义语法的语言。上面的简单语法用 SLLGEN 写作

@eopl-code{
@racketblock[
(define grammar-a1
  '((statement
      ("{" statement ";" statement "}")
      compound-statement)
    (statement
      ("while" expression "do" statement)
      while-statement)
    (statement
      (identifier ":=" expression)
      assign-statement)
    (expression
      (identifier)
      var-exp)
    (expression
      ("(" expression "-" expression ")")
      diff-exp)))
]}

SLLGEN 中的语法是由下列语法描述的列表：

@nested[#:style small]{
@envalign*{
            \mathit{Grammar} &::= @tt{(@m{\{\mathit{Production}\}^{*}})} \\[-3pt]
         \mathit{Production} &::= @tt{(@m{\mathit{Lhs}} (@m{\{\mathit{Rhs\mbox{-}item}\}^{*}}) @m{\mathit{Prod\mbox{-}name}})} \\[-3pt]
               \mathit{Lhs} &::= \mathit{Symbol} \\[-3pt]
   \mathit{Rhs\mbox{-}item} &::= \mathit{Symbol} \mid \mathit{String} \\[-3pt]
                            &::= @tt{(arbno @m{\mathit{\{Rhs\mbox{-}item\}^{*}}})} \\[-3pt]
                            &::= @tt{(separated-list @m{\mathit{\{Rhs\mbox{-}item\}^{*}}} @m{\mathit{String}})} \\[-3pt]
  \mathit{Prod\mbox{-}name} &::= \mathit{Symbol}
                         }}

语法是生成式列表。第一个生成式的左边是语法的起始符号。每个生成式包含左边（一个非
终止符号）、右边（@${rhs\mbox{-}item} 的列表），以及生成式名字。生成式的右边是符
号或者字符串列表。符号是非终止符；字符串是字符串字面值。式子右边也可以包含
@tt{arbno} 或 @tt{separated-list}；这些留待下面讨论。生成式的名字是一个符号，成
为 @tt{define-datatype} 中对应生成式的变体名。

在 SLLGEN 中，解析器必须在仅获知以下内容的条件下，通过语法断定生成式：(1) 正在寻
找哪一非终止符，(2) 正在解析的字符串中的首个符号（词牌）。这种形式的语法叫做
@${LL(1)} 语法；SLLGEN 表示 Scheme @${LL(1)} 解析器@bold{生成}器（Scheme
@${LL(1)} parser GENerator）。在实践中，这有些过于严格了，但足以应付本书需要。如
果输入语法不满足这一条件，SLLGEN 会给出一条警告。

@subsection[#:style section-title-style-unumbered #:tag "B.3-operations"]{SLLGEN的操作}

SLLGEN 包含几个过程，将扫描器和语法结合起来，形成可以执行的解析器。
@figure-ref{fig-B.2} 展示了用 SLLGEN 定义语言扫描器和解析器的例子。

过程 @tt{sllgen:make-define-datatypes} 负责为语法的每个生成式产生一个
@tt{define-datatype} 表达式，供 @tt{cases} 使用。过程
@tt{sllgen:list-define-datatypes} 也生成 @tt{define-datatype} 表达式，但是会以列
表形式返回，而非执行它们。由这些过程生成的字段名不够直观，因为语法中没有这些信息；
要得到更好的字段名，需要写出 @tt{define-datatype}。

过程 @tt{sllgen:make-string-scanner} 取一扫描器和一语法，生成一个扫描过程。得出
的过程可以处理一个字符串，得到一个词牌列表。语法用来给得到的扫描过程添加关键字。
这一过程主要用于调试。

过程 @tt{sllgen:make-stream-parser} 生成一个解析器。解析器是一过程，它取一字符串，
用扫描器扫描它，用语法解析它，然后返回一棵抽象语法树。像
@tt{sllgen:make-string-scanner} 一样，语法中的字符串字面值包含在扫描器中。

SLLGEN 也可以用来生成读入-求值-打印循环（@secref{s3.1}）。过程
@tt{sllgen:make-stream-parser} 与字符串版本类似，但是它的输入是字符流，输出是词
牌流。过程 @tt{sllgen:make-rep-loop} 取一字符串，一个单参数过程，一个流式解析器，
生成一个读入-求值-打印循环，以指定字符串为标准输出中的提示符，从标准输入读入字符，
解析它们，然后以指定过程处理抽象语法树，将结果打印出来。例如：

@eopl-figure[#:position "!t"]{
@racketblock[
(define scanner-spec-1 ...)

(define grammar-1 ...)

(sllgen:make-define-datatypes scanner-spec-1 grammar-1)

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec-1 grammar-1)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec-1 grammar-1))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-1 grammar-1))

(define read-eval-print
  (sllgen:make-rep-loop "--> " value-of--program
    (sllgen:make-stream-parser scanner-spec-1 grammar-1)))
]

@eopl-caption["fig-B.2"]{使用 SLLGEN}
}

@eopl-code{
@verbatim|{
> (define read-eval-print
    (sllgen:make-rep-loop "--> " eval-program
      (sllgen:make-stream-parser
        scanner-spec-3-1
        grammar-3-1)))
> (read-eval-print)
--> 5
5
--> add1(2)
3
--> +(add1(2),-(6,4))
5
}|
}

控制流程从这一循环返回 Scheme 读入-求值-打印循环的方式由系统决定。

@subsection[#:style section-title-style-unumbered #:tag "B.3-arbno"]{@tt{arbno}
和 @tt{separated-list} 模板关键字}

@tt{arbno} 关键字是语法中的克莱尼星号：它匹配重复任意次数的条目。例如，生成式

@nested{

@nested[#:style small]{@$${\mathit{statement} ::= @tt{{ @${\{statement @tt{;}\}^{*}} }}}}

在 SLLGEN 中可写作

@eopl-code{
@racketblock[
(define grammar-a2
  '((statement
      ("{" (arbno statement ";") "}")
      compound-statement)
     ...))
]}

这匹配一条复合语句，由任意数量分号分隔的语句序列组成。

}

@tt{arbno} 在抽象语法树中对应单个字段。该字段包含一个@emph{列表}，由 @tt{arbno}
内的非终止符数据组成。我们的例子生成如下数据类型：

@eopl-code{
@racketblock[
(define-datatype statement statement?
  (compound-statement
    (compound-statement32 (list-of statement?)))
  ...)
]}

简单交互为：

@eopl-code{
@verbatim|{
> (define scan&parse2
    (sllgen:make-string-parser scanner-spec-a grammar-a2))

> (scan&parse2 "{x := foo; y := bar; z := uu;}")
(compound-statement
  ((assign-statement x (var-exp foo))
   (assign-statement y (var-exp bar))
   (assign-statement z (var-exp uu))))
}|
}

我们可以把非终止符序列放入 @tt{arbno} 中。这时，节点中会有多个字段，每个对应一个
非终止符；每个字段包含一个语法树列表。例如：

@eopl-code{
@racketblock[
(define grammar-a3
  '((expression (identifier) var-exp)
     (expression
       ("let" (arbno identifier "=" expression) "in" expression)
       let-exp)))

(define scan&parse3
  (sllgen:make-string-parser scanner-spec-a grammar-a3))
]}

生成数据类型

@eopl-code{
@racketblock[
(define-datatype expression expression?
  (var-exp (var-exp4 symbol?))
  (let-exp
    (let-exp9 (list-of symbol?))
    (let-exp7 (list-of expression?))
    (let-exp8 expression?)))
]}

这里是运用该语法的例子：

@nested{
@eopl-code{
@verbatim|{
> (scan&parse3 "let x = y u = v in z")
(let-exp
  (x u)
  ((var-exp y) (var-exp v))
  (var-exp z))
}|
}

定义 @tt{(arbno identifier "=" expression)} 生成两个列表：标识符列表和表达式列表。
这很方便，因为我们的解释器能直接从中取出一部分表达式。

}

对某些语言的语法，在列表中只用分隔符，而不用结束符会更方便。这十分常见，因此
SLLGEN 内置这种操作。我们可以写

@eopl-code{
@racketblock[
(define grammar-a4
  '((statement
      ("{" (separated-list statement ";") "}")
      compound-statement)
     ...))
]}

它生成数据类型

@eopl-code{
@racketblock[
(define-datatype statement statement?
  (compound-statement
    (compound-statement103 (list-of statement?)))
  ...)
]}

这是简单交互的例子：

@nested{
@eopl-code{
@verbatim|{
> (define scan&parse4
    (sllgen:make-string-parser scanner-spec-a grammar-a4))
> (scan&parse4 "{}")
(compound-statement ())
> (scan&parse4 "{x:= y; u := v ; z := t}")
(compound-statement
  ((assign-statement x (var-exp y))
   (assign-statement u (var-exp v))
   (assign-statement z (var-exp t))))
> (scan&parse4 "{x:= y; u := v ; z := t ;}")
Error in parsing: at line 1
Nonterminal <seplist3> can’t begin with string "}"
}|
}

在本例中，输入字符串结尾有一个分号，与语法不符，所以报错。

}

类似于 @tt{arbno}，我们可以在 @tt{separated-list} 关键字中放置任意非终止符序列。
这时，节点中会有多个字段，每个对应一个非终止符；每个字段包含一个语法树列表。这和
@tt{arbno} 生成的数据完全相同；不同的只是具体语法。

我们偶尔会嵌套 @tt{arbno} 和 @tt{separated-list}。@tt{arbno} 内的非终止符生成一
个列表，所以 @tt{arbno} 内的 @tt{arbno} 内的非终止符生成列表的列表。

举个例子，考虑与 @tt{grammar-a4} 类似的 @tt{compound-statement}，但它支持多赋值：

@eopl-code{
@racketblock[
(define grammar-a5
  '((statement
      ("{"
        (separated-list
          (separated-list identifier ",")
          ":="
          (separated-list expression ",")
          ";")
        "}")
      compound-statement)
     (expression (number) lit-exp)
     (expression (identifier) var-exp)))
]}

@eopl-code{
@verbatim|{
> (define scan&parse5
    (sllgen:make-string-parser scanner-spec-a grammar-a5))
}|
}

它为 @tt{statement} 生成如下数据类型：

@eopl-code{
@racketblock[
(define-datatype statement statement?
  (compound-statement
    (compound-statement4 (list-of (list-of symbol?)))
    (compound-statement3 (list-of (list-of expression?)))))
]}

一般的交互如下：

@eopl-code{
@verbatim|{
> (scan&parse5 "{x,y := u,v ; z := 4; t1, t2 := 5, 6}")
(compound-statement
  ((x y) (z) (t1 t2))
  (((var-exp u) (var-exp v))
    ((lit-exp 4))
    ((lit-exp 5) (lit-exp 6))))
}|
}

这里，@tt{compound-statement} 有两个字段：标识符列表的列表，对应的表达式列表的列
表。本例中，我们用 @tt{separated-list} 代替了 @tt{arbno}，但是 @tt{arbno} 也会生
成同样的数据。

@exercise[#:level 1 #:tag "exB.1"]{

下列语法按照通常的算术操作符优先级，定义了算术操作表达式：

@envalign*{
         \mathit{Arith\mbox{-}exp} &::= \mathit{Arith\mbox{-}term} \ \{\mathit{Additive\mbox{-}op} \mathit{Arith\mbox{-}term}\}^{*} \\[-3pt]
        \mathit{Arith\mbox{-}term} &::= \mathit{Arith\mbox{-}factor} \ \{\mathit{Multiplicative\mbox{-}op} \mathit{Arith\mbox{-}factor}\}^{*} \\[-3pt]
      \mathit{Arith\mbox{-}factor} &::= \mathit{Number} \\[-3pt]
                                   &::= @tt{( @m{\mathit{Arith\mbox{-}expr}} )} \\[-3pt]
          \mathit{Arith\mbox{-}op} &::= @tt{+} \mid @tt{-} \\[-3pt]
 \mathit{Multiplicative\mbox{-}op} &::= @tt{*} \mid @tt{/}
                         }

这套语法是说，每个算术表达式都是非空项序列的和；每一项都是非空因数序列的生成式；
每个因数是一个常数或者括号表达式。

用 SLLGEN 写出词法规范和语法，根据这套语法进行扫描和解析。验证这套语法能正确处理
优先级，那么，@tt{3+2*66-5} 能正确分组为 @${3 + (2 \times 66) - 5}。

}

@exercise[#:level 2 #:tag "exB.2"]{

上面的语法为什么不能写成 @tt{separated-list}？

}

@exercise[#:level 2 #:tag "exB.3"]{

定义一个解释器，取@exercise-ref{exB.1} 中解析器生成的抽象语法树，将其当作算术表
达式求值。解析器处理通常的算术操作优先级；但解释器要处理关联性，即，确保同一优先
级（比如加和减）的操作从左向右进行。由于这些表达式中没有变量，解释器不需要取环境
参数。

}

@exercise[#:level 2 #:tag "exB.4"]{

扩展前一道练习中的语言和解释器，加入变量。这个新解释器需要环境参数。

}

@exercise[#:level 1 #:tag "exB.5"]{

给语言和解释器添加单参数操作取反，使之能正确处理输入 @tt{3*-2}。

}
