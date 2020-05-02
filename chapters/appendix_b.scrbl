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

@title[#:tag "sllgen-parsing-system"]{SLLGEN解析系统}

程序只是字符串。要处理程序，需要将这些字符归类为有意义的单元。这种归类通常分为两
个阶段：@emph{扫描} (@emph{scanning})和@emph{解析} (@emph{parsing})。

扫描过程将字符序列分为单词，标点等等。这些单元叫做@emph{词条} (@emph{lexical
items})，@emph{词素} (@emph{lexeme})，或者最常见的@emph{词牌} (@emph{token})。解
析过程将词牌序列组织成有层次的语法结构，如表达式，语句和块。这就像用从句组织句子。

SLLGEN是一个Scheme包，用来生成解析器和扫描器。在本附录中，我们首先讨论扫描和解析
的基础，然后考虑如何用SLLGEN实现这些功能。

@section[#:tag "B.1"]{扫描}

扫描问题如图B.1所示。我们在其中展示了一小段程序，以及应如何将其分割为基本单元。

字符串流应如何分割为词条是语言规范的一部分。语言的这部分规范有时称为@emph{词法规
范} (@emph{lexical specification})。典型的词法规范可能包括：

@itemlist[

 @item{任何空格和换行序列都等价于单个空格。}

 @item{注释以@tt{%}开头，持续到行尾。}

 @item{标识符是以字母开头的字母和数字序列。}

]

@nested[#:style eopl-figure]{
@(image "../images/task-of-scanner"
  #:suffixes (list ".pdf" ".svg")
  "扫描器的任务")

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "扫描器的任务"))]
}

扫描器的任务是遍历和分析输入，产生含有这些词条的数据结构。通常的语言中，扫描器可
能是一个过程，在调用时，由输入产生“下一个”词牌。

可以从头写出一个扫描器，但那又麻烦，又易错。更好的方式是写出指定语言的词法规范。
这一任务最常用的语言是@emph{正则表达式} (@emph{regular expressions})。正则表达式
语言定义如下：

@$${\mathit{R} ::= \mathit{Character} \mid \mathit{RR} \mid \mathit{R} \cup \mathit{R} \mid \neg\mathit{Character}}

每个正则表达式匹配一些字符串。我们可以用归纳法定义每个正则表达式匹配的字符串集合。

@itemlist[

 @item{匹配字符@${c}的字符串只含字符@${c}。}

 @item{匹配@${\neg c}的字符串只含一个@${c}之外的字符。}

 @item{匹配@${\mathit{RS}}的字符串由匹配@${\mathit{R}}和匹配@${\mathit{S}}的字符
 串相接而得。这叫做@emph{串联} (@emph{concatenation})。}

 @item{匹配@${\mathit{R} \cup \mathit{S}}的字符串匹配@${\mathit{R}}或
 @${\mathit{S}}。这有时写作@${\mathit{R} \mid \mathit{S}}，叫做@emph{并联}
 (@emph{alternation})。}

 @item{匹配@${\mathit{R}^{*}}的字符串由@${n} (@${n \geq 0})个匹配@${\mathit{R}}
 的字符串串联而得。这叫做@${\mathit{R}}的@emph{克莱尼闭包} (@emph{Kleene
 closure})。}

]

看些例子更有帮助：

@itemlist[

 @item{@${ab}只匹配字符串@tt{ab}。}

 @item{@${ab \cup cd}匹配字符串@tt{ab}或@tt{cd}。}

 @item{@${(ab \cup cd)(ab \cup cd \cup ef)}匹配字符串@tt{abab}、@tt{abcd}、
 @tt{abef}、@tt{cdab}、@tt{cdcd}和@tt{cdef}。}

 @item{@${(ab)^{*}}匹配空字符串、@tt{ab}、@tt{abab}、@tt{ababab}、@tt{abababab}、
 @tt{...}。}

 @item{@${(ab \cup cd)^{*}}匹配空字符串、@tt{ab}、@tt{cd}、@tt{abab}、@tt{abcd}、
 @tt{cdab}、@tt{cdcd}、@tt{ababab}、@tt{...cdcdcd}、@tt{...}。}

]

上面的例子解释了不同操作的优先级，所以，@${{ab}^{*} \cup cd}表示@${(a(b^{*}))
\cup (cd)}。

我们例子中的规范可用正则表达式写作

@nested[#:style 'code-inset]{
@verbatim|{
|@${whitespace = (space \cup newline)(space \cup newline)^{*}}
|@${comment = @tt{%}(\neg newline)^{*}}
|@${identifier = letter(letter \cup digit)^{*}}
}|
}

扫描器用正则表达式获取词牌时，规则总是取@emph{最长}匹配。所以@tt{xyz}扫描为一个
标识符，而非三个。

扫描器找到一个词牌时，它返回的数据结构至少包含下列数据：

@itemlist[

 @item{一个@emph{类别} (@emph{class})，描述词牌的种类。类别的集合是词法规范的一
 部分。SLLGEN使用Scheme符号区分这些类别；其他语法分析器可能使用其他数据结构。}

 @item{一段数据，描述特定词牌。这段数据的性质也是词法规范的一部分。在我们的系统
 中，数据如下：标识符的数据是由词牌字符串产生的Scheme符号；数字的数据是由数字字
 面值描述的数值；字符串字面值的数据就是字符串。字符串数据用作关键字和标点。在没
 有符号的实现语言中，可以改用字符串（标识符的名字），或者以标识符为索引的哈希表
 （@emph{符号表} (@emph{symbol table})）条目。}

 @item{一段数据，描述该词牌在输入中的位置。解析器用这一信息帮程序员定位语法错误。}

]

通常，词牌的内部结构只与扫描器和解析器相关，所以我们不再详加介绍。

@section[#:tag "B.2"]{解析}

解析过程将词牌序列组织成有层次的语法结构，如表达式，语句和块。这就像用从句组织句
子。语言的语法结构通常由BNF定义，也叫做@emph{上下文无关语法} (@emph{context-free
grammar})（@secref{s1.1.2}）。

解析器取一词牌序列作为输入，输出一棵抽象语法树（@secref{s2.5}）。SLLGEN生成的抽
象语法树可用@tt{define-datatype}描述。对给定的语法，每个非终止符都对应一个数据类
型。对每个非终止符，每个以其为左边内容的生成式都对应一个变体。式子右边出现的每个
非终止符，标识符和数字都对应变体中的一个字段。@secref{s2.5}有一个简单示例。当语
法中有多个非终止符时，可以考虑练习4.22中的语法。

@envalign*{
      \mathit{Statement} &::= @tt{{ @m{\mathit{Statement}} ; @m{\mathit{Statement}} }} \\[-3pt]
                         &::= @tt{while @m{\mathit{Expression}} do @m{\mathit{Statement}}} \\[-3pt]
                         &::= @tt{@m{\mathit{Identifier}} := @m{\mathit{Expression}}} \\[-3pt]
     \mathit{Expression} &::= \mathit{Identifier} \\[-3pt]
                         &::= @tt{(@m{\mathit{Expression}} - @m{\mathit{Expression}})}
                         }

这个语法产生的树由如下数据类型描述：

@nested{
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
]

式子右边的每个非终止符对应的树作为一个字段；标识符对应的符号作为一个字段。变体名
字在用SLLGEN写语法时指定。字段名自动生成；这里，我们给字段起了一些便于记忆的名字。
例如，输入

@verbatim|{{x := foo; while x do x := (x - bar)}}|

产生输出

@racketblock[
#(struct:compound-statement
   #(struct:assign-statement x #(struct:var-exp foo))
   #(struct:while-statement
      #(struct:var-exp x)
      #(struct:assign-statement x
         #(struct:diff-exp
            #(struct:var-exp x)
            #(struct:var-exp bar)))))
]

}

@section[#:tag "B.3"]{SLLGEN中的扫描器和解析器}

@subsection[#:style 'unnumbered #:tag "B.3-scanners"]{定义扫描器}

在SLLGEN中，扫描器用正则表达式定义。我们的例子用SLLGEN，要写成下面这样：

@racketblock[
(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))
]

如果扫描器要和处理关键字或标点（如@tt{while}或@tt{=}）的解析器共用，不需要手动将
这些放入扫描器中；解析器生成器会自动添加它们。

SLLGEN中的扫描器定义是满足如下语法语法的列表：

@envalign*{
            \mathit{Scanner\mbox{-}spec} &::= @tt{(@m{\{\mathit{Regexp\mbox{-}and\mbox{-}action}\}^{*}})} \\[-3pt]
\mathit{Regexp\mbox{-}and\mbox{-}action} &::= @tt{(@m{\mathit{Name}} (@m{\{\mathit{Regexp}\}^{*}}) @m{\mathit{Action}})} \\[-3pt]
                           \mathit{Name} &::= \mathit{Symbol} \\[-3pt]
                         \mathit{Regexp} &::= \mathit{String} \mid @tt{letter} \mid @tt{digit} \mid @tt{whitespace} \mid @tt{any} \\[-3pt]
                                         &::= @tt{(not @m{\mathit{Character}})} \mid @tt{(or @m{\{\mathit{Regexp}\}^{*}})} \\[-3pt]
                                         &::= @tt{(arbno @m{\mathit{Regexp}})} \mid @tt{(concat @m{\{\mathit{Regexp}\}^{*}})} \\[-3pt]
                         \mathit{Action} &::= @tt{skip} \mid @tt{symbol} \mid @tt{number} \mid @tt{string}
                         }

列表中的每一项都定义了一个正则表达式，定义包含名字，一系列正则表达式，以及匹配成
功时的动作。名字是一个Scheme符号，表示词牌的类别。

由于扫描器中的顶层@emph{正则表达式} (@emph{regexp})几乎总是串联而得，定义的第二
部分是一系列正则表达式。正则表达式可以是一个字符串；可以是预先定义的四个测试器之
一：@tt{letter}（匹配任何字母），@tt{digit}（匹配任何数字），@tt{whitespace}（匹
配任何Scheme空白字符），以及@tt{any}（匹配任意字符）；可以使一个去反字符；也可以
是组合而得的正则表达式，采用Scheme式的语法，以@tt{or}表示并联，以@tt{concat}表示
串联，以@tt{arbno}表示克莱尼星号。

扫描器工作时，把字符收集到一个缓存中。当扫描器断定找出了定义中所有正则表达式的最
长匹配时，它执行对应正则表达式的@emph{动作}。

动作为下列之一：

@itemlist[

 @item{符号@tt{skip}。这表示词牌结束，但不产生任何词牌。扫描器继续处理字符串，找
 出下一个词牌。这一动作用于空白字符和注释。}

 @item{符号@tt{symbol}。缓存中的字符转换为一个Scheme符号，并产生一个词牌，以指定
 类别名为其类别，以对应符号为其数据。}

 @item{符号@tt{number}。缓存中的字符转换为一个Scheme数值，并产生一个词牌，以指定
 类别名为其类别，以对应数值为其数据。}

 @item{符号@tt{string}。缓存中的字符转换为一个Scheme字符串，并产生一个词牌，以指
 定类别名为其类别，以对应字符串为其数据。}

]

如果两个正则表达式同时为最长匹配，@tt{string}优先于@tt{symbol}。这条规则意味着关
键字会按关键字处理，而非标识符。

@subsection[#:style 'unnumbered #:tag "B.3-grammars"]{定义语法}

SLLGEN还包含一种定义语法的语言。上面的简单语法用SLLGEN写作

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
]

SLLGEN中的语法是由下列语法描述的列表：

@envalign*{
            \mathit{Grammar} &::= @tt{(@m{\{\mathit{Production}\}^{*}})} \\[-3pt]
         \mathit{Production} &::= @tt{(@m{\mathit{Lhs}} (@m{\{\mathit{Rhs\mbox{-}item}\}^{*}}) @m{\mathit{Prod\mbox{-}name}})} \\[-3pt]
               \mathit{Lhs} &::= \mathit{Symbol} \\[-3pt]
   \mathit{Rhs\mbox{-}item} &::= \mathit{Symbol} \mid \mathit{String} \\[-3pt]
                            &::= @tt{(arbno @m{\mathit{\{Rhs\mbox{-}item\}^{*}}})} \\[-3pt]
                            &::= @tt{(separated-list @m{\mathit{\{Rhs\mbox{-}item\}^{*}}} @m{\mathit{String}})} \\[-3pt]
  \mathit{Prod\mbox{-}name} &::= \mathit{Symbol}
                         }

语法是生成式列表。第一个生成式的左边是语法的起始符号。每个生成式包含左边（一个非
终止符号），右边（@${rhs\mbox{-}item}的列表），以及生成式名字。生成式的右边是符
号或者字符串列表。符号是非终止符；字符串是字符串字面值。式子右边也可以包含
@tt{arbno}或@tt{separated-list}；这些留待下面讨论。生成式的名字是一个符号，成为
@tt{define-datatype}中对应生成式的变体名。

在SLLGEN中，解析器必须在仅获知以下内容的条件下，通过语法断定生成式：(1) 正在寻找
哪一非终止符，(2) 正在解析的字符串中的首个符号（词牌）。这种形式的语法叫做
@${LL(1)}语法；SLLGEN表示Scheme @${LL(1)} 解析器@bold{生成}器（Scheme @${LL(1)}
parser GENerator）。在实践中，这有些过于严格了，但足以应付本书需要。如果输入语法
不满足这一条件，SLLGEN给出一条警告。

@subsection[#:style 'unnumbered #:tag "B.3-operations"]{SLLGEN的操作}

SLLGEN包含几个过程，将扫描器和语法结合起来，形成可以执行的解析器。图B.2展示了用
SLLGEN定义语言扫描器和解析器的例子。

过程@tt{sllgen:make-define-datatypes}负责为语法的每个生成式产生一个
@tt{define-datatype}表达式，供@tt{cases}使用。过程
@tt{sllgen:list-define-datatypes}也生成@tt{define-datatype}表达式，但是会以列表
形式返回，而非执行它们。由这些过程生成的字段名不够直观，因为语法中没有这些信息；
要得到更好的字段名，需要写出@tt{define-datatype}。

过程@tt{sllgen:make-string-scanner}取一扫描器和一语法，生成一个扫描过程。得出的
过程可以处理一个字符串，得到一个词牌列表。语法用来给得到的扫描过程添加关键字。这
一过程主要用于调试。

过程@tt{sllgen:make-stream-parser}生成一个解析器。解析器是一过程，它取一字符串，
用扫描器扫描它，用语法解析它，然后返回一棵抽象语法树。像
@tt{sllgen:make-string-scanner}一样，语法中的字符串字面值包含在扫描器中。

SLLGEN也可以用来生成读入-求值-打印循环（@secref{s3.1}）。过程
@tt{sllgen:make-stream-parser}与字符串版本类似，但是它的输入是字符流，输出是词牌
流。过程@tt{sllgen:make-rep-loop}取一字符串，一个单参数过程，一个流式解析器，生
成一个读入-求值-打印循环，以指定字符串为标准输出中的提示符，从标准输入读入字符，
解析它们，然后以指定过程处理抽象语法树，将结果打印出来。例如：


@nested[#:style eopl-figure]{
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

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "使用SLLGEN"))]
}

@nested[#:style 'code-inset]{
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

控制流程从这一循环返回Scheme读入-求值-打印循环的方式由系统决定。

@subsection[#:style 'unnumbered #:tag "B.3-arbno"]{@tt{arbno}和
@tt{separated-list}模板关键字}
