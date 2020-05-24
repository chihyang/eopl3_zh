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

@title[#:style part-title-style-numbered #:tag "modules"]{模块}

对只有几百行代码的系统，我们介绍的语言特性已非常强大。如果我们要设计更大的系统，
有数千行代码，我们就还需要一些别的佐料。

@itemlist[#:style 'ordered

 @item{我们需要一种好办法，将系统分为相对独立的部分，并能说明各部分之间的依赖关
 系。}

 @item{我们需要一种更好的方式来控制名字的作用范围和绑定。词法定界是控制命名的有
 效工具，但当程序更大或有多个来源时还是不够。}

 @item{我们需要一种方式加强抽象边界。在@secref{da}，我们介绍了抽象数据类型的思想。
 在类型的实现中，类型的值只能通过类型接口中的过程创建和操作。我们把这叫做@emph{抽象边界}
 (@emph{abstraction boundary})。如果程序尊重这种界限，我们可以改变数据类型的实现。
 但是，如果某些代码打破了抽象，依赖实现细节，那么我们就无法任意修改实现而不破坏
 其他代码。}

 @item{最后，我们需要一种方式，将这些部分灵活组合，那么同一部分可复用于不同地方。}

]

本章，我们介绍@emph{模块} (@emph{module})，以满足这些需求。具体来说，我们展示了
如何用类型系统创建和强化抽象边界。

我们的模块语言中，程序包含一系列@emph{模块定义} (@emph{module definition})，后跟
待求值的表达式。每个模块定义把一个名字绑定到一个@emph{模块}。创建的模块可能
是@emph{简单模块} (@emph{simple module})，即类似环境的一些绑定；也可能是@emph{模
块过程} (@emph{module procedure})，取一模块，生成另一模块。

每个模块都有一套@emph{接口} (@emph{interface})。简单模块具有@emph{简单接口}
(@emph{simple interface})，接口列出模块提供的绑定及其类型。模块过程的接口指定参
数模块和返回模块的接口，就像过程的类型指明参数和结果的类型。

这些接口就像类型一样，决定了模块如何组合。因为求出示例程序的值非常简单，因此我们
强调其类型。如前所见，理解这些语言的定界和绑定规则是程序分析和求值的关键。

@section[#:style section-title-style-numbered #:tag "s8.1"]{简单模块系统}

我们的第一种语言名叫SIMPLE-MODULES，只有简单模块。它没有模块过程，只创建非常简单
的抽象边界。几种流行语言使用与之类似的模块系统。

@subsection[#:style section-title-style-numbered #:tag "s8.1.1"]{例子}

设想一个软件项目中有三名开发者：爱丽丝，鲍伯和查理。爱丽丝、鲍伯和查理正在开发项
目中相对独立的几部分。这些开发者散居各处，时区都可能不同。项目的每部分都要实现一
套@secref{s2.1}那样的接口，但接口的实现可能涉及大量其他过程。而且，开发者们需要
确保没有命名冲突，当各部分集成到一起时，不会干扰项目的其他部分。

要实现这一目标，开发者们需要公布一套接口，列出每个供他人使用的过程名字。模块系统
要保证这些名字是公开的，而它们使用的其他名字则是私有的，不会被项目中其他代码篡改。

我们可以用@secref{expr}中的定界技术，但这些无法应对更大的工程。所以，我们使用模
块系统。开发者们各给出一个模块，包含公开接口和私有实现。他们能看到自己模块的接口
和实现，但爱丽丝只能看到他人模块的接口。她的所做所为不会影响其他模块的实现，其他
模块的实现也不会影响她的。（如图8.1所示）

@nested[#:style eopl-figure]{
@centered{
@(image "../images/alice-view"
  #:suffixes (list ".pdf" ".svg")
  "项目中，爱丽丝所见的三个模块")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "项目中，爱丽丝所见的三个模块"))]
}

这是SIMPLE-MODULES的简单例子。

@nested[#:style eopl-example]{
@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [a : int
   b : int
   c : int]
 body
  [a = 33
   x = -(a,1)  % = 32
   b = -(a,x)  % = 1
   c = -(x,b)] % = 31
let a = 10
in -(-(from m1 take a,
       from m1 take b),
     a)
}|
}

类型为@tt{int}，值为@${((33-1)-10)=22}。
}
}

此程序从名为@tt{m1}的模块定义开始。像其他模块一样，它有@emph{接口}和@tt{主体}。
主体@emph{实现}接口。接口@emph{声明}变量@tt{a}、@tt{b}和@tt{c}。主体@emph{定义}
了@tt{a}、@tt{x}、@tt{b}和@tt{c}。

求程序的值时，也会求出@tt{m1}主体中表达式的值。变量@tt{from m1 take a}、@tt{from
m1 take b}和@tt{from m1 take c}绑定到适当的值，模块定义之后在它们的作用范围内。
由于@tt{from m1 take x}未在接口中声明，所以模块定义之后不在它的作用范围内。

为了同@emph{简单变量} (@emph{simple variable})却别，我们称这些新变量为@emph{受限
变量} (@emph{qualified})。在传统语言中，受限变量写作@tt{m1.a}、@tt{m1:a}或
@tt{m1::a}。在@secref{oac}探讨的面向对象语言中，@tt{m1.a}常表示其他内容。

我们说接口@emph{提出} (@emph{offer})（或称@emph{公布} (@emph{advertise})，或
称@emph{承诺} (@emph{promise})）三个整型值，主体@emph{供应}（@emph{supply}或
@emph{provide}）（或称@emph{输出} (@emph{export})）这些值。当模块主体供应的值类
型与接口命名变量时公布的相符时，称主体@emph{满足} (@emph{satisfy})接口。

在主体中，定义具有@tt{let*}那样的作用范围，所以@tt{x}、@tt{b}和@tt{c}在@tt{a}的
作用范围内。部分作用范围如图8.2所示。

本例中，以@tt{let a = 10}开头的表达式是@emph{程序主体} (@emph{program body})。它
的值即程序的值。

每个模块都在模块主体和程序其他部分之间建立了抽象边界。模块主体中的表达式在抽象边
界@emph{之内}，其他部分在抽象边界@emph{之外}。模块主体也可以供应不在接口中的名字
绑定，但那些绑定在程序主体和其他模块中不可见，正如图8.1所示。在我们的例子中，程
序主体不在@tt{from m1 take x}的作用范围内。如果我们写@tt{-(from m1 take a, from
m1 take x)}，程序就会是异常类型。

@nested[#:style eopl-example]{
程序

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [u : bool]
 body
  [u = 33]

44
}|
}

类型异常。就算程序的其他部分不使用那些值，模块主体也得将接口中的名字与适当类型的
值关联起来。
}
}

@nested[#:style eopl-figure]{
@centered{
@(image "../images/module-contour"
  #:suffixes (list ".pdf" ".svg")
  "简单模块中的一些作用范围")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "简单模块中的一些作用范围"))]
}

@nested[#:style eopl-example]{
模块主体必须供应接口中声明的所有绑定。例如，

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [u : int
   v : int]
 body
  [u = 33]

44
}|
}

类型异常，因为@tt{m1}的主体没有提供接口中公布的所有值。
}
}

@nested[#:style eopl-example]{
为了让实现简单一点，我们的语言要求模块主体按照接口声明的顺序给出各值。因此

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [u : int
   v : int]
 body
  [v = 33
   u = 44]

from m1 take u
}|
}

类型异常。可以弥补这一点（练习8.8，8.17）。
}
}

@nested[#:style eopl-example]{
在我们的语言中，模块具有@tt{let*}式的作用范围（练习3.17）。例如，

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [u : int]
 body
  [u = 44]

module m2
 interface
  [v : int]
 body
  [v = -(from m1 take u,11)]

-(from m1 take u, from m2 take v)
}|
}

类型为@tt{int}。但如果我们交换定义的顺序，得

@nested[#:style 'code-inset]{
@verbatim|{
module m2
 interface
  [v : int]
 body
  [v = -(from m1 take u,11)]

module m1
 interface
  [u : int]
 body
  [u = 44]

-(from m1 take u, from m2 take v)
}|
}

类型异常，因为@tt{m2}主体中使用@tt{from m1 take u}之处不在后者的作用范围内。
}
}

@subsection[#:style section-title-style-numbered #:tag "s8.1.2"]{实现简单模块系统}

@subsubsection[#:style section-title-style-unumbered #:tag "s8.1-syntax"]{语法}

SIMPLE-MODULES的程序包含一串模块定义，然后是一个表达式。

@envalign*{
           \mathit{Program} &::= \{\mathit{ModuleDefn}\}^{*} \mathit{Expression} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{a-program (m-defs body)}}}

模块定义包含名字、接口和主体。

@envalign*{
           \mathit{ModuleDefn} &::= @tt{module} \mathit{Identifier} @tt{interface} \mathit{Iface} @tt{body} \mathit{ModuleBody} \\[-3pt]
            &\mathrel{\phantom{::=}} \fbox{@tt{a-module-definition (m-name expected-iface m-body)}}}

简单模块的接口包含任意数量的声明。每个声明指定程序中一个变量的类型。我们称之
为@emph{值声明} (@emph{value declaration})，因为要声明的变量表示一个值。在后面几
节中，我们介绍其他种类的接口和声明。

@envalign*{
           \mathit{Iface} &::= @tt["["] \{\mathit{Decl}\}^{*} @tt["]"] \\[-3pt]
        &\mathrel{\phantom{::=}} \fbox{@tt{simple-iface (decls)}} \\[-5pt]
            \mathit{Decl} &::= \mathit{Identifier} @tt{:} \mathit{Type} \\[-3pt]
        &\mathrel{\phantom{::=}} \fbox{@tt{val-decl (var-name ty)}}
            }

模块主体包含任意数量的定义。每个定义将变量和某个表达式的值关联起来。

@envalign*{
           \mathit{ModuleBody} &::= @tt["["] \{\mathit{Defn}\}^{*} @tt["]"] \\[-3pt]
             &\mathrel{\phantom{::=}} \fbox{@tt{defns-module-body (defns)}} \\[-5pt]
                 \mathit{Defn} &::= \mathit{Identifier} @tt{=} \mathit{Expression} \\[-3pt]
             &\mathrel{\phantom{::=}} \fbox{@tt{val-defn (var-name exp)}}
            }

我们的表达式与CHECKED（@secref{s7.3}）相同，但要修改语法，新增一种表达式，以便使
用受限变量。

@envalign*{
           \mathit{Expression} &::= @tt{from} \mathit{Identifier} @tt{take} \mathit{Identifier} \\[-3pt]
             &\mathrel{\phantom{::=}} \fbox{@tt{qualified-var-exp (m-name var-name)}}
            }

@subsubsection[#:style section-title-style-unumbered #:tag "s8.1-the-interpreter"]{解释器}

求模块主体的值会得到一个@emph{模块}。在我们的简单语言中，模块是一个环境，包含输
出的所有绑定。我们用数据类型@tt{typed-module}表示这些。

@racketblock[
(define-datatype typed-module typed-module?
  (simple-module
    (bindings environment?)))
]

我们用一种新的绑定在环境中绑定模块名：

@racketblock[
(define-datatype environment environment?
  (empty-env)
  (extend-env ...as before...)
  (extend-env-rec ...as before...)
  (extend-env-with-module
    (m-name symbol?)
    (m-val typed-module?)
    (saved-env environment?)))
]

例如，如果我们的程序是

@nested{

@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [a : int
   b : int
   c : int]
 body
  [a = 33
   b = 44
   c = 55]
module m2
 interface
  [a : int
  b : int]
 body
  [a = 66
   b = 77]
let z = 99
in -(z, -(from m1 take a, from m2 take a))
}|
}

那么声明@${z}之后的环境是

@racketblock[
#(struct:extend-env
   z #(struct:num-val 99)
   #(struct:extend-env-with-module
      m2 #(struct:simple-module
            #(struct:extend-env
               a #(struct:num-val 66)
               #(struct:extend-env
                  b #(struct:num-val 77)
                  #(struct:empty-env))))
      #(struct:extend-env-with-module
         m1 #(struct:simple-module
               #(struct:extend-env
                  a #(struct:num-val 33)
                  #(struct:extend-env
                     b #(struct:num-val 44)
                     #(struct:extend-env
                        c #(struct:num-val 55)
                        #(struct:empty-env)))))
         #(struct:empty-env))))
]

在这个环境中，绑定到@tt{m1}和@tt{m2}的简单模块，个包含一个小的环境。

}

我们用@tt{lookup-qualified-var-in-env}求用到的受限变量@tt{from @${m} take
@${var}}的值。它在当前环境中查找模块@${m}，然后在得到的环境中查找@${var}。

@racketblock[
@#,elem{@bold{@tt{lookup-qualified-var-in-env!}} : @${\mathit{Sym} \times \mathit{Sym} \times \mathit{Env} \to \mathit{ExpVal}}}
(define lookup-qualified-var-in-env
  (lambda (m-name var-name env)
    (let ((m-val (lookup-module-name-in-env m-name env)))
      (cases typed-module m-val
        (simple-module (bindings)
          (apply-env bindings var-name))))))
]

欲求程序的值，我们把所有模块定义加入当前环境中，得到初始环境，然后求程序主体的值。
过程@tt{add-module-defns-to-env}遍历模块定义，求每个模块定义主体的值，并将得到的
模块加入当前环境中。如图8.3所示。

最后，欲求模块主体的值，我们按照@tt{let*}式定界，在适当的环境中求每个表达式的值，
得出一环境。过程@tt{defns-to-env}求出的环境中，只包含定义@tt{defns}产生的绑定
（图8.4）。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{ExpVal}}}
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (m-defns body)
        (value-of body
          (add-module-defns-to-env m-defns (empty-env)))))))

@#,elem{@bold{@tt{add-module-defns-to-env}} : @${\mathit{Listof(Defn)} \times \mathit{Env} \to \mathit{Env}}}
(define add-module-defns-to-env
  (lambda (defns env)
    (if (null? defns)
      env
      (cases module-definition (car defns)
        (a-module-definition (m-name iface m-body)
          (add-module-defns-to-env
            (cdr defns)
            (extend-env-with-module
              m-name
              (value-of-module-body m-body env)
              env)))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "SIMPLE-MODULES的解释器，第1部分"))]
}

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{value-of-module-body}} : @${\mathit{ModuleBody} \times \mathit{Env} \to \mathit{TypedModule}}}
(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-module
          (defns-to-env defns env))))))

@#,elem{@bold{@tt{defns-to-env}} : @${\mathit{Listof(Defn)} \times \mathit{Env} \to \mathit{Env}}}
(define defns-to-env
  (lambda (defns env)
    (if (null? defns)
      (empty-env)
      (cases definition (car defns)
        (val-defn (var exp)
          (let ((val (value-of exp env)))
            (let ((new-env (extend-env var val env)))
              (extend-env var val
                (defns-to-env
                  (cdr defns) new-env)))))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "SIMPLE-MODULES的解释器，第2部分"))]
}

@subsubsection[#:style section-title-style-unumbered #:tag "s8.1-checker"]{检查器}

检查器的工作是确保每个模块主体满足其接口，每个变量的使用符合其类型。

我们语言的定界规则很简单：在模块中，按@tt{let*}式定界，依次进入绑定的受限变量的
作用范围。接口告诉我们每个受限变量的类型。声明和定义也都遵循@tt{let*}式定界（见
图8.2）。

就像@secref{types}中的检查器那样，我们用类型环境记录与当前作用范围内各名字相关的
信息。因为我们现在有了模块名，我们要在类型环境中绑定模块名。每个模块名绑定到模块
的接口，作为其类型。

@racketblock[
(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv @#,elem{@emph{...同前...}})
  (extend-tenv-with-module
    (name symbol?)
    (interface interface?)
    (saved-tenv type-environment?)))
]

要找出受限变量@tt{from @${m} take @${var}}的类型，我们首先在类型环境中找出@${m}，
然后在得到的接口中查找@${var}的类型。

@racketblock[
@#,elem{@bold{@tt{lookup-qualified-var-in-tenv}} : @${\mathit{Sym} \times \mathit{Sym} \times \mathit{Tenv} \to \mathit{Type}}}
(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name tenv)
    (let ((iface (lookup-module-name-in-tenv tenv m-name)))
      (cases interface iface
        (simple-iface (decls)
          (lookup-variable-name-in-decls var-name decls))))))
]


@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{type-of-program}} : @${\mathit{Program} \to \mathit{Type}}}
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (module-defns body)
        (type-of body
          (add-module-defns-to-tenv module-defns
            (empty-tenv)))))))

@#,elem{@bold{@tt{add-module-defns-to-tenv}} : @${\mathit{Listof(ModuleDefn)} \times \mathit{Tenv} \to \mathit{Tenv}}}
(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
      tenv
      (cases module-definition (car defns)
        (a-module-definition (m-name expected-iface m-body)
          (let ((actual-iface (interface-of m-body tenv)))
            (if (<:-iface actual-iface expected-iface tenv)
              (let ((new-tenv
                      (extend-tenv-with-module
                        m-name
                        expected-iface
                        tenv)))
                (add-module-defns-to-tenv
                  (cdr defns) new-tenv))
              (report-module-doesnt-satisfy-iface
                m-name expected-iface actual-iface))))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "SIMPLE-MODULES的检查器，第1部分"))]
}

就像@secref{types}那样，检查程序类型的过程类似于求程序的值，但我们记录的不是值，
而是类型。我们用@tt{type-of-program}代替@tt{value-of-program}，用
@tt{add-module-defns-to-tenv}代替@tt{add-module-defns-to-env}。过程
@tt{add-module-defns-to-tenv}用@tt{<:-iface}，检查各模块主体产生的接口与宣称的接
口是否相符；如果相符，就将模块加入到类型环境中；否则报错。

模块主体的接口将主体中各定义的变量与类型关联起来。例如，如果我们查看第一个例子的
主体，

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
[a = 33
 x = -(a,1)
 b = -(a,x)
 c = -(x,b)]
}|
}

可得

@nested[#:style 'code-inset]{
@verbatim|{
[a : int
 x : int
 b : int
 c : int]
}|

}

}

一旦有了一套接口来描述模块主体输出的所有绑定，我们就能将其与模块公布的接口比较。

回忆一下，简单接口包含一个声明列表。过程@tt{defns-to-decls}创建这样一个列表，调
用@tt{type-of}找出每个定义的类型。在每一步，它还按@tt{let*}式定界，扩展局部类型
环境（见图8.6）。

剩下的只是用@tt{<:-iface}比较每个模块的期望类型与实际类型。我们定义@tt{<:}为，若
@${i_1 @tt{<:} i_2}，则满足接口@${i_1}的任何模块也满足接口@${i_2}。例如

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
[u : int         [u : int
 v : int    <:    z : int]
 z : int]
}|
}

因为满足接口@tt{[u : int v : bool z : int]}的任何模块都供应了接口@tt{[u : int z
: int]}公布的所有值。

}

对我们的简单模块语言，@tt{<:-iface}只需调用@tt{<:-decls}比较声明。这些过程取一
@tt{tenv}参数，简单模块系统不使用，但@secref{s8.2}需要。见图8.7。

过程@tt{<-decls}执行主要的比较工作，比较两个声明集合。如果@${decls_1}和
@${decls_2}是两个声明集合，当且仅当模块能供应@${decls_1}中声明的绑定时，也能供应
@${decls_2}中声明的绑定，我们说@${decls_1 <: decls_2}。如果@${decls_2}中的所有声
明，在@${decls_1}中都有与之匹配的声明，就能保证这一点，就像上面的例子那样。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{interface-of}} : @${\mathit{ModuleBody} \times \mathit{Tenv} \to \mathit{Iface}}}
(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-iface
          (defns-to-decls defns tenv))))))

@#,elem{@bold{@tt{defns-to-decls}} : @${\mathit{Listof(Defn)} \times \mathit{Tenv} \to \mathit{Decl}}}
(define defns-to-decls
  (lambda (defns tenv)
    (if (null? defns)
      '()
      (cases definition (car defns)
        (val-defn (var-name exp)
          (let ((ty (type-of exp tenv)))
            (cons
              (val-decl var-name ty)
              (defns-to-decls
                (cdr defns)
                (extend-tenv var-name ty tenv)))))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "SIMPLE-MODULES的检查器，第2部分"))]
}

过程@tt{<:-decls}首先检查@tt{decls1}和@tt{decls2}。若@tt{decls2}为空，那么它对
@tt{decls1}无所要求，所以结果为@tt{#t}。若@tt{decls2}非空，但@tt{decls1}为空，那
么@tt{decls2}有所要求，但@tt{decls1}无可提供，所以结果为@tt{#f}。否则，我们比较
@tt{decls1}和@tt{decls2}声明的第一对变量的名字；若二者相同，那么它们的类型必须匹
配，然后我们递归处理两个声明列表余下的部分；若他们不同，那么我们递归处理
@tt{decls1}的@tt{cdr}，找到匹配@tt{decls2}中第一个声明的内容。

这样，简单模块系统就完成了。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{<:-iface}} : @${\mathit{Iface} \times \mathit{Iface} \times \mathit{Tenv} \to \mathit{Bool}}}
(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases interface iface1
      (simple-iface (decls1)
        (cases interface iface2
          (simple-iface (decls2)
            (<:-decls decls1 decls2 tenv)))))))

@#,elem{@bold{@tt{<:-decls}} : @${\mathit{Listof(Decl)} \times \mathit{Listof(Decl)} \times \mathit{Tenv} \to \mathit{Bool}}}
(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond
      ((null? decls2) #t)
      ((null? decls1) #f)
      (else
        (let ((name1 (decl->name (car decls1)))
              (name2 (decl->name (car decls2))))
          (if (eqv? name1 name2)
            (and
              (equal?
                (decl->type (car decls1))
                (decl->type (car decls2)))
              (<:-decls (cdr decls1) (cdr decls2) tenv))
            (<:-decls (cdr decls1) decls2 tenv)))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "SIMPLE-MODULES的接口比较"))]
}

@exercise[#:level 1 #:tag "ex8.1"]{

修改检查器，检测并拒绝任何定义两个同名模块的程序。

}

@exercise[#:level 1 #:tag "ex8.2"]{

过程@tt{add-module-defns-to-env}不完全正确，因为它加入了模块定义的所有值，而不只
是接口中的值。修改@tt{add-module-defns-to-env}，只加入接口中声明的值。
@tt{add-module-defns-to-tenv}也有此问题吗？

}

@exercise[#:level 1 #:tag "ex8.3"]{

修改语言的语法，以@tt{m.v}代替@tt{from m take v}使用受限变量。

}

@exercise[#:level 1 #:tag "ex8.4"]{

修改语言的表达式，像练习7.24那样，加入多声明@tt{let}，多参数过程和多声明
@tt{letrec}。

}

@exercise[#:level 1 #:tag "ex8.5"]{

允许在模块主体中使用@tt{let}和@tt{letrec}声明。例如，可以写

@nested[#:style 'code-inset]{
@verbatim|{
module even-odd
 interface
  [even : (int -> bool)
   odd  : (int -> bool)]
 body
  letrec
   bool local-odd (x : int)  = ... (local-even -(x,1)) ...
   bool local-even (x : int) = ... (local-odd -(x,1)) ...
  in [even = local-even
      odd = local-odd]
}|
}

}

@exercise[#:level 2 #:tag "ex8.6"]{

允许在模块主体中定义局部模块。例如，可以写

@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [u : int
   v : int]
 body
  module m2
   interface [v : int]
   body [v = 33]
  [u = 44
   v = -(from m2 take v, 1)]
}|
}

}

@exercise[#:level 2 #:tag "ex8.7"]{

扩展前一题的解答，允许模块将其他模块作为输出的一部分。例如，可以写

@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [u : int
   n : [v : int]]
 body
  module m2
   interface [v : int]
   body [v = 33]
  [u = 44
   n = m2]

from m1 take n take v
}|
}

}

@exercise[#:level 2 #:tag "ex8.8"]{

在我们的语言中，模块必须按照接口中的顺序产生值，可以取消这种限制。取消它。

}

@exercise[#:level 2 #:tag "ex8.9"]{

我们说我们的模块系统应当对模块之间的依赖关系给出说明。给SIMPLE-MODULES添加这种能
力，要求在各模块主体和程序主体中添加一条@tt{depends-on}语句。那么，模块@tt{m}不
是在之前声明的所有模块的作用范围中，只是在自身@tt{depends-on}语句中列出模块的作
用范围中。例如，考虑程序

@nested[#:style 'code-inset]{
@verbatim|{
module m1 ...
module m2 ...
module m3 ...
module m4 ...
module m5
 interface [...]
 body
  depends-on m1, m3
  [...]
}|
}

@tt{m5}的主体仅在来自@tt{m1}或@tt{m3}的受限变量的作用范围中。使用@tt{from m4
take x}将造成类型异常，即使@tt{m4}输出了@tt{x}的值。

}

@exercise[#:level 3 #:tag "ex8.10"]{

我们还可以用@tt{depends-on}这样的特性控制模块主体求值的时机。给SIMPLE-MODULES增
加这种能力，要求给各模块主体和程序主体添加一条@tt{imports}语句。@tt{imports}就像
@tt{depends-on}，不同之处是，仅当（用@tt{imports}语句）将其输入到其他模块时，才
求其主体的值。

这样，如果我们的语言有打印表达式，程序

@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface [] body [x = print(1)]
module m2
 interface [] body [x = print(2)]
module m3
 interface []
 body
  import m2
  [x = print(3)]
import m3, m1
33
}|
}

在返回33之前，将打印2、3和1。这里的模块接口为空，因为我们只关心它们主体求值的顺
序。

}

@exercise[#:level 3 #:tag "ex8.11"]{

修改检查器，用INFERRED作为语言的表达式。这道练习中，你需要修改@tt{<:-decls}，不
能用@tt{equal?}比较类型。例如，在

@nested[#:style 'code-inset]{
@verbatim|{
module m
 interface [f : (int -> int)]
 body [f = proc (x : ?) x]
}|
}

中，类型推导器报告的@tt{f}实际类型可能是@tt{(tvar07 -> tvar07)}，应当接受这个。
但是，我们应拒绝模块


@nested[#:style 'code-inset]{
@verbatim|{
module m
 interface [f : (int -> bool)]
 body [f = proc (x : ?) x]
}|
}

虽然类型推导器报告的类型仍为@tt{(tvar07 -> tvar07)}。

}

@section[#:style section-title-style-numbered #:tag "s8.2"]{声明类型的模块}

至今为止，我们的接口只声明了普通变量及其类型。在下面这种模块语言OPAQUE-TYPES中，
我们还允许接口声明类型。例如，在定义

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [opaque t
   zero : t
   succ : (t -> t)
   pred : (t -> t)
   is-zero : (t -> bool)]
body
 ...
}|
}

中，接口声明了类型@tt{t}，以及该类型值的一些操作@tt{zero}，@tt{succ}，@tt{pred}
和@tt{is-zero}。如同@secref{s2.1}，这套接口可能与算数操作的实现相关。这里@tt{t}
声明为@emph{模糊类型} (@emph{opaque typs})，意为，模块之外的代码不知道这种类型的
值如何表示。所有的外部代码都知道，可以用@tt{from m1 take zero}、@tt{from m1 take
succ}等过程处理@tt{from m1 take t}类型的值。这样，@tt{from m1 take t}就像原生类
型@tt{int}和@tt{bool}一样。

}

我们将介绍两种类型声明：@emph{透明} (@emph{transparent}) 类型和@emph{模糊}
(@emph{opaque})类型。好的模块系统中，二者缺一不可。

@subsection[#:style section-title-style-numbered #:tag "s8.2.1"]{例子}

欲知其用途，再想想我们的几位开发者。爱丽丝一直用包含一对整数的数据结构表示点的横
坐标和纵坐标。她使用的语言具有练习7.8那样的类型，所以她的模块@tt{Alices-points}
接口具有如下声明

@nested[#:style 'code-inset]{
@verbatim|{
initial-point : (int -> pairof int * int)
increment-x : (pairof int * int -> pairof int * int)
}|
}

鲍伯和查理对此直发牢骚。他们不想一遍又一遍地写@tt{pairof int * int}。因此，爱丽
丝用透明类型声明重写她的接口。这样，她可以写

@nested[#:style 'code-inset]{
@verbatim|{
module Alices-points
 interface
  [transparent point = pairof int * int
   initial-point : (int -> point)
   increment-x : (point -> point)
   get-x : (point -> int)
   ...]
}|
}

这减轻了她的工作，因为她写得更少；这也简化了她合作者的工作，因为在他们的实现中可
以写

@nested[#:style 'code-inset]{
@verbatim|{
[transparent point = from Alices-points take point
 foo = proc (p1 : point)
        proc (p2 : point) ...
 ...]
}|
}

在某些项目中，这样做很不错。不过，爱丽丝的项目中，正好要用点表示固定形状金属导轨
上的点，所以横纵坐标不是相互独立的。@note{不妨将金属导轨视为有长度无宽度的圆形轨
迹，以圆心为坐标原点。要保证横坐标变化时，得到的点仍在圆上，则纵坐标也要相应改变。
反之亦然。——@emph{译注}}爱丽丝实现@tt{increment-x}时，要仔细更新纵坐标，以匹配横
坐标的改变。但是鲍伯不知道这点，所以他的过程写作

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
increment-y = proc (p : point)
               unpair x y = p
               in newpair(x, -(y,-1))
}|
}

由于鲍伯的代码修改纵坐标时不随之修改横坐标。爱丽丝的代码就没法正常工作了。

}

更糟糕的是，如果爱丽丝打算修改点的表示，把纵坐标作为第一部分呢？她可以按照新的表
示修改她的代码。但是现在，鲍伯的代码就坏掉了，因为过程@tt{increment-y}修改了序对
中的错误部分。

爱丽丝可以把@tt{point}声明为@emph{模糊}数据类型来解决她的问题。她把接口重写为

@nested[#:style 'code-inset]{
@verbatim|{
opaque point
initial-point : (int -> point)
increment-x : (point -> point)
get-x : (point -> int)
}|
}

现在鲍伯用过程@tt{initial-point}创建新的点，可以用@tt{from Alices-points take
get-x}和@tt{from Alices-points take increment-x}处理点，但是除了爱丽丝接口中过程
外，他无法用其他过程处理点。尤其是，他写不出过程@tt{increment-y}，因为它用了爱丽
丝接口之外的过程处理点。

在本节的剩余部分中，我们探究这些组件的更多例子。

@subsubsection[#:style section-title-style-unumbered #:tag "s8.2-transparent-types"]{透明类型}

我们首先讨论透明类型声明。有时这些又称作@emph{具体} (@emph{concrete})类型
或@emph{类型缩写} (@emph{type abbreviation})。

@nested[#:style eopl-example]{
程序

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [transparent t = int
   z : t
   s : (t -> t)
   is-z? : (t -> bool)]
 body
  [type t = int
   z = 33
   s = proc (x : t) -(x,-1)
   is-z? = proc (x : t) zero?(-(x,z))]

 proc (x : from m1 take t)
  (from m1 take is-z? -(x,0))
}|
}

类型为@tt{(int -> bool)}。
}
}

@nested[#:style eopl-figure]{
@centered{
@(image "../images/module-type"
  #:suffixes (list ".pdf" ".svg")
  "模块类型声明的作用范围")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "模块类型声明的作用范围"))]
}

在接口的剩余部分中，声明@tt{transparent t = int}将@tt{t}绑定到类型@tt{int}，所以
我们可以写@tt{z : t}。更重要的是，在程序的剩余部分中，声明也将@tt{from m1 take
t}绑定到@tt{int}。我们称之为@emph{受限类型} (@emph{qualified type})。这里，我们
用它声明了绑定到变量@tt{z}的类型。声明的作用范围是接口的剩余部分，以及模块定义之
后，程序的剩余部分。

模块主体中的定义@tt{type t = int}在主体的剩余部分中，将@tt{t}绑定到@tt{int}，所
以我们可以写@tt{s = proc (x : t) ...}。像之前那样，定义的作用范围是主体的剩余部
分（见图8.8）。

当然，我们可以给类型起任意名字，也可以声明多个类型。类型声明可以出现在接口中任意
位置，只要每个声明都先于使用。

@subsubsection[#:style section-title-style-unumbered #:tag "s8.2-opaque-types"]{模糊类型}

模块还可以用@tt{opaque-type}声明输出@emph{模糊}类型。模糊类型有时又称作@emph{抽
象类型} (@emph{abstract type})。

@nested[#:style eopl-example]{
我们把例8.6程序中的透明类型替换为模糊类型。得出的程序是

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [opaque t
   z : t
   s : (t -> t)
   is-z? : (t -> bool)]
 body
  [type t = int
   z = 33
   s = proc (x : t) -(x,-1)
   is-z? = proc (x : t) zero?(-(x,z))]

 proc (x : from m1 take t)
  (from m1 take is-z? -(x,0))
}|
}
}
}

接口中的声明@tt{opaque t}用@tt{t}作为一种新的模糊类型名字。模糊类型就像@tt{int}
或@tt{bool}之类的原生类型一样。名为@tt{t}的类型在接口的剩余部分中绑定到这种模糊
类型，而受限类型@tt{from m1 take t}在程序的剩余部分中绑定到同一模糊类型。程序的
剩余部分都知道@tt{from m1 take z}绑定到一个值，其类型为@tt{from m1 take t}；
@tt{from m1 take s}和@tt{from m1 take is-z?}绑定到过程，用来处理这种类型的值。这
就是抽象边界。类型检查器确保表达式的类型为@tt{from m1 take t}时，求值是安全的，
所以表达式的值只能通过这些操作符产生，如@pageref{suitable-env}所述。

与之对应，定义@tt{type t = int}在模块主体内部，将@tt{t}作为@tt{int}的名字，但是，
由于程序的剩余部分是从模块接口得出绑定的，所以对此一无所知。

所以@tt{-(x,0)}类型异常，因为主程序不知道类型@tt{from m1 take t}实为类型@tt{int}。

我们改变程序，删掉算数操作，得

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [opaque t
   z : t
   s : (t -> t)
   is-z? : (t -> bool)]
 body
  [type t = int
   z = 33
   s = proc (x : t) -(x,-1)
   is-z? = proc (x : t) zero?(-(x,z))]

 proc (x : from m1 take t)
  (from m1 take is-z? x)
}|
}

现在，我们的程序类型正常，类型为@tt{(from m1 take t -> bool)}。
}

通过强制抽象边界，类型检查器确保程序只能通过接口提供的过程处理接口提供的值。
如@secref{da}所述，这给我们提供了机制来分离数据类型的用户和实现。接下来，我们给
出这一技术的几个例子。

@nested[#:style eopl-example]{
如果程序使用了模块定义

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module colors
 interface
  [opaque color
   red : color
   green : color
   is-red? : (color -> bool)]
 body
  [type color = int
   red = 0
   green = 1
   is-red? = proc (c : color) zero?(c)]
}|
}

程序没法知道@tt{from colors take color}实为@tt{int}，也不知道@tt{from colors
take green}实为1（也许有个例外：返回颜色作为最终答案，然后打印出来）。
}
}

@nested[#:style eopl-example]{
程序

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module ints1
 interface
  [opaque t
   zero : t
   succ : (t -> t)
   pred : (t -> t)
   is-zero : (t -> bool)]
 body
  [type t = int
   zero = 0
   succ = proc(x : t) -(x,-5)
   pred = proc(x : t) -(x,5)
   is-zero = proc (x : t) zero?(x)]

let z = from ints1 take zero
in let s = from ints1 take succ
   in (s (s z))
}|
}

类型为@tt{from ints1 take t}，值为10。但我们只能通过@tt{ints1}输出的过程处理这个
值。这个模块用表达值@${5*k}表示整数@${k}。用 @secref{s2.1}的表示法，是@${\lceil k
\rceil = 5 * k}。
}
}

@nested[#:style eopl-example]{
在本模块中，@${\lceil k \rceil = -3 * k}。

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module ints2
 interface
  [opaque t
   zero : t
   succ : (t -> t)
   pred : (t -> t)
   is-zero : (t -> bool)]
 body
  [type t = int
   zero = 0
   succ = proc(x : t) -(x,3)
   pred = proc(x : t) -(x,-3)
   is-zero = proc (x : t) zero?(x)]

let z = from ints2 take zero
in let s = from ints2 take succ
   in (s (s z))
}|
}

类型为@tt{from ints2 take t}，值为-6。

}
}

@nested[#:style eopl-example]{在前面的例子中，我们不能直接处理值，但我们能用模块
输出的过程处理它们。像@secref{da}那样，我们可以结合这些过程做有用的工作。这里，
我们用它们写出过程@tt{to-int}，把模块中的值转回类型@tt{int}的值。

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module ints1 |@emph{...同前...}

let z = from ints1 take zero
in let s = from ints1 take succ
in let p = from ints1 take pred
in let z? = from ints1 take is-zero
in letrec int to-int (x : from ints1 take t) =
              if (z? x)
              then 0
              else -((to-int (p x)), -1)
in (to-int (s (s z)))
}|
}

类型为@tt{int}，值为2。

}
}

@nested[#:style eopl-example]{
这例用到的技术与@tt{ints2}中算数操作的实现相同。

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module ints2 |@emph{...同前...}

let z = from ints2 take zero
in let s = from ints2 take succ
in let p = from ints2 take pred
in let z? = from ints2 take is-zero
in letrec int to-int (x : from ints2 take t) =
              if (z? x)
              then 0
              else -((to-int (p x)), -1)
in (to-int (s (s z)))
}|
}

同样类型为@tt{int}，值为2。

}
}

在@secref{s8.3}中，我们展示如何抽象这两个例子中的模式。

@nested[#:style eopl-example]{
在下面的程序中，我们设计一个模块来封装布尔类型。布尔值用整数值表示，但是像例8.8
那样，程序的剩余部分对此一无所知。

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module mybool
 interface
  [opaque t
   true : t
   false : t
   and : (t -> (t -> t))
   not : (t -> t)
   to-bool : (t -> bool)]
 body
  [type t = int
   true = 0
   false = 13
   and = proc (x : t)
          proc (y : t)
           if zero?(x) then y else false
   not = proc (x : t)
           if zero?(x) then false else true
   to-bool = proc (x : t) zero?(x)]

let true = from mybool take true
in let false = from mybool take false
   in let and = from mybool take and
      in ((and true) false)
}|
}

类型为@tt{from mybool take t}，值为13。

}
}

@exercise[#:level 1 #:tag "ex8.12"]{

在例8.13中，@tt{and}和@tt{not}的定义可以从模块内部移到外面吗？@tt{to-bool}呢？

}

@exercise[#:level 1 #:tag "ex8.13"]{

写一个模块，用@${5*k+3}表示整数@${k}，实现算数操作。

}

@exercise[#:level 1 #:tag "ex8.14"]{

下面是@tt{mybool}（例8.13）的另一种定义：

@nested[#:style 'code-inset]{
@verbatim|{
module mybool
 interface
  [opaque t
   true : t
   false : t
   and : (t -> (t -> t))
   not : (t -> t)
   to-bool : (t -> bool)]
 body
  [type t = int
   true = 1
   false = 0
   and = proc (x : t)
          proc (y : t)
           if zero?(x) then false else y
   not = proc (x : t)
          if zero?(x) then true else false
   to-bool = proc (x : t)
              if zero?(x) then zero?(1) else zero?(0)]
}|
}

有没有程序类型为@tt{int}，用@tt{mybool}原来的定义返回一个值，用新的定义返回另一
个值？

}

@exercise[#:level 2 #:tag "ex8.15"]{

写一个模块，实现抽象表。你实现的表应像环境那样，但不是把符号绑定到Scheme值，而是
把整数值绑定到整数值。接口提供一个值，表示空表；两个过程@tt{add-to-table}和
@tt{lookup-in-table}类似@tt{extend-env}和@tt{apply-env}。由于我们的语言只有单参
数过程，我们用咖喱化（练习3.20）实现等效的多参数过程。你可以把任何查询都返回0的
表作为空表。这是该模块的一个例子：

@nested[#:style 'code-inset]{
@verbatim|{
module tables
 interface
  [opaque table
   empty : table
   add-to-table : (int -> (int -> (table -> table)))
   lookup-in-table : (int -> (table -> int))]
 body
  [type table = (int -> int)
   ...]

let empty = from tables take empty
in let add-binding = from tables take add-to-table
   in let lookup = from tables take lookup-in-table
      in let table1 = (((add-binding 3) 300)
                       (((add-binding 4) 400)
                        (((add-binding 3) 600)
                         empty)))
         in -(((lookup 4) table1),
              ((lookup 3) table1))
}|
}

这个程序类型应为@tt{int}。表@tt{table1}把4绑定到400，把3绑定到300，所以程序的值
应为100。

}

@subsection[#:style section-title-style-numbered #:tag "s8.2.2"]{实现}

现在我们来扩展系统，实现透明类型和模糊类型声明，及受限类型的使用。

@subsubsection[#:style section-title-style-unumbered #:tag "syntax-and-the-interpreter"]{语法和解释器}

我们给两种新类型添加语法：有名类型（如@tt{t}）和受限类型（如@tt{from m1 take t}）。

@envalign*{\mathit{Type} &::= \mathit{Identifier} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{named-type (name)}} \\[5pt]
           \mathit{Type} &::= @tt{from @m{\mathit{Identifier}} take @m{\mathit{Identifier}}} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{qualified-type (m-name t-name)}}}

我们为模糊类型和透明类型新增两种声明。

@envalign*{\mathit{Decl} &::= @tt{opaque @m{\mathit{Identifier}}} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{opaque-type-decl (t-name)}} \\[5pt]
           \mathit{Decl} &::= @tt{transparent @m{\mathit{Identifier}} = @m{\mathit{Type}}} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{transparent-type-decl (t-name ty)}}}

我们还要新增一种定义：类型定义，用来定义模糊类型和透明类型。

@envalign*{\mathit{Defn} &::= @tt{type @m{\mathit{Identifier}} = @m{\mathit{Type}}} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{type-defn (name ty)}}}

解释器不需要查看类型和声明，所以解释器的唯一改动是忽略类型定义。

@racketblock[
@#,elem{@bold{@tt{defns-to-env}} : @${\mathit{Listof(Defn)} \times \mathit{Env} \to \mathit{Env}}}
(define defns-to-env
  (lambda (defns env)
    (if (null? defns)
      (empty-env)
      (cases definition (car defns)
        (val-defn (var exp) ...as before...)
        (type-defn (type-name type)
          (defns-to-env (cdr defns) env))))))
]

@subsubsection[#:style section-title-style-unumbered #:tag "the-checker"]{检查器}

检查器的改动就多多了，因为所有关于类型的操作都要扩展，以便处理新的类型。

首先，我们介绍处理模糊类型和透明类型的系统性方法。模糊类型就像@tt{int}或
@tt{bool}之类的原生类型一样。而透明类型名副其实，是透明的：它们的行为与定义相同。
所以每个类型都等价于下列语法：

@nested{
@$${\mathit{Type} ::= @tt{int} | @tt{bool} | @tt{from @${m} take @${t}} | @tt{(@${\mathit{Type}} -> @${\mathit{Type}})}}

其中，@${t}为@${m}中的模糊类型声明。我们称这种形式的类型为@emph{展开类型}
(@emph{expanded type})。}

接下来我们扩展类型环境，处理新类型。我们的类型环境将每个有名类型或受限类型绑定到
一个展开类型。新的类型环境定义为

@nested{
@racketblock[
(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv @#,elem{@emph{...同前...}})
  (extend-tenv-with-module @#,elem{@emph{...同前...}})
  (extend-tenv-with-type
    (t-name symbol?)
    (type type?)
    (saved-tenv type-environment?)))
]

它满足条件，@tt{type}总是一个展开类型。像@pageref{invariant}讨论的，这个条件是一
@emph{不变式}。

}

接着我们写函数@tt{expand-type}，它取一类型和一类型环境，用类型环境中绑定的类型扩
展类型参数。根据结果类型总是展开这一不变式，它在类型环境中查询有名类型和受限类型，
对@tt{proc}类型，它递归处理参数和结果类型。

@racketblock[
@#,elem{@bold{@tt{expand-type}} : @${\mathit{Type} \times \mathit{Tenv} \to \mathit{ExpandedType}}}
(define expand-type
  (lambda (ty tenv)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-type result-type)
        (proc-type
          (expand-type arg-type tenv)
          (expand-type result-type tenv)))
      (named-type (name)
        (lookup-type-name-in-tenv tenv name))
      (qualified-type (m-name t-name)
        (lookup-qualified-type-in-tenv m-name t-name tenv)))))
]

为了维持这一不变式，我们必须保证不论何时扩展类型环境，都要调用@tt{expand-type}。
这种地方有三处：

@itemlist[

 @item{在检查器中的@tt{type-of}内；}

 @item{用@tt{defns-to-decls}处理类型定义列表之处；}

 @item{在@tt{add-module-defns-to-tenv}中，向类型环境添加模块之处。}

]

在检查器中，我们把形如

@nested{
@racketblock[(extend-tenv sym ty tenv)]

的调用替换为

@racketblock[(extend-tenv var (expand-type ty tenv) tenv)]

}

在@tt{defns-to-decls}中，当我们遇到类型定义时，我们扩展定义右边，然后将其加入类
型环境中。@tt{type-of}返回的类型一定是展开的，所以我们不需要再次扩展它。当我们把
由于在模块主体中，所有类型绑定都是透明的，所以我们把类型定义转换为透明类型声明。
在@tt{add-module-defns-to-tenv}中，我们调用@tt{extend-tenv-with-module}，将接口
加入类型环境。这里，我们需要扩展接口，确保它包含的所有类型都已展开。要完成这一点，
我们修改@tt{add-module-defns-to-tenv}，调用@tt{expand-iface}。见图8.9。

过程@tt{expand-iface}（图8.10）调用@tt{expand-decls}。我们提出这些过程，为
@secref{s8.3}做准备。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{defns-to-decls}} : @${\mathit{Listof(Defn)} \times \mathit{Tenv} \to \mathit{Decl}}}
(define defns-to-decls
  (lambda (defns tenv)
    (if (null? defns)
      '()
      (cases definition (car defns)
        (val-defn (var-name exp)
          (let ((ty (type-of exp tenv)))
            (let ((new-env (extend-tenv var-name ty tenv)))
              (cons
                (val-decl var-name ty)
                (defns-to-decls (cdr defns) new-env)))))
        (type-defn (name ty)
          (let ((new-env
                  (extend-tenv-with-type
                    name (expand-type ty tenv) tenv)))
            (cons
              (transparent-type-decl name ty)
              (defns-to-decls (cdr defns) new-env))))))))

@#,elem{@bold{@tt{add-module-defns-to-tenv}} : @${\mathit{Listof(ModuleDefn)} \times \mathit{Tenv} \to \mathit{Tenv}}}
(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
      tenv
      (cases module-definition (car defns)
        (a-module-definition (m-name expected-iface m-body)
          (let ((actual-iface (interface-of m-body tenv)))
            (if (<:-iface actual-iface expected-iface tenv)
              (let ((new-env
                      (extend-tenv-with-module m-name
                        (expand-iface
                          m-name expected-iface tenv)
                        tenv)))
                (add-module-defns-to-tenv
                  (cdr defns) new-env))
              (report-module-doesnt-satisfy-iface
                m-name expected-iface actual-iface))))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "OPAQUE-TYPES的检查器，第1部分"))]
}

过程@tt{expand-decls}遍历声明的集合，创建新的类型环境，其中的每个类型和变量名都
绑定到一个展开类型。麻烦之处是声明遵循@tt{let*}式作用范围：集合中的每个声明的作
用范围包含它之后的所有声明。

要理解这些，考虑模块定义

@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [opaque t
   transparent u = int
   transparent uu = (t -> u)
   % A 处
   f : uu
   ...]
 body
  [...]
}|
}

要满足不变式，类型环境中的@tt{m1}应绑定到包含如下声明的借口

@nested[#:style 'code-inset]{
@verbatim|{
[transparent t = from m1 take t
 transparent u = int
 transparent uu = (from m1 take t -> int)
 f : (from m1 take t -> int)
 ...]
}|
}

只要我们这样做，不论何时我们从类型环境中查询类型时，得到的都是期望中的展开类型。

在A处，紧随声明@tt{f}之后，类型环境应绑定到

@nested[#:style 'code-inset]{
@tabular[#:sep @hspace[1]
         (list (list @tt{t}  @elem{绑定到} @tt{from m1 take t})
               (list @tt{u}  @elem{绑定到} @tt{int})
               (list @tt{uu} @elem{绑定到} @tt{(from m1 take t -> int)}))]
}

我们把A处即类似位置之上的类型环境称为@emph{内部}类型环境。它作为参数传给
@tt{expand-decls}。

现在我们可以写出@tt{expand-decls}。像@tt{defns-to-decls}，这个过程只创建透明类型，
因为它的用途就是创建一种查询受限类型的数据结构。

最后，我们修改@tt{<:-decls}，处理两种新声明。我们必须处理声明集合内部的作用范围
关系。例如，如果我们比较

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{expand-iface}} : @${\mathit{Sym} \times \mathit{Iface} \times \mathit{Tenv} \to \mathit{Iface}}}
(define expand-iface
  (lambda (m-name iface tenv)
    (cases interface iface
      (simple-iface (decls)
        (simple-iface
          (expand-decls m-name decls tenv))))))

@#,elem{@bold{@tt{expand-decls}} : @${\mathit{Sym} \times \mathit{Listof(Decl)} \times \mathit{Tenv} \to \mathit{Listof(Decl)}}}
(define expand-decls
  (lambda (m-name decls internal-tenv)
    (if (null? decls) ()
      (cases declaration (car decls)
        (opaque-type-decl (t-name)
          (let ((expanded-type
                  (qualified-type m-name t-name)))
            (let ((new-env
                    (extend-tenv-with-type
                      t-name expanded-type internal-tenv)))
              (cons
                (transparent-type-decl t-name expanded-type)
                (expand-decls
                  m-name (cdr decls) new-env)))))
        (transparent-type-decl (t-name ty)
          (let ((expanded-type
                  (expand-type ty internal-tenv)))
            (let ((new-env
                    (extend-tenv-with-type
                      t-name expanded-type internal-tenv)))
              (cons
                (transparent-type-decl t-name expanded-type)
                (expand-decls
                  m-name (cdr decls) new-env)))))
        (val-decl (var-name ty)
          (let ((expanded-type
                  (expand-type ty internal-tenv)))
            (cons
              (val-decl var-name expanded-type)
              (expand-decls
                m-name (cdr decls) internal-tenv))))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "OPAQUE-TYPES的检查器，第2部分"))]
}

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
[transparent t = int
x : bool                <:    [y : int]
y : t]
}|
}

我们处理到声明@tt{y}时，我们得知道@tt{t}指代@tt{int}类型。所以，当我们递归向下，
处理声明列表时，我们需要随之扩展类型环境，就像在@tt{expand-decls}中生成
@tt{internal-tenv}一样。我们调用@tt{extend-tenv-with-decl}处理这些，它取一声明，
根据类型环境将其展开为适当的类型（图8.11）。

}

展开时，我们总使用@tt{decls1}。欲知其原因，考虑比较

@nested[#:style 'code-inset]{
@verbatim|{
[transparent t = int           [opaque t
transparent u = (t -> t)  <:    transparent u = (t -> int)
f : (t -> u)]                   f : (t -> (int -> int))]
}|
}

这一比较应该通过，因为模块主体供应左侧的绑定时，也是右侧接口的正确实现。

比较类型@tt{u}的两个定义时，我们得知道类型@tt{t}实为@tt{int}。像上面第一个例子中
的声明@tt{t}所展示的，即使左边的声明在右边没出现，同样的技巧也适用。我们调用
@tt{expand-type}维持不变式，展开类型环境中的所有类型。@tt{extend-tenv-with-decl}
最后一句中选什么模块名无关紧要，因为受限类型支持的唯一操作是@tt{equal?}。所以用
@tt{fresh-module-name}足以保证这一受限类型是新生成的。

现在来处理关键问题：如何比较声明？仅当二者使用相同的名字（变量或类型）时，声明才
能匹配。如果一对声明名字相同，有四种匹配方式：

@itemlist[

 @item{二者均为值声明，类型匹配。}

 @item{二者均为模糊类型声明。}

 @item{二者均为透明类型声明，定义匹配。}

 @item{@tt{decl1}为透明类型声明，@tt{decl2}为模糊类型声明。例如，假设有个模块声
 明了@tt{opaque t}，主体中的定义为@tt{type t = int}。应当接受这种做法。过程
 @tt{defns-to-decls}将定义@tt{type t = int}转换为透明类型声明，所以
 @tt{add-module-defns-to-tenv}中的条件

@nested[#:style 'code-inset]{
@verbatim|{
actual-iface <: expected-iface
}|
}

 需要检查

 @$${@tt{(transparent @${t} = int)} <: @tt{(opaque @${t})}}

 是否成立。

 要接受这一模块，该条件应返回真。

 这就是说，类型已知的对象总能作为类型未知的对象，反之不然。例如，

 @$${@tt{(opaque @${t})} <: @tt{(transparent @${t} = int)}}

 不成立，因为值的类型为模糊类型时，其实际类型可能不是@tt{int}，而且满足
 @tt{opaque t}的模块可能不满足@tt{transparent t = int}。}

]

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{<:-decls}} : @${\mathit{Listof(Decl)} \times \mathit{Listof(Decl)} \times \mathit{Tenv} \to \mathit{Bool}}}
(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond
      ((null? decls2) #t)
      ((null? decls1) #f)
      (else
        (let ((name1 (decl->name (car decls1)))
              (name2 (decl->name (car decls2))))
          (if (eqv? name1 name2)
            (and
              (<:-decl
                (car decls1) (car decls2) tenv)
              (<:-decls
                (cdr decls1) (cdr decls2)
                (extend-tenv-with-decl
                  (car decls1) tenv)))
            (<:-decls
              (cdr decls1) decls2
              (extend-tenv-with-decl
                (car decls1) tenv))))))))

@#,elem{@bold{@tt{extend-tenv-with-decl}} : @${\mathit{Decl} \times \mathit{Tenv} \to \mathit{Tenv}}}
(define extend-tenv-with-decl
  (lambda (decl tenv)
    (cases declaration decl
      (val-decl (name ty) tenv)
      (transparent-type-decl (name ty)
        (extend-tenv-with-type
          name
          (expand-type ty tenv)
          tenv))
      (opaque-type-decl (name)
        (extend-tenv-with-type
          name
          (qualified-type (fresh-module-name '%unknown) name)
          tenv)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "OPAQUE-TYPES的检查器，第3部分"))]
}

这样，我们就得出图8.12中的代码。@tt{equiv-type?}的定义扩展其类型，所以，在上面那
样的例子

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
[transparent t = int x : bool y : t] <: [y : int]
}|
}

中，左边的@tt{t}展开为@tt{int}，匹配成功。
}

@exercise[#:level 1 #:tag "ex8.16"]{

用练习7.24中的语言扩展本节的系统，然后重写练习8.15，用多参数过程代替返回过程的过
程。

}

@exercise[#:level 2 #:tag "ex8.17"]{

仿照练习8.8，允许模块以不同于接口声明的顺序产生值。但是记住，定义必须遵守定界规
则，尤其是类型定义。

}

@exercise[#:level 2 #:tag "ex8.18"]{

我们代码依赖的不变式是，类型环境中的每个类型都已展开。我们在代码中多次调用
@tt{expand-type}来维持这一不变式。这就很容易因忘记调用@tt{expand-type}而破坏系统。
重构代码，减少@tt{expand-type}的调用，以便更稳定地维持不变式。

}

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{<:-decl}} : @${\mathit{Decl} \times \mathit{Decl} \times \mathit{Tenv} \to \mathit{Bool}}}
(define <:-decl
  (lambda (decl1 decl2 tenv)
    (or
      (and
        (val-decl? decl1)
        (val-decl? decl2)
        (equiv-type?
          (decl->type decl1)
          (decl->type decl2) tenv))
      (and
        (transparent-type-decl? decl1)
        (transparent-type-decl? decl2)
        (equiv-type?
          (decl->type decl1)
          (decl->type decl2) tenv))
      (and
        (transparent-type-decl? decl1)
        (opaque-type-decl? decl2))
      (and
        (opaque-type-decl? decl1)
        (opaque-type-decl? decl2)))))

@#,elem{@bold{@tt{equiv-type?}} : @${\mathit{Type} \times \mathit{Type} \times \mathit{Tenv} \to \mathit{Bool}}}
(define equiv-type?
  (lambda (ty1 ty2 tenv)
    (equal?
      (expand-type ty1 tenv)
      (expand-type ty2 tenv))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "OPAQUE-TYPES的检查器，第4部分"))]
}

@section[#:style section-title-style-numbered #:tag "s8.3"]{模块过程}

OPAQUE-TYPES中的程序有固定的依赖关系。模块@tt{m4}可能依赖@tt{m3}和@tt{m2}，
@tt{m2}依赖@tt{m1}。有时，我们说依赖关系是@emph{写死的} (@emph{hard-coded})。通
常，这种写死的依赖关系会造成糟糕的程序设计，因为这使模块难以复用。本节，我们给系
统添加名为@emph{模块过程} (@emph{module procedure})（有时又称@emph{参数化模块}
(@emph{parameterized module})）的组件，以便复用模块。我们称这种新语言为
PROC-MODULES。

@subsection[#:style section-title-style-numbered #:tag "s8.3.1"]{例子}

再来看我们的三位开发者。查理想用爱丽丝模块的某些组件。但爱丽丝的模块使用了鲍伯模
块提供的数据库，而查理想用另一数据库，由其他模块提供（戴安娜所写）。

要实现这些，爱丽丝用模块过程重写她的代码。模块过程就像过程，但它处理的是模块，而
非表达值。在模块层面上，接口就像类型。就像CHECKED中的过程类型指定参数和结果类型，
模块过程的接口指定接口的参数类型和结果类型。

爱丽丝写出新的模块@tt{Alices-point-builder}，开头为

@nested[#:style 'code-inset]{
@verbatim|{
module Alices-point-builder
 interface
  ((database : [opaque db-type
                opaque node-type
                insert-node : (node-type ->
                               (db-type -> db-type))
                ...])
   => [opaque point
       initial-point : (int -> point)
       ...])
}|
}

这套接口说的是，@tt{Alices-point-builder}是一模块过程。它取一模块为参数，该模块
输出两个类型，@tt{db-type}和@tt{node-type}，一个过程，@tt{insert-node}，可能还有
其他值。给定这个模块，@tt{Alices-point-builder}应生成一模块，生成的模块输出模糊
类型@tt{point}，过程@tt{initial-point}，还可能有些其他值。
@tt{Alices-point-builder}的接口还指定了参数的局部名字；稍后我们会看到为何需要它。

爱丽丝的新模块主体开头为

@nested[#:style 'code-inset]{
@verbatim|{
body
 module-proc (m : [opaque db-type
                   opaque node-type
                   insert-node : (node-type ->
                                  (db-type -> db-type))
                   ...])
  [type point = ...
   initial-point = ... from m take insert-node ...
   ...]
}|
}

一般的过程表达式形如

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
proc (|@${var} : |@${t}) |@${e}
}|
}

类似地，模块过程形如

@nested[#:style 'code-inset]{
@verbatim|{
module-proc (|@${m} : [...]) [...]
}|
}

}

本例中，爱丽丝选择@tt{m}作为模块过程中绑定变量的名字；它不必和接口中的局部名字相
同。我们重写参数中的接口，因为模块接口的作用范围不包含模块主体。可以弥补这一点
（见练习8.27）。

现在，爱丽丝把她的模块重写为

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module Alices-points
 interface
  [opaque point
  initial-point : (int -> point)
  ...]
 body
  (Alices-point-builder Bobs-db-module)
}|
}

查理的模块写成

@nested[#:style 'code-inset]{
@verbatim|{
module Charlies-points
 interface
  [opaque point
   initial-point : (int -> point)
   ...]
 body
  (Alices-point-builder Dianas-db-module)
}|
}

}

模块@tt{Alices-points}使用@tt{Bobs-db-module}的数据库。模块@tt{Charlies-points}
使用@tt{Dianas-db-module}的数据库。这样安排可以复用@tt{Alices-point-builder}中的
代码。这不仅避免了写两次同样的代码，而且，在代码需要变动时，可以只改一处，自动传
播到@tt{Alices-point}和@tt{Charlies-points}。

另一个例子，来看例8.11和8.12。在这两个例子中，我们用基本相同的代码写@tt{to-int}。
在例8.11中它是

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
letrec int to-int (x : from ints1 take t)
            = if (z? x)
              then 0
              else -((to-int (p x)), -1)
}|
}

在例8.12中，@tt{x}的类型是@tt{from ints2 take t}。所以我们将其重写为模块过程，其
参数模块产生所需的整数。

}

@nested[#:style eopl-example]{
声明

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module to-int-maker
 interface
  ((ints : [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)])
    => [to-int : (from ints take t -> int)])
 body
  module-proc (ints : [opaque t
               zero : t
               succ : (t -> t)
               pred : (t -> t)
               is-zero : (t -> bool)])
   [to-int
     = let z? = from ints take is-zero
       in let p = from ints take pred
       in letrec int to-int (x : from ints take t)
                      = if (z? x)
                        then 0
                        else -((to-int (p x)), -1)
       in to-int]
}|
}

定义了一个模块过程。这套接口说的是，该模块取模块@tt{ints}，生成另一模块。
@tt{ints}实现算数操作的接口，生成的模块输出过程@tt{to-int}，将@tt{ints}中的类型
@tt{t}转换为整数。得出的过程@tt{to-int}不能依赖算数操作的实现，因为我们根本不知
道实现是什么！这段代码中，@tt{ints}声明了两次：一次在接口中，一次在主体中。像我
们之前说的，这是因为接口中声明的作用范围局限于接口，不包含模块主体。

}
}

@nested[#:style eopl-example]{

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module to-int-maker |@emph{...同前...}

module ints1 |@emph{...同前...}

module ints1-to-int
 interface [to-int : (from ints1 take t -> int)]
 body
  (to-int-maker ints1)

let two1 = (from ints1 take succ
            (from ints1 take succ
             from ints1 take zero))
in (from ints1-to-int take to-int
    two1)
}|
}

类型为@tt{int}，值为2。因为我们首先定义了模块@tt{to-int-maker}和@tt{ints1}。然后
我们用@tt{ints1}调用@tt{to-int-maker}，得到模块@tt{ints1-to-int}，它输出绑定
@tt{from ints1-to-int take to-int}。

}
}

下面这个例子两次使用@tt{to-int-maker}，处理两种不同的算数操作实现。

@nested[#:style eopl-example]{

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module to-int-maker |@emph{...同前...}

module ints1 |@emph{...同前...}

module ints2 |@emph{...同前...}

module ints1-to-int
interface [to-int : (from ints1 take t -> int)]
body (to-int-maker ints1)

module ints2-to-int
interface [to-int : (from ints2 take t -> int)]
body (to-int-maker ints2)

let s1 = from ints1 take succ
in let z1 = from ints1 take zero
in let to-ints1 = from ints1-to-int take to-int

in let s2 = from ints2 take succ
in let z2 = from ints2 take zero
in let to-ints2 = from ints2-to-int take to-int

in let two1 = (s1 (s1 z1))
in let two2 = (s2 (s2 z2))
in -((to-ints1 two1), (to-ints2 two2))
}|
}

类型为@tt{int}，值为0。若我们用@tt{(to-ints2 two1)}替换@tt{(to-ints2 two2)}，则
程序类型异常，因为@tt{to-ints2}期望的参数类型是@tt{int2}表示的算数，但值
@tt{two1}的类型是@tt{int1}表示的算数。

}
}

@exercise[#:level 1 #:tag "ex8.19"]{

例8.16中，创建@tt{two1}和@tt{two2}的代码重复，因此可以抽象出来。完成模块定义

@nested[#:style 'code-inset]{
@verbatim|{
module from-int-maker
 interface
  ((ints : [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)])
    => [from-int : (int -> from ints take t)])
 body
  ...
}|
}

将整数表达值转换为模块@tt{ints}中的表示。用你的模块重做例8.16中的计算。用大于2的
参数测试。

}

@exercise[#:level 1 #:tag "ex8.20"]{

完成模块定义

@nested[#:style 'code-inset]{
@verbatim|{
module sum-prod-maker
 interface
  ((ints : [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)])
    => [plus : (from ints take t
                -> (from ints take t
                    -> from ints take t))
        times : (from ints take t
                 -> (from ints take t
                     -> from ints take t))])
 body
  [plus = ...
   times = ...]
}|
}

定义一个模块过程，它取一算数操作的实现，返回这种实现的求和和求积过程。用@pageref{plus}
中的@tt{plus}定义，以及类似的@tt{times}定义。

}

@exercise[#:level 1 #:tag "ex8.21"]{

写一个模块过程，取算数操作的实现@tt{ints}，返回算数操作的另一种实现，其中，整数
@${k}以@tt{ints}中的@${2*k}表示。

}

@exercise[#:level 1 #:tag "ex8.22"]{

完成模块定义

@nested[#:style 'code-inset]{
@verbatim|{
module equality-maker
 interface
  ((ints : [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)])
    => [equal : (from ints take t
                 -> (from ints take t
                     -> bool))])
body
 ...
}|
}

定义一个模块过程，它取一算数操作的实现，返回一过程，对这种实现做等值比较。

}

@exercise[#:level 1 #:tag "ex8.23"]{

写出模块@tt{table-of}，它与练习8.15中的@tt{table}模块类似，只是将表的内容参数化，
这样就能用

@nested[#:style 'code-inset]{
@verbatim|{
module mybool-tables
 interface
  [opaque table
   empty : table
   add-to-table : (int ->
                   (from mybool take t ->
                    (table -> table)))
   lookup-in-table : (int ->
                      (table ->
                       from mybool take t))]
 body
  (table-of mybool)
}|
}

定义包含@tt{from mybool take t}类型值的表。

}

@subsection[#:style section-title-style-numbered #:tag "s8.3.2"]{实现}

@subsubsection[#:style section-title-style-unumbered #:tag "s8.3-syntax"]{语法}

给我们的语言添加模块过程很像添加过程。模块过程的接口很像@tt{proc}的类型。

@envalign*{
           \mathit{Iface} &::= @tt{((@m{\mathit{Identifier}} : @m{\mathit{Iface}})) => @m{\mathit{Iface}}} \\[-3pt]
        &\mathrel{\phantom{::=}} \fbox{@tt{proc-iface (param-name param-iface result-iface)}}
            }

虽然这套接口看起来像是普通的过程类型，它还是有两点不同。首先，它描述了模块值到模
块值的函数，而非表达值到表达值的函数。第二，不像过程类型，它要给函数的输入命名。
必须如此，因为输出的接口可能依赖于输入的值，就像@tt{to-int-maker}的类型那样：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
((ints : [opaque t
          zero : t
          succ : (t -> t)
          pred : (t -> t)
          is-zero : (t -> bool)])
  => [to-int : (from ints take t -> int)])
}|
}

@tt{to-int-maker}取一模块@tt{ints}，返回一模块，其类型不仅依赖@tt{ints}中的固定
类型，也依赖@tt{ints}本身。@elemtag["module-proc-eg"]{}当我们像例8.16中那样用
@tt{ints1}调用@tt{to-int-maker}时，得到的模块接口是

@nested[#:style 'code-inset]{
@verbatim|{
[to-int : (from ints1 take t -> int)]
}|
}

而当我们用@tt{ints2}调用时，得到的是另一个接口

@nested[#:style 'code-inset]{
@verbatim|{
[to-int : (from ints2 take t -> int)]
}|
}

}

我们扩展@tt{expand-iface}，处理这些新接口，按已展开处理。这么做能行，因为参数接
口和结果接口在需要自会展开。

@racketblock[
@#,elem{@bold{@tt{expand-iface}} : @${\mathit{Sym} \times \mathit{Iface} \times \mathit{Tenv} \to \mathit{Iface}}}
(define expand-iface
  (lambda (m-name iface tenv)
    (cases interface iface
      (simple-iface (decls) @#,elem{@emph{...同前...}})
      (proc-iface (param-name param-iface result-iface)
        iface))))
]

我们需要新的模块主体来创建、引用和调用模块过程。

@envalign*{
           \mathit{ModuleBody} &::= @tt{module-proc (@m{\mathit{Identifier}} : @m{\mathit{Iface}}) @m{\mathit{ModuleBody}}} \\[-3pt]
             &\mathrel{\phantom{::=}} \fbox{@tt{proc-module-body (m-name m-type m-body)}} \\[-5pt]
           \mathit{ModuleBody} &::= \mathit{Identifier} \\[-3pt]
             &\mathrel{\phantom{::=}} \fbox{@tt{var-module-body (m-name)}} \\[-5pt]
           \mathit{ModuleBody} &::= @tt{(@m{\mathit{Identifier}} @m{\mathit{Identifier}})} \\[-3pt]
             &\mathrel{\phantom{::=}} \fbox{@tt{app-module-body (rator rand)}}
            }

@subsubsection[#:style section-title-style-unumbered #:tag "s8.3-interpreter"]{解释器}

首先，类似过程，我们新加一种模块。

@racketblock[
(define-datatype typed-module typed-module?
  (simple-module
    (bindings environment?))
  (proc-module
    (b-var symbol?)
    (body module-body?)
    (saved-env environment?)))
]

我们扩展@tt{value-of-module-body}处理新的模块主体。代码类似于表达式的变量引用和
过程调用（图8.13）。

@subsubsection[#:style section-title-style-unumbered #:tag "s8.3-checker"]{检查器}

我们可以给新的模块主体写出@secref{s7.2}那样的规则。这些规则如图8.14所示。为了能
在一页纸内写下规则，我们用@tt{(@${\rhd} @${body} @${tenv}) = @${i}}代替
@tt{(interface-of @${body} @${tenv}) = @${i}}。

模块变量的类型从类型环境中取得，与期望相符。@tt{module-proc}的类型由参数类型和主
体类型得到，正像CHECKED中的过程。

模块过程的调用很像CHECKED中的过程调用。但有两点重要不同。

首先，操作数的类型（规则IFACE-M-APP中的@${i_2}）不必与参数类型 (@${i_1})相同。我
们只要求@${i_2 <: i_1}。这就够了，因为@${i_2 <: i_1}表示满足接口@${i_2}的任何模
块也满足接口@${i_1}，因此，能作为模块过程的参数。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{value-of-module-body}} : @${\mathit{ModuleBody} \times \mathit{Env} \to \mathit{TypedModule}}}
(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
      (defns-module-body (defns) ...as before...)
      (var-module-body (m-name)
        (lookup-module-name-in-env m-name env))
      (proc-module-body (m-name m-type m-body)
        (proc-module m-name m-body env))
      (app-module-body (rator rand)
        (let ((rator-val
                (lookup-module-name-in-env rator env))
               (rand-val
                 (lookup-module-name-in-env rand env)))
          (cases typed-module rator-val
            (proc-module (m-name m-body env)
              (value-of-module-body m-body
                (extend-env-with-module
                  m-name rand-val env)))
            (else
              (report-bad-module-app rator-val))))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para @tt{value-of-module-body}))]
}

@nested[#:style eopl-figure]{
@$${\begin{array}{l}
     \small{\textrm{IFACE-M-VAR}} \\
     @tt{(@${\rhd} @${m} @${tenv})} = tenv@tt{(@${m})}
    \end{array}}

@$${\begin{array}{l}
     \small{\textrm{IFACE-M-PROC}} \\
     \infer{@tt{(@${\rhd} (m-proc (@${m}:@${i_1}) @${body}))} = @tt{((@${m}:@${i_1}) => @${i^{\prime}_{1}})}}
           {@tt{(@${\rhd} body [@${m}=@${i_1}]tenv)} = i^{\prime}}
    \end{array}}

@$${\begin{array}{l}
     \small{\textrm{IFACE-M-APP}} \\
     \infer{@tt{(@${\rhd} (@${m_1} @${m_2}) tenv)} = @tt{i((@${m}:@${i_1}) => @${i^{\prime}_{1}})}}
           {\begin{array}{c}
             tenv(m_1) = @tt{((@${m}:@${i_1}) => @${i^{\prime}_{1}})} \quad tenv(@${m_2}) = @${i_2} \\
             i_2 <: i_1
            \end{array}}
    \end{array}}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "新模块主体的判类规则"))]
}

其次，在结果类型@${t'{_1}}中，我们把@${m}代换为操作数@${m_2}。
考虑@pageref{module-proc-eg}的例子。其中，我们用@tt{ints1}和@tt{ints2}调用模块过
程@tt{to-int-maker}，其接口为

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
((ints : [opaque t
          zero : t
          succ : (t -> t)
          pred : (t -> t)
          is-zero : (t -> bool)])
  => [to-int : (from ints take t -> int)])
}|
}

当我们用@tt{ints1}调用@tt{to-int-maker}时，代换后的接口是

@nested[#:style 'code-inset]{
@verbatim|{
[to-int : (from ints1 take t -> int)]
}|
}

当我们用@tt{ints2}调用@tt{to-int-maker}时，代换后的接口是

@nested[#:style 'code-inset]{
@verbatim|{
[to-int : (from ints2 take t -> int)]
}|
}

正合期望。

}

用这些规则，很容易写出@tt{interface-of}的代码（图8.15）。当我们检查
@tt{module-proc}的主体时，我们把参数添加到类型环境中，就好像它是一个顶层模块。这
段代码用过程@tt{rename-in-iface}对得到的接口进行代换。

最后，我们扩展@tt{<:-iface}，处理新的类型。用来比较@tt{proc-iface}的规则是

@$${\infer{@tt{((@${m_1} : @${i_1}) => @${i^{\prime}_{1}})} <: @tt{((@${m_2} : @${i_2}) => @${i^{\prime}_{2}})}}
          {i_2 <: i_1 &
           i^{\prime}_{1}@tt{[@${m^{\prime}/m_{1}}]} <: i^{\prime}_{2}@tt{[@${m^{\prime}/m_{2}}]} &
           m^{\prime} 不在 i^{\prime}_{1} 或 i^{\prime}_{2} 中}}

要使@tt{((@${m_1} : @${i_1}) => @${i^{\prime}_{1}})}，满足第一个接口的模块
@${m_0}必须满足第二个接口。这就是说，接口为@${i_2}的任何模块都能传给@${m_0}作参
数，@${m_0}产生的任何模块都满足@${i^{\prime}_{2}}。

为满足第一个要求，我们要@${i_2 <: i_1}。这保证了满足@${i_2}的任何模块都能作为参
数传给@${m_0}。注意逆序：在参数类型中，我们说@emph{子类型判定}
(@emph{subtyping})是@emph{逆变的} (@emph{contravariant})。

结果的类型呢？我们可以要求@${i^{\prime}_{1} <: i^{\prime}_{2}}。不幸的是，这行不
通。@${i^{\prime}_{1}}中，可能出现模块变量@${m_1}，@${i^{\prime}_{2}}中，可能出
现模块变量@${m_2}的实例。所以，要比较它们，我们得将@${m_1}和@${m_2}重命名为新的
模块变量@${m^{\prime}}。一旦如此，我们就能照常比较它们了。这就得出条件
@${i^{\prime}_{1}@tt{[@${m^{\prime}/m_{1}}]} <:
i^{\prime}_{2}@tt{[@${m^{\prime}/m_{2}}]}}。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{interface-of}} : @${\mathit{ModuleBody} \times \mathit{Tenv} \to \mathit{Iface}}}
(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
      (var-module-body (m-name)
        (lookup-module-name-in-tenv tenv m-name))
      (defns-module-body (defns)
        (simple-iface
          (defns-to-decls defns tenv)))
      (app-module-body (rator-id rand-id)
        (let ((rator-iface
                (lookup-module-name-in-tenv tenv rator-id))
               (rand-iface
                 (lookup-module-name-in-tenv tenv rand-id)))
          (cases interface rator-iface
            (simple-iface (decls)
              (report-attempt-to-apply-simple-module rator-id))
            (proc-iface (param-name param-iface result-iface)
              (if (<:-iface rand-iface param-iface tenv)
                (rename-in-iface
                  result-iface param-name rand-id)
                (report-bad-module-application-error
                  param-iface rand-iface m-body))))))
      (proc-module-body (rand-name rand-iface m-body)
        (let ((body-iface
                (interface-of m-body
                  (extend-tenv-with-module rand-name
                    (expand-iface rand-name rand-iface tenv)
                    tenv))))
          (proc-iface rand-name rand-iface body-iface))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "PROC-MODULES的检查器，第1部分"))]
}

判断这种关系的代码较为直接（图8.16）。判断
@${i^{\prime}_{1}@tt{[@${m^{\prime}/m_{1}}]} <:
i^{\prime}_{2}@tt{[@${m^{\prime}/m_{2}}]}}时，我们扩展类型环境，给@${m^{\prime}}
添加绑定。我们将@${m^{\prime}}与@${i_1}关联起来，因为它比@${i_2}小。我们调用
@tt{extend-tenv-with-module}比较结果的类型时，要调用@tt{expand-iface}维持不变式。

现在，完成了。吃杯圣代吧，放些佐料，有满足奶油接口的，有满足热浇汁接口的，还有满
足坚果接口的。怎么混合不要紧，好吃就行！

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{<:-iface}} : @${\mathit{Iface} \times \mathit{Iface} \times \mathit{Tenv} \to \mathit{Bool}}}
(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases interface iface1
      (simple-iface (decls1)
        (cases interface iface2
          (simple-iface (decls2)
            (<:-decls decls1 decls2 tenv))
          (proc-iface (param-name2 param-iface2 result-iface2)
            #f)))
      (proc-iface (param-name1 param-iface1 result-iface1)
        (cases interface iface2
          (simple-iface (decls2) #f)
          (proc-iface (param-name2 param-iface2 result-iface2)
            (let ((new-name (fresh-module-name param-name1)))
              (let ((result-iface1
                      (rename-in-iface
                        result-iface1 param-name1 new-name))
                     (result-iface2
                       (rename-in-iface
                         result-iface2 param-name2 new-name)))
                (and
                  (<:-iface param-iface2 param-iface1 tenv)
                  (<:-iface result-iface1 result-iface2
                    (extend-tenv-with-module
                      new-name
                      (expand-iface new-name param-iface1 tenv)
                      tenv)))))))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "PROC-MODULES的检查器，第2部分"))]
}

@exercise[#:level 1 #:tag "ex8.24"]{

调用模块时，只能用标识符。要是我们想根据类型规则检查调用@tt{(m1 (m2 m3))}，会有
什么问题？

}

@exercise[#:level 1 #:tag "ex8.25"]{

扩展PROC-MODULES，像练习3.21那样，允许模块取多个参数。

}

@exercise[#:level 2 #:tag "ex8.26"]{

扩展语言的模块主体，将模块调用的生成式改为

@envalign*{
           \mathit{ModuleBody} &::= @tt{(@m{\mathit{ModuleBody}} @m{\mathit{ModuleBody}})} \\[-3pt]
             &\mathrel{\phantom{::=}} \fbox{@tt{app-module-body (rator rand)}}
            }
}

@exercise[#:level 3 #:tag "ex8.27"]{

在PROC-MODULES中，我们总要一遍又一遍写这种接口

@nested[#:style 'code-inset]{
@verbatim|{
[opaque t
 zero : t
 succ : (t -> t)
 pred : (t -> t)
 is-zero : (t -> bool)]
}|
}

给程序添加语法，支持有名接口，这样我们就能写

@nested[#:style 'code-inset]{
@verbatim|{
interface int-interface = [opaque t
                           zero : t
                           succ : (t -> t)
                           pred : (t -> t)
                           is-zero : (t -> bool)]
module make-to-int
 interface
  ((ints : int-interface)
    => [to-int : from ints take t -> int])
 body
  ...
}|
}

}
