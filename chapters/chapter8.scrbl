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

@title[#:style 'numbered #:tag "modules"]{模块}

对只有几百行代码的系统，我们介绍的语言特性已非常强大。如果我们要设计更大的系统，
有数千行代码，我们就还需要一些别的佐料。

@itemlist[#:style 'ordered

 @item{我们需要一种好办法，将系统分为相对独立的部分，并能说明各部分之间的依赖关
 系。}

 @item{我们需要一种更好的方式来控制名字的作用范围和绑定。词法定界是控制命名的有
 效工具，但当程序更大或有多个来源时还是不够。}

 @item{我们需要一种方式加强抽象边界。在@secref{da}，我们介绍了抽象数据类型的思想。
 在类型的实现中，类型的值只能通过类型接口中的过程创建和操作。我们把这叫做
 @emph{抽象边界} (@emph{abstraction boundary})。如果程序尊重这种界限，我们可以改
 变数据类型的实现。但是，如果某些代码打破了抽象，依赖实现细节，那么我们就无法任
 意修改实现而不破坏其他代码。}

 @item{最后，我们需要一种方式，将这些部分灵活组合，那么同一部分可复用于不同地方。}

]

本章，我们介绍@emph{模块} (@emph{module})，以满足这些需求。具体来说，我们展示了
如何用类型系统创建和强化抽象边界。

我们的模块语言中，程序包含一系列@emph{模块定义} (@emph{module definition})，后跟
待求值的表达式。每个模块定义把一个名字绑定到一个@emph{模块}。创建的模块可能是
@emph{简单模块} (@emph{simple module})，即类似环境的一些绑定；也可能是@emph{模块
过程} (@emph{module procedure})，取一模块，生成另一模块。

每个模块都有一套@emph{接口} (@emph{interface})。简单模块具有@emph{简单接口}
(@emph{simple interface})，接口列出模块提供的绑定及其类型。模块过程的接口指定参
数模块和返回模块的接口，就像过程的类型指明参数和结果的类型。

这些接口就像类型一样，决定了模块如何组合。因为求出示例程序的值非常简单，因此我们
强调其类型。如前所见，理解这些语言的定界和绑定规则是程序分析和求值的关键。

@section[#:tag "s8.1"]{简单模块系统}

我们的第一种语言名叫SIMPLE-MODULES，只有简单模块。它没有模块过程，只创建非常简单
的抽象边界。几种流行语言使用与之类似的模块系统。

@subsection[#:tag "s8.1.1"]{例子}

设想一个软件项目中有三名开发者：爱丽丝，鲍伯和查理。爱丽丝、鲍伯和查理正在开发项
目中相对独立的几部分。这些开发者散居各处，时区都可能不同。项目的每部分都要实现一
套@secref{s2.1}那样的接口，但接口的实现可能涉及大量其他过程。而且，开发者们需要
确保没有命名冲突，当个部分集成到一起时，不会干扰项目的其他部分。

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
@tt{m1::a}。在第9章探讨的面向对象语言中，@tt{m1.a}常表示其他内容。

我们说接口@emph{提出} (@emph{offer})（或称@emph{公布} (@emph{advertise})，或称
@emph{承诺} (@emph{promise})）三个整型值，主体@emph{供应}（@emph{supply}或
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

@subsection[#:tag "s8.1.2"]{实现简单模块系统}

@subsubsection[#:style 'unnumbered #:tag "s8.1.2.1"]{语法}

SIMPLE-MODULES的程序包含一串模块定义，然后是一个表达式。

@envalign*{
           \mathit{Program} &::= \{\mathit{ModuleDefn}\}^{*} \mathit{Expression} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{a-program (m-defs body)}}}

模块定义包含名字、接口和主体。

@envalign*{
           \mathit{ModuleDefn} &::= @tt{module} \mathit{Identifier} @tt{interface} \mathit{Iface} @tt{body} \mathit{ModuleBody} \\[-3pt]
            &\mathrel{\phantom{::=}} \fbox{@tt{a-module-definition (m-name expected-iface m-body)}}}

简单模块的接口包含任意数量的声明。每个声明指定程序中一个变量的类型。我们称之为
@emph{值声明} (@emph{value declaration})，因为要声明的变量表示一个值。在后面几节
中，我们介绍其他种类的接口和声明。

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

@subsubsection[#:style 'unnumbered #:tag "s8.1.2.2"]{解释器}

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

@subsubsection[#:style 'unnumbered #:tag "s8.1.2.3"]{检查器}

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
  (extend-tenv ...as before...)
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
      ’()
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

@section[#:tag "s8.2"]{声明类型的模块}
