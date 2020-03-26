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
