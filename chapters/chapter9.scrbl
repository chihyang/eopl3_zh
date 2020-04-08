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

@title[#:style 'numbered #:tag "oac"]{对象和类}

许多编程任务都需要程序通过接口管理某些状态。例如，文件系统有内部状态，但访问和修
改那一状态只能通过文件系统的接口。状态常常涉及多个变量，为了维护状态的一致性，必
须协同修改那些变量。因此，需要某种技术，确保组成状态的多个变量能协同更新。
@emph{面向对象编程} (@emph{Object-oriented programming})正是用来完成此任务的技术。

在面向对象编程中，每种受管理的状态称为一个@emph{对象} (@emph{object})。一个对象
中存有多个量，称为@emph{字段} (@emph{field})；有多个相关过程，称为@emph{方法}
(@emph{method})，方法能够访问字段。调用方法常被视为将方法名和参数当作消息传给对
象；有时，又说这是从@emph{消息传递} (@emph{message-psasing})的视角看待面向对象编
程。

在@secref{state}那样的有状态语言中，过程就是用对象编程的绝佳示例。过程是一种对象，
其状态包含在自由变量之中。闭包只有一种行为：用某些参数调用它。例如，
@elem[#:style question]{105页}的@tt{g}控制计数器的状态，对这一状态，唯一能做的就
是将其递增。但是，更常见的是让一个对象具有多种行为。面向对象编程语言提供这种功能。

同一方法常常需要管理多重状态，例如多个文件系统或程序中的多个队列。为便于方法共享，
面向对象编程系统通常提供名为@emph{类} (@emph{class})的结构，用来指定某种对象的字
段及方法。每个对象都创建为类的@emph{实例} (@emph{instance})。

类似地，多个类可能有相似而不相同的字段和方法。为便于共享实现，面向对象编程语言通
常提供@emph{继承} (@emph{inheritance})，允许程序员增改某些方法的行为，添加字段，
对现有类小做修改，就能定义新类。这时，由于新类的其他行为从原类继承而得，我们说新
类@emph{继承于} (@emph{inherit from})或@emph{扩展} (@emph{extend})旧类。

不论程序元素是在建模真实世界中的对象还是人为层面的系统状态，都要弄清楚，程序结构
能否由结合行为和状态的对象组成。将行为类似的对象与同一个类关联起来，也是自然而然
的。

真实世界中的对象通常具有某种@emph{状态}和@emph{行为}，后者要么控制前者，要么受前
者控制。例如，猫能吃，打呼噜，跳和躺下，这些活动都由猫当前的状态控制，包括它们有
多饿，有多累。

对象和模块颇多相似，但又截然不同。模块和类都提供了定义模糊类型的机制。但对象是一
种有行为的数据结构，模块只是一组绑定。同一个类可以有很多个对象；大多数模块系统没
有提供相仿的能力。但是，PROC-MODULES这样的模块系统提供了更为灵活的方式来控制名字
的可见性。模块和类可以相得益彰。

@section[#:tag "s9.1"]{面向对象编程}

@section[#:tag "s9.2"]{继承}

@section[#:tag "s9.3"]{语言}

@section[#:tag "s9.4"]{解释器}

@section[#:tag "s9.5"]{有类型的语言}

@section[#:tag "s9.6"]{类型检查器}
