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

本章，我们研究一种简单的面向对象编程语言，名为CLASSES。CLASSES程序包含一些类声明，
然后是一个可能用到那些类的表达式。

图9.1展示了这种语言的一个简单程序。它定义了继承于@tt{object}的类@tt{c1}。类
@tt{c1}的每个对象都包含两个字段，名为@tt{i}和@tt{j}。字段叫做@emph{成员}
(@emph{member})或@emph{实例变量} (@emph{instance variable})。类@tt{c1}支持三个
@emph{方法}或@emph{成员函数} (@emph{member function})，名为 @tt{initialize}、
@tt{countup}和@tt{getstate}。每个方法包含@emph{方法名} (@emph{method name})，若
干@emph{方法变量} (@emph{method var})（又称 @emph{方法参数} (@emph{method
parameters})），以及 @emph{方法主体} (@emph{method body})。方法名对应于@tt{c1}示
例能够响应的@emph{消息}种类。有时，我们说成是“@tt{c1}的方法@tt{countup}”。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
class c1 extends object
 field i
 field j
 method initialize (x)
  begin
   set i = x;
   set j = -(0,x)
  end
 method countup (d)
  begin
   set i = +(i,d);
   set j = -(j,d)
  end
 method getstate () list(i,j)
let t1 = 0
    t2 = 0
    o1 = new c1(3)
in begin
    set t1 = send o1 getstate();
    send o1 countup(2);
    set t2 = send o1 getstate();
    list(t1,t2)
   end
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "简单的面向对象程序"))]
}


本例中，类的每个方法都维护完整性约束或@emph{不变式}@${i = -j}。当然，现实中程序
例子的完整性约束可能复杂得多。

图9.1中的程序首先初始化三个变量。@tt{t1}和@tt{t2}初始化为0。@tt{o1}初始化为
@tt{c1}的一个对象。我们说这个对象是类@tt{c1}的一个@emph{实例}。对象通过操作
@tt{new}创建。它会触发调用类的方法@tt{initialize}，在本例中，是将对象的字段
@tt{i}设置为3，字段@tt{j}设置为-3。然后，程序调用@tt{o1}的方法@tt{getstate}，返
回列表@tt{(3 -3)}。接着，它调用@tt{o1}的方法@tt{countup}，将两个字段的值改为5和-5。
然后再次调用@tt{getstate}，返回@tt{(5 -5pt)}。最后，值@tt{list(t1,t2)}，即
@tt{((3 -3) (5 -5))}成为整段程序的返回值。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
class interior-node extends object
 field left
 field right
 method initialize (l, r)
  begin
   set left = l;
   set right = r
  end
 method sum () +(send left sum(),send right sum())
class leaf-node extends object
 field value
 method initialize (v) set value = v
 method sum () value
let o1 = new interior-node(
          new interior-node(
           new leaf-node(3),
           new leaf-node(4)),
          new leaf-node(5))
in send o1 sum()
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "求树叶之和的面向对象程序"))]
}

图9.2解释了面向对象编程中的关键思想：@emph{动态分发} (@emph{dynamic dispatch})。
在这段程序中，我们的树有两种节点，@tt{interior-node}和@tt{leaf-node}。通常，我们
不知道是在给哪种节点发消息。相反，每个节点接受@tt{sum}消息，并用自身的@tt{sum}方
法做适当操作。这叫做@emph{动态分发}。这里，表达式生成一棵树，有两个内部节点，三
个叶子节点。它将@tt{sum}消息发给节点@tt{o1}；@tt{o1}将@tt{sum}消息发给子树，依此
类推，最终返回12。这段程序也展示了，所有方法都是互递归的。

方法主体可通过标识符@tt{self}（有时叫做@tt{this}）调用同一对象的其他方法，
@tt{self}总是绑定于方法调用时的对象。例如，在

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
class oddeven extends object
 method initialize () 1
 method even (n)
  if zero?(n) then 1 else send self odd(-(n,1))
 method odd (n)
  if zero?(n) then 0 else send self even(-(n,1))
let o1 = new oddeven()
in send o1 odd(13)}|
}

中，方法@tt{even}和@tt{odd}递归调用彼此，因为它们执行时，@tt{self}绑定到包含二者
的对象。这就像练习3.37中，用动态绑定实现递归。
}

@section[#:tag "s9.2"]{继承}

@section[#:tag "s9.3"]{语言}

@section[#:tag "s9.4"]{解释器}

@section[#:tag "s9.5"]{有类型的语言}

@section[#:tag "s9.6"]{类型检查器}
