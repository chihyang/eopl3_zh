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
parameters})），以及 @emph{方法主体} (@emph{method body})。方法名对应于@tt{c1}的
实例能够响应的@emph{消息}种类。有时，我们说成是“@tt{c1}的方法@tt{countup}”。

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

通过继承，程序员能够逐步修改旧类，得到新类。在实践中，这十分有用。例如，有颜色的
点类似一个点，但是它还有处理颜色的方法，如图9.3中的经典例子所示。

如果类@${c_2}扩展类@${c_1}，我们说@${c_1}是@${c_2}的@emph{父类} (@emph{parent})
或@emph{超类} (@emph{superclass})，@${c_2}是@${c_1}的@emph{子类} (@emph{child})。
继承时，由于@${c_2}定义为@${c_1}的扩展，@${c_1}必须在@${c_2}之前定义。在此之前，
语言包含了一个预先定义的类，名为@tt{object}，它没有任何方法或字段。由于类
@tt{object}没有@tt{initialize}方法，因此无法创建它的对象。除@tt{object}之外的所
有类都有唯一父类，但可以有许多子类。因此，由@tt{extends}得出的关系在类与类之间产
生了树状结构，其根为@tt{object}。因为每个类至多只有一个直接超类，这是一种
@emph{单继承} (@emph{single-inheritance})语言。有些语言允许类继承自多个超类。
@emph{多继承} (@emph{multiple inheritance})虽然强大，却不无问题。在练习中，我们
考虑一些困难之处。

术语@emph{继承}源于对宗谱的类比。我们常常引申这一类比，说类的@emph{祖先}
(@emph{ancestor})（从类的父类到根部的类@tt{object}）和@emph{后代}
(@emph{descendant})。如果@${c_2}是@${c_1}的后代，我们有时说@${c_2}是@${c_1}的
@emph{子类} (@emph{subclass})，写作@${c_2 < c_1}。

如果类@${c_2}继承自类@${c_1}，@${c_1}的所有字段和方法都对@${c_2}的方法可见，除非
在@${c_2}中重新声明它们。由于一个类继承了父类的所有方法和字段，子类的实例可以在
任何能够使用父类实例的地方使用。类似地，类后代的实例可以在任何能够使用类实例的地
方使用。有时，这叫做@emph{子类多态} (@emph{subclass polymorphism})。我们的语言选
择这种设计，其他面向对象语言可能有不同的可见性规则。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
class point extends object
 field x
 field y
 method initialize (initx, inity)
  begin
   set x = initx;
   set y = inity
  end
 method move (dx, dy)
  begin
   set x = +(x,dx);
   set y = +(y,dy)
  end
 method get-location () list(x,y)
class colorpoint extends point
 field color
 method set-color (c) set color = c
 method get-color () color
let p = new point(3,4)
    cp = new colorpoint(10, 20)
in begin
    send p move(3,4);
    send cp set-color(87);
    send cp move(10,20);
    list(send p get-location(),   % |@emph{返回} (6 8)
         send cp get-location(),  % |@emph{返回} (20 40)
         send cp get-color())     % |@emph{返回} 87
   end
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "继承的经典例子：" @tt{colorpoint}))]
}

接下来，我们看看重新声明类的字段或方法时会发生什么。如果@${c_1}的子读在子类
@${c_2}中重新声明，新的声明@emph{遮蔽} (@emph{shadow})旧的，就像词法定界一样。例
如，考虑图9.4。类@tt{c2}的对象有两个名为@tt{y}的字段：@tt{c1}中声明的和@tt{c2}中
声明的。@tt{c1}中声明的方法能看到@tt{c1}的字段@tt{x}和@tt{y}。在@tt{c2}中，
@tt{getx2}中的@tt{x}指代@tt{c1}的字段@tt{x}，但@tt{gety2}中的@tt{y}指代@tt{c1}的
字段@tt{y}。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
class c1 extends object
 field x
 field y
 method initialize () 1
 method setx1 (v) set x = v
 method sety1 (v) set y = v
 method getx1 () x
 method gety1 () y
class c2 extends c1
 field y
 method sety2 (v) set y = v
 method getx2 () x
 method gety2 () y
let o2 = new c2()
in begin
    send o2 setx1(101);
    send o2 sety1(102);
    send o2 sety2(999);
    list(send o2 getx1(),  % |@emph{返回} 101
         send o2 gety1(),  % |@emph{返回} 102
         send o2 getx2(),  % |@emph{返回} 101
         send o2 gety2())  % |@emph{返回} 999
   end
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "字段遮蔽的例子"))]
}

如果类@${c_1}的方法@${m}在某个子类@${c_2}中重新声明，我们说新的方法@emph{覆盖}
(@emph{override})旧的方法。我们将方法声明所在的类称为方法的@emph{持有类}
(@emph{host class})。同样地，我们将表达式的持有类定义为表达式所在方法（如果有的
话）的持有类。我们还将方法或表达式的超类定义为持有类的父类。

如果给类@${c_2}的对象发送消息@${m}，应使用新的方法。这条规则很简单，其结果却很微
妙。考虑如下例子：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
class c1 extends object
 method initialize () 1
 method m1 () 11
 method m2 () send self m1()
class c2 extends c1
 method m1 () 22
let o1 = new c1() o2 = new c2()
in list(send o1 m1(), send o2 m1(), send o2 m2())
}|
}

我们希望@tt{send o1 m1()}返回11，因为@tt{o1}是@tt{c1}的实例。同样地，我们希望
@tt{send o2 m1()}返回22，因为@tt{o2}是@tt{c2}的实例。那么@tt{send o2 m2()}呢？方
法@tt{m2}只是调用方法@tt{m1}，但这是哪一个？
}

动态分发告诉我们，应查看绑定到@tt{self}的对象属于哪个类。@tt{self}的值是@tt{o2}，
属于类@tt{c2}。因此，调用@tt{send self m1()}应返回22。

我们的语言还有一个重要特性，@emph{超类调用} (@emph{super call})。考虑图9.5中的程
序。其中，我们在类@tt{colorpoint}中重写了@tt{initialize}方法，同时设置字段@tt{x}、
@tt{y}和@tt{color}。但是，新方法的主体复制了原方法的代码。在我们的小例子中，这尚
可接受，但在大型例子中，这显然是一种坏的做法。（为什么？）而且，如果
@tt{colorpoint}声明了字段@tt{x}，就没法初始化@tt{point}的字段@tt{x}，就像
@elem[#:style question]{331页}的例子中，没法初始化第一个@tt{y}一样。

解决方案是，把@tt{colorpoint}的@tt{initialize}方法主体中的重复代码替换为@emph{超
类调用}，形如@tt{super initialize()}。那么@tt{colorpoint}中的@tt{initialize}方法
写作：

@nested[#:style 'code-inset]{
@verbatim|{
method initialize (initx, inity, initcolor)
 begin
  super initialize(initx, inity);
  set color = initcolor
 end
}|
}

方法@${m}主体中的超类调用@tt{super @${n}(...)}使用的是@${m}持有类父类的方法@${n}。
这不一定是@tt{self}所指类的父类。@tt{self}所指类总是@${m}持有类的子类，但不一定
是同一个，因为@${m}可能在目标对象的某个祖先中声明。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
class point extends object
 field x
 field y
 method initialize (initx, inity)
  begin
   set x = initx;
   set y = inity
 end
 method move (dx, dy)
  begin
   set x = +(x,dx);
   set y = +(y,dy)
  end
 method get-location () list(x,y)
class colorpoint extends point
 field color
 method initialize (initx, inity, initcolor)
  begin
   set x = initx;
   set y = inity;
   set color = initcolor
  end
 method set-color (c) set color = c
 method get-color () color
let o1 = new colorpoint(3,4,172)
in send o1 get-color()}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "演示" @tt{super} "必要性的例子"))]
}

要解释这种区别，考虑图9.6。给类@tt{c3}的对象@tt{o3}发送消息@tt{m3}，找到的是
@tt{c2}的方法@tt{m3}，它执行@tt{super m1()}。@tt{o3}的类是@tt{c3}，其父类是
@tt{c2}，但方法的持有类是@tt{c2}，@tt{c2}的超类是@tt{c1}。所以，执行的是@tt{c1}
的方法@tt{m1}。这是@emph{静态方法分发} (@emph{static method dispatch})的例子。虽
然进行超类方法调用的对象是@tt{self}，方法分发却是静态的，因为要使用的方法可以从
程序文本中判断，与@tt{self}所指类无关。

本例中，@tt{c1}的方法@tt{m1}调用@tt{o3}的方法@tt{m2}。这是普通方法调用，所以使用
动态分发，找出的是@tt{c3}的方法@tt{m2}，返回33。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
class c1 extends object
 method initialize () 1
 method m1 () send self m2()
 method m2 () 13
class c2 extends c1
 method m1 () 22
 method m2 () 23
 method m3 () super m1()
class c3 extends c2
 method m1 () 32
 method m2 () 33
let o3 = new c3()
in send o3 m3()
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "解释" @tt{super} "调用与" @tt{self} "相互作用的例子"))]
}

@section[#:tag "s9.3"]{语言}

我们的语言CLASSES由IMPLICIT-REFS扩展而得，新增生成式如图9.7所示。程序中首先是一
些类声明，然后是一个待执行的表达式。类声明有名字，最接近的超类名，0个或多个字段
声明，以及0个或多个方法声明。方法声明类似@tt{letrec}中的过程声明，有个名字，一个
形参列表，以及主体。同时我们扩展语言，支持多参数过程，多声明@tt{let}和多声明
@tt{letrec}表达式，还有些其他操作，如加法和@tt{list}。列表操作同练习3.9。最后，
我们增加@tt{begin}表达式，同练习4.4，它从左到右求出子表达式的值，返回最后一个的
值。

我们新增表达值对象和列表，所以有

@nested{

@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} + \mathit{Listof(ExpVal)} + \mathit{Obj}\\
\mathit{DenVal} &= \mathit{Ref(ExpVal)}
}

我们写@${\mathit{Listof(ExpVal)}}，表示列表可以包含任何表达值。

}

我们将在@secref{s9.4.1}考察@${\mathit{Obj}}。在我们的语言中，类既不是指代值，也
不是表达值：它们作为对象的一部分，但不能做变量的绑定或表达式的值，不过，看看练习
9.29。

@nested[#:style eopl-figure]{

@envalign*{
           \mathit{Program} &::= \{\mathit{ClassDecl}\}^{*} \phantom{x} \mathit{Expression} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{a-program (class-decls body)}} \\[5pt]
         \mathit{ClassDecl} &::= @tt{class @m{\mathit{Identifier}} extends @m{\mathit{Identifier}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \phantom{x}\{@tt{field @m{\mathit{Identifier}}}\}^{*}\phantom{x}\{\mathit{MethodDecl}\}^{*} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{\begin{math}\begin{alignedat}{-1}
                                          &@tt{a-class-decl} \\
                                          &\phantom{x}@tt["("]@tt{class-name super-name} \\
                                          &\phantom{xx}@tt{field-names method-decls}@tt[")"]
                                         \end{alignedat}\end{math}} \\[5pt]
        \mathit{MethodDecl} &::= @tt{method @m{\mathit{Identifier}} (@m{\{\mathit{Identifier}\}^{*(,)}}) @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{a-method-decl (method-name vars body)}} \\[5pt]
        \mathit{Expression} &::= @tt{new @m{\mathit{Identifier}} (@m{\{\mathit{Expression}\}^{*(,)}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{new-object-exp (class-name rands)}} \\[5pt]
        \mathit{Expression} &::= @tt{send @m{\mathit{Expression}} @m{\mathit{Identifier}} (@m{\{\mathit{Expression}\}^{*(,)}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{method-call-exp (obj-exp method-name rands)}} \\[5pt]
        \mathit{Expression} &::= @tt{super @m{\mathit{Identifier}} (@m{\{\mathit{Expression}\}^{*(,)}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{super-call-exp (method-name rands)}} \\[5pt]
        \mathit{Expression} &::= @tt{self} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{self-exp}}
          }

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "简单面向对象语言中新增的生成式"))]
}

我们新增了四种表达式。@tt{new}表达式创建指定类的对象，然后调用@tt{initialize}方
法初始化对象的字段。@tt{rands}求值后，传给@tt{initialize}方法。这个方法调用的返
回值直接抛弃，新对象则作为@tt{new}表达式的值返回。

@tt{self}表达式返回当前方法操作的对象。

@tt{send}表达式包含一值为对象的表达式，一个方法名，0或多个操作数。它从对象的类中
取出指定的方法，然后求操作数的值，将实参传给该方法。就像在IMPLICIT-REFS中那样，
它要为每个实参分配一个新位置，然后将方法的形参与对应位置的引用绑定起来，并在这个
词法绑定的作用范围内求方法主体的值。

@tt{super-call}表达式包含一个方法名，0或多个参数。它从表达式持有类的超类开始，找
出指定的方法，然后以当前对象为@tt{self}，求出方法主体的值。

@section[#:tag "s9.4"]{解释器}

@subsection[#:tag "s9.4.1"]{对象}

@section[#:tag "s9.5"]{有类型的语言}

@section[#:tag "s9.6"]{类型检查器}
