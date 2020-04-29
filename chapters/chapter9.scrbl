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
是同一个，@note{任何类都是自身的子类，故有此说。——@emph{译注}}因为@${m}可能在目
标对象的某个祖先中声明。

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

求程序的值时，所有类声明都用@tt{initialize-class-env!}处理，随后求表达式的值。过
程@tt{initialize-class-env!}创建一个全局@emph{类环境} (@emph{class environment})，
将类名映射到类的方法。因为这个环境是全局的，我们用一个Scheme变量表示它。在
@secref{s9.4.3}我们再详细讨论类环境。

@racketblock[
@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{ExpVal}}}
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (class-decls body)
        (initialize-class-env! class-decls)
        (value-of body (init-env))))))
]

像之前那样，语言中的各种表达式在过程@tt{value-of}里都有对应的从句，也包括四种新
的生成式。

我们依次考虑新增的每种表达式。

通常，表达式需要求值，是因为它是操作某个对象的方法的一部分。在环境中，这个对象绑
定到伪变量@tt{%self}。我们称之为@emph{伪变量} (@emph{pseudo-variable})，因为它虽
然像普通变量那样遵循词法绑定，但却像下面将要探讨的那样，具有一些独特性质。类似地，
当前方法持有类的超类名字绑定到伪变量@tt{%super}。

求@tt{self}表达式的值时，返回的是@tt{%self}的值。这句话在@tt{value-of}中写作

@codeblock[#:indent 7]{
(self-exp ()
  (apply-env env '%self))
}

求@tt{send}表达式的值时，需要求操作数和对象表达式的值。我们从对象中找出它的类名，
然后用@tt{find-method}找出方法。@tt{find-method}取一类名，一方法名，返回一方法。
接着，我们用当前对象和方法参数调用这个方法。

@codeblock[#:indent 7]{
(method-call-exp (obj-exp method-name rands)
  (let ((args (values-of-exps rands env))
        (obj (value-of obj-exp env)))
    (apply-method
      (find-method
        (object->class-name obj)
        method-name)
      obj
      args)))
}

超类调用与普通方法调用类似，不同之处是，要在表达式持有类的超类中查找方法。
@tt{value-of}中的语句是

@codeblock[#:indent 7]{
(super-call-exp (method-name rands)
  (let ((args (values-of-exps rands env))
        (obj (apply-env env '%self)))
    (apply-method
      (find-method (apply-env env '%super) method-name)
      obj
      args)))
}

我们的最后一项工作是创建对象。求@tt{new}表达式的值时，需要求操作数的值，并根据类
名创建一个新对象。然后，调用对象的初始化函数，但要忽略这个函数的值。最后，返回该
对象。

@codeblock[#:indent 7]{
(new-object-exp (class-name rands)
  (let ((args (values-of-exps rands env))
        (obj (new-object class-name)))
    (apply-method
      (find-method class-name 'initialize)
      obj
      args)
    obj))
}

接下来，我们决定如何表示对象、方法和类。我们通过一个示例解释这种表示，如图9.8所
示。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
class c1 extends object
 field x
 field y
 method initialize ()
  begin
   set x = 11;
   set y = 12
  end
 method m1 () ... x ... y ...
 method m2 () ... send self m3() ...
class c2 extends c1
 field y
 method initialize ()
  begin
   super initialize();
   set y = 22
  end
 method m1 (u,v) ... x ... y ...
 method m3 () ...
class c3 extends c2
 field x
 field z
 method initialize ()
  begin
   super initialize();
   set x = 31;
   set z = 32
  end
 method m3 () ... x ... y ... z ...
let o3 = new c3()
in send o3 m1(7,8)
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "OOP实现的示例程序"))]
}

@nested[#:style eopl-figure]{
@centered{
@(image "../images/simple-object"
  #:suffixes (list ".pdf" ".svg")
  "简单对象")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "简单对象"))]
}

@subsection[#:tag "s9.4.1"]{对象}

我们用包含类名和字段引用的数据类型表示对象。

@racketblock[
(define-datatype object object?
  (an-object
    (class-name identifier?)
    (fields (list-of reference?))))
]

在列表中，我们把“最老”类的字段排在前面。这样，在图9.8中，类@tt{c1}对象的字段排
列为@tt{(x y)}；类@tt{c2}对象的字段排列为@tt{(x y y)}，其中，第二个@tt{y}是
@tt{c2}中的；类@tt{c3}对象的字段排列为@tt{(x y y x z)}。图9.8中对象@tt{o3}的表示
如图9.9所示。当然，我们想让类@tt{c3}中的方法使用@tt{c3}中声明的字段@tt{x}，而不
是@tt{c1}中声明的。我们在建立方法主体的求值环境时处理这点。

这种方式有一条有用的性质：对@tt{c3}的任何子类，列表中同样位置具有同样的字段，因
为随后添加的任何字段都会出现在这些字段的右边。在@tt{c3}任一子类定义的某个方法中，
@tt{x}在什么位置呢？我们知道，在所有这些方法中，如果@tt{x}没有重新定义，@tt{x}的
位置一定是3。那么，当声明字段变量时，对应值的位置保持不变。与@secref{s3.6}处理变
量时类似，这条性质使我们能静态确定字段引用。

创建新方法很容易。我们只需用新引用列表创建一个@tt{an-object}，其长度与对象字段数
目相等。要确定其数目，我们从对象所属类中取出字段变量列表。我们用一个非法值初始化
每个位置，以便得知程序对未初始化位置索值。

@racketblock[
@#,elem{@${\mathit{ClassName} = \mathit{Sym}}}

@#,elem{@bold{@tt{new-object}} : @${\mathit{ClassName} \to \mathit{Obj}}}
(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (map
        (lambda (field-name)
          (newref (list 'uninitialized-field field-name)))
        (class->field-names (lookup-class class-name))))))
]

@subsection[#:tag "s9.4.2"]{方法}

接下来我们处理方法。方法就像过程，但是它们不存储环境。相反，它们记录引用字段的名
字。当调用方法时，它在如下环境中执行其主体

@itemlist[

 @item{方法的形参绑定到新引用，引用初始化为实参的值。这与IMPLICIT-REFS中的
 @tt{apply-procedure}行为类似。}

 @item{伪变量@tt{%self}和@tt{%super}分别绑定到当前对象和方法的超类。}

 @item{可见的字段名绑定到当前对象的字段。要实现这点，我们定义

@racketblock[
(define-datatype method method?
  (a-method
    (vars (list-of identifier?))
    (body expression?)
    (super-name identifier?)
    (field-names (list-of identifier?))))

@#,elem{@bold{@tt{apply-method}} : @${\mathit{Method} \times \mathit{Obj} \times \mathit{Listof(ExpVal)} \to \mathit{ExpVal}}}
(define apply-method
  (lambda (m self args)
    (cases method m
      (a-method (vars body super-name field-names)
        (value-of body
          (extend-env* vars (map newref args)
            (extend-env-with-self-and-super
              self super-name
              (extend-env* field-names (object->fields self)
                (empty-env)))))))))
]
 }

]

这里，我们用练习2.10中的@tt{extend-env*}，扩展环境时，把变量列表绑定到指代值的列
表。我们还给环境接口新增过程@tt{extend-env-with-self-and-super}，分别将
@tt{%self}和@tt{%super}绑定到对象和类名。

要确保各方法看到正确的字段，我们构建@tt{field-names}列表时要小心。各方法只应见到
最后一个声明的同名字段，其他同名字段应被遮蔽。所以，我们构建@tt{field-names}列表
时，将把最右边之外的出现的每个名字替换为新名。对图9.8中的程序，得出的
@tt{field-names}如下

@nested{

@tabular[#:sep @hspace[4]
         (list (list @bold{类} @bold{定义的字段} @bold{字段}      @bold{@tt{field-names}})
               (list @tt{c1}   @tt{x, y}         @tt{(x y)}       @tt{(x@${\phantom{xxx}}y)})
               (list @tt{c2}   @tt{y}            @tt{(x y y)}     @tt{(x@${\phantom{xxx}}y%1 y)})
               (list @tt{c3}   @tt{x, z}         @tt{(x y y x z)} @tt{(x%1@${\phantom{x}}y%1 y x z)}))]

由于方法主体对@tt{x%1}和@tt{y%1}一无所知，所以对各字段变量，它们只能见到最右边的。

}

图9.10展示了求值图9.8中方法主体内的@tt{send o3 m1(7,8)}时创建的环境。这张图表明，
引用列表可能比变量列表长：变量列表只是@tt{(x y%1 y)}，因为从@tt{c2}的方法@tt{m1}
中只能见到这些字段变量，但@tt{(object->fields self)}的值是对象中所有字段的列表。
不过，由于三个可见字段变量的值是列表中的头三个元素，而且我们把第一个@tt{y}重命名
为@tt{y%1}（该方法对词一无所知），方法@tt{m1}将把变量@tt{y}与@tt{c2}中声明的
@tt{y}关联起来，正符期望。

@nested[#:style eopl-figure]{
@centered{
@(image "../images/env-for-method"
  #:scale 0.95
  #:suffixes (list ".pdf" ".svg")
  "方法调用时的环境")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "方法调用时的环境"))]
}

当@tt{self}的持有类和所属类相同时，变量列表的长度通常与字段位置列表的长度相同。
如果持有类位于类链的上端，那么位置可能多于字段变量，与字段变量对应的值将位于列表
开头，其余值则不可见。

@subsection[#:tag "s9.4.3"]{类和类环境}

迄今为止，我们的实现都依赖从类名获取与类相关的信息。所以，我们需要一个@emph{类环
境} (@emph{class environment}) 完成这一工作。类环境将每个类名与描述类的数据结构
关联起来。

类环境是全局的：在我们的语言中，类声明聚集于程序开头，且对整个程序生效。所以，我
们用名为@tt{the-class-env}的全局变量表示类环境，它包含一个(类名, 类)列表的列表，
但我们用过程@tt{add-to-class-env!}和@tt{lookup-class}隐藏这一表示。

@racketblock[
@#,elem{@${\mathit{ClassEnv} = \mathit{Listof(List(ClassName, Class))}}}

@#,elem{@bold{@tt{the-class-env}} : @${\mathit{ClassEnv}}}
(define the-class-env '())

@#,elem{@bold{@tt{add-to-class-env!}} : @${\mathit{ClassName} \times \mathit{Class} \to \mathit{Unspecified}}}
(define add-to-class-env!
  (lambda (class-name class)
    (set! the-class-env
      (cons
        (list class-name class)
        the-class-env))))

@#,elem{@bold{@tt{lookup-class}} : @${\mathit{ClassName} \to \mathit{Class}}}
(define lookup-class
  (lambda (name)
    (let ((maybe-pair (assq name the-class-env)))
      (if maybe-pair (cadr maybe-pair)
        (report-unknown-class name)))))
]

对每个类，我们记录三样东西：超类的名字，字段变量的列表，以及将方法名映射到方法的
环境。

@nested{
@racketblock[
(define-datatype class class?
  (a-class
    (super-name (maybe identifier?))
    (field-names (list-of identifier?))
    (method-env method-environment?)))
]

这里，我们用谓词@tt{(maybe identifier?)}，判断值是否为符号或@tt{#f}。后一种情况
对是必须的，因为类@tt{object}没有超类。@tt{filed-names}是类的方法能见到的字段，
@tt{method-env}是一环境，给出了类中每个方法名的定义。

}

我们初始化类环境时，为类@tt{object}添加一个绑定。对每个声明，我们向类环境添加新
的绑定，将类名绑定到一个@tt{class}，它包含超类名，类中方法的@tt{field-names}，以
及类中方法的环境。

@racketblock[
@#,elem{@bold{@tt{initialize-class-env!}} : @${\mathit{Listof(ClassDecl)} \to \mathit{Unspecified}}}
(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
      (list
        (list 'object (a-class #f '() '()))))
    (for-each initialize-class-decl! c-decls)))

@#,elem{@bold{@tt{initialize-class-decl!}} : @${\mathit{ClassDecl} \to \mathit{Unspecified}}}
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (c-name s-name f-names m-decls)
        (let ((f-names
                (append-field-names
                  (class->field-names (lookup-class s-name))
                  f-names)))
          (add-to-class-env!
            c-name
            (a-class s-name f-names
              (merge-method-envs
                (class->method-env (lookup-class s-name))
                (method-decls->method-env
                  m-decls s-name f-names)))))))))
]

过程@tt{append-field-names}用来给当前类创建@tt{field-names}。它@elem[#:style
question]{扩展}超类字段和新类声明的字段，只是将超类中被新字段遮蔽的字段替换为新
名字，就像@elem[#:style question]{341页}的例子那样。

@racketblock[
@#,elem{@bold{@tt{append-field-names}} : @linebreak[]@${\phantom{xx}}@${\mathit{Listof(FieldName)} \times \mathit{Listof(FieldName)} \to \mathit{Listof(FieldName)}}}
(define append-field-names
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields)
      (else
        (cons
          (if (memq (car super-fields) new-fields)
            (fresh-identifier (car super-fields))
            (car super-fields))
          (append-field-names
            (cdr super-fields) new-fields))))))
]

@subsection[#:tag "s9.4.4"]{方法环境}

剩下的只有@tt{find-method}和@tt{merge-method-envs}。

像之前处理类那样，我们用(方法名, 方法)列表的列表表示方法环境，用@tt{find-method}
查找方法。

@racketblock[
@#,elem{@${\mathit{MethodEnv} = \mathit{Listof(List(MethodName, Method))}}}

@#,elem{@bold{@tt{find-method}} : @${\mathit{Sym} \times \mathit{Sym} \to \mathit{Method}}}
(define find-method
  (lambda (c-name name)
    (let ((m-env (class->method-env (lookup-class c-name))))
      (let ((maybe-pair (assq name m-env)))
        (if (pair? maybe-pair) (cadr maybe-pair)
          (report-method-not-found name))))))
]

用这条信息，我们可以写出@tt{method-decls->method-env}。它取一个类的方法声明，创
建一个方法环境，记录每个方法的绑定变量，主体，持有类的超类名，以及持有类的
@tt{field-names}。

@racketblock[
@#,elem{@bold{@tt{method-decls->method-env}} : @linebreak[]@${\phantom{xx}}@${\mathit{Listof(MethodDecl)} \times \mathit{ClassName} \times \mathit{Listof(FieldName)} \to \mathit{MethodEnv}}}
(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
      (lambda (m-decl)
        (cases method-decl m-decl
          (a-method-decl (method-name vars body)
            (list method-name
              (a-method vars body super-name field-names)))))
      m-decls)))
]

最后，我们写出@tt{merge-method-envs}。由于新类中的方法覆盖了旧类的同名方法，我们
可以直接扩展环境，将新方法添加到前面。

@nested{
@racketblock[
@#,elem{@bold{@tt{merge-method-envs}} : @${\mathit{MethodEnv} \times \mathit{MethodEnv} \to \mathit{MethodEnv}}}
(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))
]

还有一些方式构建的方法环境在查询方法时更高效（练习9.18）。

}

@nested[#:style eopl-figure]{
@racketblock[
((c3
   #(struct:a-class c2 (x%2 y%1 y x z)
      ((initialize #(struct:a-method ()
                      #(struct:begin-exp ...) c2 (x%2 y%1 y x z)))
        (m3 #(struct:a-method ()
               #(struct:diff-exp ...)) c2 (x%2 y%1 y x z))
        (initialize #(struct:a-method ...))
        (m1 #(struct:a-method (u v)
               #(struct:diff-exp ...) c1 (x y%1 y)))
        (m3 #(struct:a-method ...))
        (initialize #(struct:a-method ...))
        (m1 #(struct:a-method ...))
        (m2 #(struct:a-method ()
               #(struct:method-call-exp #(struct:self-exp) m3 ())
               object (x y))))))
  (c2
    #(struct:a-class c1 (x y%1 y)
       ((initialize #(struct:a-method ()
                       #(struct:begin-exp ...) c1 (x y%1 y)))
         (m1 #(struct:a-method (u v)
                #(struct:diff-exp ...) c1 (x y%1 y)))
         (m3 #(struct:a-method ()
                #(struct:const-exp 23) c1 (x y%1 y)))
         (initialize #(struct:a-method ...))
         (m1 #(struct:a-method ...))
         (m2 #(struct:a-method ()
                #(struct:method-call-exp #(struct:self-exp) m3 ())
                object (x y))))))
  (c1
    #(struct:a-class object (x y)
       ((initialize #(struct:a-method ()
                       #(struct:begin-exp ...) object (x y)))
         (m1 #(struct:a-method ()
                #(struct:diff-exp ...) object (x y)))
         (m2 #(struct:a-method ()
                #(struct:method-call-exp #(struct:self-exp) m3 ())
                object (x y))))))
  (object
    #(struct:a-class #f () ())))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "图9.8中的类环境"))]
}

@subsection[#:tag "s9.4.5"]{练习}

@exercise[#:level 1 #:tag "ex9.1"]{

用本节的语言实现以下各项：

@itemlist[#:style 'ordered

 @item{队列类，包含方法@tt{empty?}、@tt{enqueue}和@tt{dequeue}。}

 @item{扩展队列类，添加计数器，记录当前队列已进行的操作数。}

 @item{扩展队列类，添加计数器，记录本类所有队列已进行的操作总数。提示：可在初始
 化时传递共享计数器。}

]

}

@exercise[#:level 1 #:tag "ex9.2"]{

继承也可以很危险，因为子类可以任意覆盖一个方法，改变其行为。定义继承自
@tt{oddeven}的类@tt{bogus-oddeven}，覆盖方法@tt{even}，从而导致@tt{let o1 = new
bogus-oddeven() in send o1 odd (13)}给出错误的答案。

}

@exercise[#:level 2 #:tag "ex9.3"]{

在图9.11中，哪里是共享的方法环境？哪里是共享的@tt{field-names}列表？

}

@exercise[#:level 1 #:tag "ex9.4"]{

修改对象的表示，让@${\mathit{Obj}}包含对象所属的类，而非其名字。跟文中的方式相比，
这有什么优势和劣势？

}

@exercise[#:level 1 #:tag "ex9.5"]{

@secref{s9.4}中的解释器在词法环境中存储方法持有类的超类名。修改实现，让方法存储
持有类的名字，然后用持有类的名字查找超类名。

}

@exercise[#:level 1 #:tag "ex9.6"]{

给我们的语言添加表达式@tt{instanceof @${exp} @${class\mbox{-}name}}。当且仅当表
达式@${exp}的值为对象，且是@${class\mbox{-}name}或其子类的实例时，这种表达式的值
为真。

}

@exercise[#:level 1 #:tag "ex9.7"]{

在我们的语言中，方法环境包含持有类@emph{以及}超类声明的字段变量的绑定。限制它，
只包含持有类的字段变量绑定。

}

@exercise[#:level 1 #:tag "ex9.8"]{

给我们的语言添加一个新表达式，

@centered{@tt{fieldref @${obj} @${field\mbox{-}name}}}

取出指定对象指定字段的内容。再添加

@centered{@tt{fieldset @${obj} @${field\mbox{-}name} = @${exp}}}

将指定字段设置为@${exp}的值。

}

@exercise[#:level 1 #:tag "ex9.9"]{

添加表达式@tt{superfieldref @${field\mbox{-}name}}和@tt{superfieldset
@${field\mbox{-}name} = @${exp}}，处理@tt{self}中被遮蔽的字段。记住，@tt{super}
是静态的，总是指持有类的超类。

}

@exercise[#:level 2 #:tag "ex9.10"]{

有些面向对象编程语言支持指明类的方法调用和字段引用。在指明类的方法调用中，可以写
@tt{named-send c1 o m1()}。只要@tt{o}是@tt{c1}或其子类的实例，这会对@tt{o}调用
@tt{c1}的方法@tt{m1}，即使@tt{o}所属类覆盖了@tt{m1}。这是一种静态方法分发。指明
类的字段引用为字段引用提供类似工具。给本节的语言添加指明类的方法调用，字段引用和
字段设置。

}

@exercise[#:level 2 #:tag "ex9.11"]{

允许CLASSES指定每个方法是@emph{私有的} (@emph{private})，只能在持有类内访问；或
@emph{受保护的} (@emph{protected})，只能在持有类及其后代中访问；或 @emph{公有的}
(@emph{public})，各处都能访问。许多面向对象编程语言包含了某种形式的这一特性。

}

@exercise[#:level 2 #:tag "ex9.12"]{

像练习9.11那样，允许CLASSES指定每个字段是私有的，受保护的，还是公有的。

}

@exercise[#:level 2 #:tag "ex9.13"]{

为了防止练习9.2那样的恶意子类，许多面向对象编程语言都能指定无法覆盖的@${final}方
法。给CLASSES添加这样的组件，那么就能写

@nested[#:style 'code-inset]{
@verbatim|{
class oddeven extends object
 method initialize () 1
 final method even (n)
  if zero?(n) then 1 else send self odd(-(n,1))
 final method odd (n)
  if zero?(n) then 0 else send self even(-(n,1))
}|
}

}

@exercise[#:level 2 #:tag "ex9.14"]{

另一种防止恶意子类的方法是使用某种形式的@emph{静态分发}。修改CLASSES，通过
@tt{self}调用的总是持有类的方法，而不是目标对象所属类的方法。

}

@exercise[#:level 2 #:tag "ex9.15"]{

很多面向对象编程语言提供@emph{静态} (@emph{static})或者@emph{类} (@emph{class})
变量。静态变量与类的某些状态相关联；类的所有实例共享这一状态。例如，可以写：

@nested[#:style 'code-inset]{
@verbatim|{
class c1 extends object
 static next-serial-number = 1
 field my-serial-number
 method get-serial-number () my-serial-number
 method initialize ()
  begin
   set my-serial-number = next-serial-number;
   set next-serial-number = +(next-serial-number,1)
  end
let o1 = new c1()
    o2 = new c1()
in list(send o1 get-serial-number(),
        send o2 get-serial-number())
}|
}

类@tt{c1}的每个新对象具有连续的序列号。

给我们的语言添加静态变量。由于静态变量可以在方法主体中出现，@tt{apply-method}必
须在它创建的环境中添加额外的绑定。求静态变量（上例中的@tt{l}）初始化表达式的值时，
应使用什么环境？

}

@exercise[#:level 2 #:tag "ex9.16"]{

面向对象编程语言常允许@emph{重载} (@emph{overloading})方法。这一特性允许类有多个
同名方法，只要它们有不同的@emph{签名} (@emph{signature})。方法签名通常是方法名加
上参数类型。由于CLASSES中没有类型，我们可以仅依靠方法名和参数数量重载方法。例如，
类可能有两个@tt{initialize}方法，一个没有参数，用它来初始化时，需要给字段默认值；
另一个有一个参数，用它来初始化时，需要给字段特定值。扩展我们的解释器，允许通过方
法的参数数量重载方法。

}

@exercise[#:level 2 #:tag "ex9.17"]{

显而易见，我们语言中的类定义是全局的。给CALSSES添加局部类，可写成@tt{letclass
@${c} = ... in @${e}}。提示：考虑给解释器添加一个类环境参数。

}

@exercise[#:level 2 #:tag "ex9.18"]{

@tt{merge-method-envs}产生的方法环境可能很长。新写一个@tt{merge-method-envs}，保
证每个方法名只出现一次，而且总是出现在与最先声明相同的位置。例如，在图9.8中，在
@tt{c1}、@tt{c2}、@tt{c3}，以及@tt{c3}任意后代的方法环境中，方法@tt{m2}应出现在
同样的位置。

}

@exercise[#:level 2 #:tag "ex9.19"]{

为CLASSES实现词法寻址。首先，为本节语言写出类似@secref{s3.7}的词法地址计算器。然
后修改环境的实现，去掉其中的名字。接着修改@tt{value-of}和@tt{apply-env}，不再取
符号，而是像@secref{s3.7.2}那样取一词法地址。

}

@exercise[#:level 3 #:tag "ex9.20"]{

方法调用也能够用类似练习9.19中的方式优化吗？讨论为什么能，或为什么不能。

}

@exercise[#:level 2 #:tag "ex9.21"]{

如果类中有很多方法，从头线性搜索方法列表会很耗时。将其改为更快的实现。你的实现能
改进多少？解释你的结果，不论更好还是更坏。

}

@exercise[#:level 2 #:tag "ex9.22"]{

在练习9.16中，我们扩展解释器，给语言添加方法重载。另一种支持重载的方法不需修改解
释器，而是用语法预处理器。写一个预处理器，将每个方法@${m}重命名为@$["m:@n"]的形
式，其中@${n}是方法声明中参数的数量。同样地，它还必须根据操作数的数量，改变每个
方法调用的名字。我们假定程序员在方法名中不使用@$[":@"]，但解释器接受使用@$[":@"]
的方法名。编译器经常使用这种技术实现方法重载。这是一种通用技巧的例子，名为
@emph{名称混淆} (@emph{name mangling})。

}

@exercise[#:level 3 #:tag "ex9.23"]{

我们以词法绑定的方式看待超类调用。但我们还可以做得更好：我们可以@emph{静态}确定
@tt{super}调用。由于超类调用指向类的父类的方法，而父类与其方法在执行之前已知，我
们可以在进行词法寻址和其他分析的同时确定超类调用究竟指的是那个方法。写一个翻译器，
将每个超类调用替换为一个抽象语法树节点，节点中包含实际要调用的方法。

}

@exercise[#:level 3 #:tag "ex9.24"]{

写一个翻译器，把练习9.10中指明类调用的方法名替换为数字，数字表示运行时，指定方法
在指定类方法表中的偏移。为翻译后的代码实现一个解释器，在常数时间内访问指定类的方
法。

}

@exercise[#:level 3 #:tag "ex9.25"]{

我们给图9.5第一个继承例子中的类@tt{point}添加一个方法，判断两个点是否具有相同的
横纵坐标。我们照下面这样给点类添加方法@tt{similarpoints}：

@nested[#:style 'code-inset]{
@verbatim|{
method similarpoints (pt)
 if equal?(send pt getx(), x)
 then equal?(send pt gety(), y)
 else zero?(1)
}|
}

这对所有类型的点都有效。因为@tt{getx}，@tt{gety}和@tt{similarpoints}都在类
@tt{point}中定义，通过继承，它们在@tt{colorpoint}中也有定义。测试
@tt{similarpoints}，比较点和点，点和有色点，有色点和点，以及有色点和有色点。

接下来考虑一个小扩展。我们给类@tt{colorpoint}添加新方法@tt{similarpoints}。我们
希望两个点横纵坐标相同，都是有色点且颜色相同时，它返回真，否则返回假。这里是种错
误做法。

@nested[#:style 'code-inset]{
@verbatim|{
method similarpoints (pt)
 if super similarpoints(pt)
 then equal?(send pt getcolor(),color)
 else zero?(1)
}|
}

测试这一扩展。说明它为何不适用于任意情况。修复它，让所有测试都返回正确的值。

过程依赖多个对象造成的困难称为@emph{二元方法问题} (@emph{binary method problem})。
它表明，本章探讨的以类为中心的面向对象编程模型在处理多个对象时有其不足。这叫做
@emph{二元}方法问题，因为两个对象就能引起这一问题，但当对象数目增加时，它会愈发
严重。

}

@exercise[#:level 3 #:tag "ex9.26"]{

多继承允许一个类有多个父类，但可能带来过度的复杂性。如果两个被继承的类具有同名方
法呢？可以禁止这种情况；也可以按照某种规则遍历方法，比如深度优先、从左到右；还可
以要求在调用时消除这种歧义。字段的情况就更糟了。考虑下面的情形，类@tt{c4}继承自
@tt{c2}和@tt{c3}，二者均继承自@tt{c1}：

@nested[#:style 'code-inset]{
@verbatim|{
class c1 extends object
 field x
class c2 extends c1
class c3 extends c1
class c4 extends c2, c3
}|
}

@tt{c4}的实例是有一个字段@tt{x}实例，由@tt{c2}和@tt{c3}共享呢，还是有两个@tt{x}
字段，一个继承自@tt{c2}，一个继承自@tt{c3}？有些语言选择共享，有些不，还有一些
（至少在某些条件下）任选。这问题的复杂性致使设计时，更偏爱类的单继承，而多继承只
用于接口（@secref{s9.5}），以尽量避免这些困难。

给CLASSES添加多继承。对语法做必要扩展。指出解决方法名和字段名冲突时面临什么问题。
描述共性问题及其解决方法。

}

@exercise[#:level 3 #:tag "ex9.27"]{

实现下面设计的无类有对象语言。对象是一组闭包，各闭包共享一个环境（亦即某种状态），
以方法名为索引。类则由返回对象的过程替代。所以，我们不用写@tt{send o1
m1(11,22,33)}，而是写普通的过程调用@tt{(getmethod(o1,m2) 11 22 33)}；不用写

@nested[#:style 'code-inset]{
@verbatim|{
class oddeven extends object
 method initialize () 1
 method even (n)
  if zero?(n) then 1 else send self odd(-(n,1))
 method odd (n)
  if zero?(n) then 0 else send self even(-(n,1))
let o1 = new oddeven()
in send o1 odd(13)
}|
}

而是写

@nested[#:style 'code-inset]{
@verbatim|{
let make-oddeven
 = proc ()
    newobject
     even = proc (n) if zero?(n) then 1
                     else (getmethod(self,odd) -(n,1))
     odd = proc (n) if zero?(n) then 0
                    else (getmethod(self,even) -(n,1))
    endnewobject
in let o1 = (make-oddeven) in (getmethod(o1,odd) 13)
}|
}

}

@exercise[#:level 3 #:tag "ex9.28"]{

给练习9.27的语言添加继承。

}

@exercise[#:level 3 #:tag "ex9.29"]{

设计和实现不需写明类的面向对象语言，让每个对象包含自身的方法环境。这种对象叫做
@emph{原型} (@emph{prototype})。把类@tt{object}替换为没有方法和字段的原型对象。
这样，我们就能用@tt{let c2 = extend c1 ...}替代@tt{class c2 extends c1 ...}。把
操作@tt{new}替换为@tt{clone}，它取一对象，仅复制其方法和字段。这种语言中的方法位
于一个词法作用范围中，所以应该能像通常那样访问词法上可见的变量以及字段变量。当@emph{超型}
(@emph{superprototype})的字段变量与当前所在词法作用范围的变量同名时，
遮蔽关系是怎样的？

}

@section[#:tag "s9.5"]{带类型的语言}

在@secref{types}，我们展示了如何用类型系统检查程序，保证它决不进行不当操作。通过
检查器的程序决不会调用非过程处理实参，调用过程或其他操作符时，也不会使用错误数量
或类型的实参。

本节，我们将这种技术应用于名为TYPED-OO的面向对象语言。这种语言具有上述所有安全属
性，此外，通过我们检查器的程序决不会给没有对应方法的对象发送消息，也不会给对象发
送实参数量或类型错误的消息。

TYPED-OO的示例程序如图9.12所示。这段程序定义了一个类@tt{tree}，其方法@tt{sum}像
图9.2那样求出叶子之和，方法@tt{equal}取另一棵树，递归向下处理树，判断二者是否相
等。

这种语言的主要新特性是：

@itemlist[

 @item{字段和方法需要用@secref{types}那样的语法指定类型。}

 @item{在面向对象@elem[#:style question]{设置}中，引入@emph{接口}
 (@emph{interface})的概念。}

 @item{语言中引入了@emph{子类型多态} (@emph{subtype polymorphism})的概念。}

 @item{语言中引入了@emph{强制转换} (@emph{casting})的概念，同时包含练习9.6中的
 @tt{instanceof}判断。}

]

我们依次考虑这些特性。

TYPED-OO中的新生成式如图9.13所示。我们添加一种类型@tt{void}，作为@tt{set}操作的
类型，然后添加练习7.9中的列表类型；像练习7.9那样，我们要求调用@tt{list}时至少给
出一个实参。我们将标识符添加到类型表达式的集合中，但在本章，用作类型的标识符与同
名的类或接口相关联。稍后我们详细思考这种对应关系。方法需要指明结果类型和参数类型，
其语法与@secref{types}中的@tt{letrec}类似。最后是新增的两种表达式，@tt{cast}和
@tt{instanceof}。

要理解这种语言的新特性，我们必须像定义7.1.1那样，定义语言的类型。

@nested[#:style (make-style "sdef" '())]{
定义类型为@${t}的表达值@${v}具有如下属性：

 @nested[#:style 'inset]{
 @itemlist[

  @item{若@${c}为类，当且仅当值是一个对象，且是类@${c}或其后代的实例时，值类型为
  @tt{c}。}

  @item{若@${I}为接口，当且仅当值是一个对象，且所属类实现了@${I}时，值类型为
  @${I}。当且仅当类具有@tt{implements @${I}}声明，或其祖先实现了@${I}时，类实现
  了@${I}。}

  @item{若@${t}为其他类型，则用定义7.1.1中的规则。}

 ]
 }

}

对象只能是一个类的实例，但可以有很多类型。

@itemlist[

 @item{创建对象时的类是其类型。}

 @item{该类的超类以及继承关系上方的所有类是其类型。特别地，每个对象都是
 @tt{object}类型。}

 @item{对象所属类实现的任意接口均是其类型。}

]

第二条性质叫做@emph{子类多态} (@emph{subclass polymorphism})。第三条性质叫做@emph{接口多态}
(@emph{interface polymorphism})。

接口表示实现某些方法的所有对象集合，而不论这些对象如何产生。仅当类@${c}按照约定
的类型实现了接口@${I}要求的所有方法时，我们的判类系统才允许@${c}声称实现了@${I}。
虽然我们的例子中只用了一个接口，但一个类可以实现多个不同接口。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
interface tree
 method int sum ()
 method bool equal (t : tree)

class interior-node extends object implements tree
 field tree left
 field tree right
 method void initialize(l : tree, r : tree)
  begin
   set left = l; set right = r
  end
 method tree getleft () left
 method tree getright () right
 method int sum () +(send left sum(), send right sum())
 method bool equal (t : tree)
  if instanceof t interior-node
  then if send left equal(send
                           cast t interior-node
                           getleft())
       then send right equal(send
                              cast t interior-node
                              getright())
       else zero?(1)
  else zero?(1)

class leaf-node extends object implements tree
 field int value
 method void initialize (v : int) set value = v
 method int sum () value
 method int getvalue () value
 method bool equal (t : tree)
  if instanceof t leaf-node
  then zero?(-(value, send cast t leaf-node getvalue()))
  else zero?(1)

let o1 = new interior-node (
          new interior-node (
           new leaf-node(3),
           new leaf-node(4)),
          new leaf-node(5))
in list(send o1 sum(),
        if send o1 equal(o1) then 100 else 200)
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "TYPED-OO的程序示例"))]
}

@nested[#:style eopl-figure]{
@envalign*{
         \mathit{ClassDecl} &::= @tt{class @m{\mathit{Identifier}} extends @m{\mathit{Identifier}}} \\
          &\mathrel{\phantom{::=}} \phantom{x}\{@tt{implements @m{\mathit{Identifier}}}\}^{*} \\
          &\mathrel{\phantom{::=}} \phantom{x}\{@tt{field @m{\mathit{Type}} @m{\mathit{Identifier}}}\}^{*} \\
          &\mathrel{\phantom{::=}} \phantom{x}\{@m{\mathit{MethodDecl}}\}^{*} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{\begin{math}\begin{alignedat}{-1}
                                          &@tt{a-class-decl}@tt["("]@tt{c-name s-name i-names} \\
                                          &\phantom{xxxxxxxxxxxx}@tt{f-types f-names m-decls}@tt[")"]
                                         \end{alignedat}\end{math}} \\[5pt]
         \mathit{ClassDecl} &::= @tt{interface @m{\mathit{Identifier}} @m{\{\mathit{AbstractMethodDecl}\}^{*}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{an-interface-decl (i-name abs-m-decls)}} \\[5pt]
        \mathit{MethodDecl} &::= @tt{method @m{\mathit{Type}} @m{\mathit{Identifier}} (@m{\{\mathit{Identifier} \ : \mathit{Type}\}^{*(,)}}) @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{{\begin{math}\begin{alignedat}{-1}
                                          &@tt{a-method-decl} \\
                                          &\phantom{x}@tt{(res-type m-name vars var-types body)}
                                         \end{alignedat}\end{math}}} \\[5pt]
\mathit{AbstractMethodDecl} &::= @tt{method @m{\mathit{Type}} @m{\mathit{Identifier}} (@m{\{\mathit{Identifier} \ : \mathit{Type}\}^{*(,)}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{{\begin{math}\begin{alignedat}{-1}
                                          &@tt{a-method-decl} \\
                                          &\phantom{x}@tt{(res-type m-name m-vars m-var-types)}
                                         \end{alignedat}\end{math}}} \\[5pt]
        \mathit{Expression} &::= @tt{cast @m{\mathit{Expression}} @m{\mathit{Identifier}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cast-exp (exp c-name)}} \\[5pt]
        \mathit{Expression} &::= @tt{instanceof @m{\mathit{Expression}} @m{\mathit{Identifier}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{instanceof-exp (exp name)}} \\[5pt]
              \mathit{Type} &::= @tt{void} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{void-type ()}} \\[5pt]
              \mathit{Type} &::= \mathit{Identifier} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{class-type (class-name)}} \\[5pt]
              \mathit{Type} &::= @tt{listof @m{\mathit{Type}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{list-type (type1)}}
          }

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "TYPED-OO中的新生成式"))]
}

在图9.2中，类@tt{interior-node}和@tt{leaf-node}都实现了接口@tt{tree}。类型检查器
允许这样，因为它们都实现了@tt{tree}要求的方法。

当@${e}的值是一个对象，且是类@${c}或其后代的实例时，表达式@tt{instanceof @${e}
@${c}}返回真。强制转换是@tt{instanceof}的补充。当@${e}的值是一对像，且是类@${c}
或其后代的实例时，@tt{cast}表达式@${case @${e} @${c}}的值与@${e}的值相同。否则
@tt{case}表达式报错。@tt{cast @${e} @${c}}的类型总是@${c}，因为它只要返回值，类
型就一定是@${c}。

例如，我们的示例程序包含如下方法

@nested{

@nested[#:style 'code-inset]{
@verbatim|{
method bool equal(t : tree)
 if instanceof t interior-node
 then if send left
          equal(send cast t interior-node getleft())
      then send right
          equal(send cast t interior-node getright())
      else false
 else false
}|
}

表达式@tt{case t interior-node}检查@tt{t}的值是否为@tt{interior-node}（或其后代，
如果有的话）的实例。如果是，返回@tt{t}的值；否则报错。当且仅当对应的@tt{cast}成
功时，@tt{instanceof}表达式返回真值。因此，本例中的@tt{instanceof}确保强制转换一
定成功。而强制转换又确保@tt{send ... getleft()}能够使用。强制转换表达式返回值的
类型一定为类@tt{interior-node}，因此，给这个值发消息@tt{getleft}是安全的。

}

我们的实现从@secref{s9.4.1}中的解释器开始。我们给@tt{value-of}添加两条从句，求
@tt{instanceof}和@tt{cast}表达式的值：

@codeblock[#:indent 11]{
(cast-exp (exp c-name)
  (let ((obj (value-of exp env)))
    (if (is-subclass? (object->class-name obj) c-name)
      obj
      (report-cast-error c-name obj))))

(instanceof-exp (exp c-name)
  (let ((obj (value-of exp env)))
    (if (is-subclass? (object->class-name obj) c-name)
      (bool-val #t)
      (bool-val #f))))
}

过程@tt{is-subclass?}顺着第一个类结构的父系而上，直到找出第二个类，或在父系为
@tt{#f}时停止。由于接口只用作类型，这个过程忽略它们。

@racketblock[
@#,elem{@bold{@tt{is-subclass?}} : @${\mathit{ClassName} \times \mathit{ClassName} \to \mathit{Bool}}}
(define is-subclass?
  (lambda (c-name1 c-name2)
    (cond
      ((eqv? c-name1 c-name2) #t)
      (else
        (let ((s-name (class->super-name
                        (lookup-class c-name1))))
          (if s-name (is-subclass? s-name c-name2) #f))))))
]

这样，本节语言的解释器就修改完成了。

@exercise[#:level 1 #:tag "ex9.30"]{

创建接口@tt{summable}：

@nested[#:style 'code-inset]{
@verbatim|{
interface summable
 method int sum ()
}|
}

为可求和列表、可求和二叉树（如图9.12）和可求和广义树（每个节点包含一个可求和的子
节点列表）定义类。

然后为接口

@nested[#:style 'code-inset]{
@verbatim|{
interface stringable
 method string to-string ()
}|
}

做同样的操作。

}

@exercise[#:level 1 #:tag "ex9.31"]{

在图9.12中，把@tt{tree}定义为类，然后让两个节点类继承@tt{tree}可行吗？在什么情况
下这种方法比使用类似@tt{summable}的接口更好？在什么情况下更糟？

}

@exercise[#:level 2 #:tag "ex9.32"]{

不使用@tt{instanceof}和@tt{cast}，给类@tt{tree}写一个等值判断谓词。这里需要用
@emph{双派发} (@emph{double dispatch})替代通常方法使用的单派发。可做如下模拟：不
用@tt{instanceof}找出实参@tt{t}的类，而是让当前的树给@tt{t}发回一条消息，这条消
息编码当前树所属类，其参数则包含适当字段的值。

}

@section[#:tag "s9.6"]{类型检查器}

现在我们来看这种语言的检查器。检查器的目标是确保一些安全属性。对我们的语言来说，
这些属性包括原有过程式语言的那部分和之后面向对象语言的那部分：通过我们类型检查器
的程序不会

@itemlist[

 @item{给非对象发送消息，}

 @item{给没有对应方法的对象发消息，}

 @item{给对象发送参数数量或类型错误的消息。}

]

我们无意验证@tt{initialize}方法确实初始化了所有字段，所以程序仍可能引用未初始化
的字段。同样地，由于@tt{initialize}方法的类型通常难以预测，我们的检查器未防止以
错误数量或类型的参数直接调用@tt{initialize}方法，但通过@tt{new}间接调用
@tt{initialize}方法一定是正确的。

检查器首先实现@tt{type-of-program}。由于所有类的所有方法都是互递归的，我们以类似
@tt{letrec}的方式处理。对@tt{letrec}，我们首先收集过程声明的类型，建立
@tt{tenv-for-letrec-body}（图7.3）。然后，我们对照每个过程主体与其声明类型。最后，
我们在@tt{tenv-for-letrec-body}中检查@tt{letrec}的主体。

这里，我们首先调用@tt{initialize-static-class-env!}，遍历类声明，将所有类型收集
到一个静态环境中。由于这个环境是全局的，且不会改变，我们不是将其作参数传递，而是
把它存储在一个Scheme变量中。然后，我们用@tt{check-class-decl!}检查每个类声明。最
后，我们找出程序主体的类型。

@racketblock[
@#,elem{@bold{@tt{type-of-program}} : @${\mathit{Program} \to \mathit{Type}}}
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (class-decls exp1)
        (initialize-static-class-env! class-decls)
        (for-each check-class-decl! class-decls)
        (type-of exp1 (init-tenv))))))
]

静态类环境将每个类名映射到一个静态类，这个类包含其父类的名字，字段的名字和类型，
以及方法的名字和类型。在我们的语言中，接口没有父类，没有字段，所以我们用只含所需
方法名字和类型的数据结构表示它们（但是，看看练习9.36）。

@racketblock[
(define-datatype static-class static-class?
  (a-static-class
    (super-name (maybe identifier?))
    (interface-names (list-of identifier?))
    (field-names (list-of identifier?))
    (field-types (list-of type?))
    (method-tenv method-tenv?))
  (an-interface
    (method-tenv method-tenv?)))
]

在思考如何建立静态环境之前，我们先思考如何扩展@tt{type-of}，检查六种面向对象表达
式的类型：@tt{self}、@tt{instanceof}、@tt{cast}、方法调用、超类调用、以及
@tt{new}。

对@tt{self}表达式，我们用伪变量@tt{%self}查询其类型，该变量一定绑定到当前持有类
的类型，就像在解释器中，它绑定到当前持有对象一样。

@tt{instanceof}表达式如果返回，总是返回@tt{bool}值。若@${e}的值是一个对象，且是
@${c}或它的某个后代的实例，则表达式@tt{cast @${e} @${c}}返回@${e}的值。因此，
@tt{cast @${e} @${c}}如果返回值，值的类型是@${c}。所以我们总能将@tt{cast @${e}
@${c}}的类型视为@${c}。对@tt{instanceof}和@tt{cast}表达式，解释器求出参数的值，
并用它执行@tt{object->class-name}，所以我们也必须确保操作数类型正常，且返回值是
一个对象。这三种情况的代码如图9.14所示。

接下来我们考虑方法调用。现在，我们的语言中有三种调用：过程调用、方法调用和超类调
用。我们抽象出一个过程来检查它们。

@nested[#:style eopl-figure]{
@codeblock[#:indent 11]{
(self-exp ()
  (apply-tenv tenv '%self))

(instanceof-exp (exp class-name)
  (let ((obj-type (type-of exp tenv)))
    (if (class-type? obj-type)
      (bool-type)
      (report-bad-type-to-instanceof obj-type exp))))

(cast-exp (exp class-name)
  (let ((obj-type (type-of exp tenv)))
    (if (class-type? obj-type)
      (class-type class-name)
      (report-bad-type-to-cast obj-type exp))))
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "面向对象表达式在" @tt{type-of} "中的对应语句，第1部分"))]
}

@racketblock[
@#,elem{@bold{@tt{type-of-call}} : @${\mathit{Type} \times \mathit{Listof(Type)} \times \mathit{Listof(Exp)} \times \mathit{Exp} \to \mathit{Type}}}
(define type-of-call
  (lambda (rator-type rand-types rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
        (if (not (= (length arg-types) (length rand-types)))
          (report-wrong-number-of-arguments
            (map type-to-external-form arg-types)
            (map type-to-external-form rand-types)
            exp))
        (for-each check-is-subtype! rand-types arg-types rands)
        result-type)
      (else
        (report-rator-not-of-proc-type
          (type-to-external-form rator-type)
          exp)))))
]

这个过程等价于CHECKED中@tt{call-exp}对应的那一行（图7.2），但有两处明显区别。首
先，由于我们的过程现在取多个参数，我们要确保调用时的实参数目正确，而在
@tt{for-each}这行，我们逐一对照每个操作数的类型和过程类型中相对应的参数类型。更
有意思的是第二点，我们把图7.2中的@tt{check-equal-type!}换成了
@tt{check-is-subtype!}。

@nested[#:style eopl-figure]{
@centered{
@(image "../images/subtyping-proc-type"
  #:scale 1.5
  #:suffixes (list ".pdf" ".svg")
  "过程类型的子类型判定")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "过程类型的子类型判定"))]
}

为什么必须这样？子类多态原则是说，如果类@${c_2}扩展了@${c_1}，那么类@${c_2}的对
象可在类@${c_1}对象能够出现的任何地方使用。如果我们写出了过程@tt{proc (o :
@${c_1}) ...}，那么该过程应该能取类型为@${c_2}的实参。

通常，我们可以将子类多态的概念推广到@emph{子类型多态}，就像@secref{modules}中处
理@tt{<:}那样。我们说@${t_1}是@${t_2}的子类型，当且仅当

@itemlist[

 @item{@${t_1}和@${t_2}是类，且@${t_1}是@${t_2}的子类，或}

 @item{@${t_1}是类，@${t_2}是接口，且@${t_1}或其某个超类实现了@${t_2}，或}

 @item{@${t_1}和@${t_2}是过程类型，且@${t_2}参数类型是@${t_1}参数类型的子类型，
 @${t_1}结果类型是@${t_2}结果类型的子类型。}

]

要理解最后一条规则，令@${t_1}为@tt{(c1 -> d1)}，@${t_2}为@tt{(c2 -> d2)}，且
@tt{c2 < c1}，@tt{d1 < d2}。令@tt{f}为一过程，类型为@${t_1}。我们说@tt{f}类型也
为@${t_2}。为什么？假设我们给@tt{f}传递了类型为@tt{c2}的参数。由于@tt{c2 < c1}，
参数类型也是@tt{c1}，所以@tt{f}可以接受这个参数。然后，@tt{f}返回值类型为@tt{d1}。
但由于@tt{d1 < d2}，这个结果类型也是@tt{d2}。所以，如果给@tt{f}一个类型为@tt{c2}
的参数，其返回值类型为@tt{d2}。因此，@tt{f}类型为@tt{(c2 -> d2)}。我们说结果类型
的子类型判定是@emph{协变的} (@emph{covariant})，参数类型的子类型判定是@emph{逆变
的}(@emph{contravariant})。见图9.15。这与 @secref{s8.3.2}中@tt{<:-iface}的定义类
似。

这部分代码如图9.16所示。代码使用@tt{every2?}，它扩展练习1.24中的过程@tt{every?}，
取一个双参数谓词和两个列表，当列表长度相同且对应元素满足谓词时，返回@tt{#t}，否
则返回@tt{#f}。

现在可以逐一考虑三种调用（图9.17）。对方法调用，我们首先像通常那样，找出目标对象
和操作数的类型。我们用类似@tt{find-method}的@tt{find-method-type}找出方法的类型。
如果目标类型不是类或接口，那么@tt{type->class-name}报错。如果没有对应方法，那么
@tt{find-method-type}报错。然后，我们调用@tt{type-of-call}验证操作数的类型与方法
的期望相符，并返回结果的类型。

对@tt{new}表达式，我们首先取出类名对应的类信息。如果没有类与名字相关联，那就报错。
之后，用操作数的类型调用@tt{type-of-call}，检查调用@tt{initialize}是否安全。如果
检查通过，那么执行表达式就是安全的。由于@tt{new}表达式返回指定类的新对象，结果类
型就是对应类的类型。

TYPED-OO中表达式的检查讨论完了，我们接着来构建静态类环境。

要构建静态类环境，@tt{initialize-static-class-env!}首先将其设置为空，然后为类
@tt{object}添加绑定。接着它遍历各个类和接口，给静态类环境添加适当的内容。

@racketblock[
@#,elem{@bold{@tt{initialize-static-class-env!}} : @${\mathit{Listof(ClassDecl)} \to \mathit{Unspecified}}}
(define initialize-static-class-env!
  (lambda (c-decls)
    (empty-the-static-class-env!)
    (add-static-class-binding!
      'object (a-static-class #f '() '() '() '()))
    (for-each add-class-decl-to-static-class-env! c-decls)))
]

过程@tt{add-class-decl-to-static-class-env!}（图9.18）承担创建静态类的艰巨工作。
对每个类，我们必须收集其接口、字段和方法：

@itemlist[

 @item{类实现父类实现的任何接口，以及自身声称实现的接口。}

 @item{类具有父类的所有字段，以及自身的字段，但是父类字段被当前声明的字段遮蔽。
 所以，@tt{field-names}由@tt{append-field-names}计算而得，就像
 @tt{initialize-class-decl!}那样（@elem[#:style question]{334页}）。}

 @item{类字段的类型包括父类字段的类型，以及自身声明字段的类型。}

 @item{类的方法包括父类的和自身的，方法带有声明类型。我们用@tt{proc-type}记录方
 法的类型。我们把当前声明的方法放在前面，因为它们覆盖父类的方法。}

 @item{我们确保当前类中声明的方法名、接口名和字段名不重复。我们还确保类中一定有
 @tt{initialize}方法。}

]

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{check-is-subtype!}} : @${\mathit{Type} \times \mathit{Type} \times \mathit{Exp} \to \mathit{Unspecified}}}
(define check-is-subtype!
  (lambda (ty1 ty2 exp)
    (if (is-subtype? ty1 ty2)
      #t
      (report-subtype-failure
        (type-to-external-form ty1)
        (type-to-external-form ty2)
        exp))))

@#,elem{@bold{@tt{is-subtype?}} : @${\mathit{Type} \times \mathit{Type} \to \mathit{Bool}}}
(define is-subtype?
  (lambda (ty1 ty2)
    (cases type ty1
      (class-type (name1)
        (cases type ty2
          (class-type (name2)
            (statically-is-subclass? name1 name2))
          (else #f)))
      (proc-type (args1 res1)
        (cases type ty2
          (proc-type (args2 res2)
            (and
              (every2? is-subtype? args2 args1)
              (is-subtype? res1 res2)))
          (else #f)))
      (else (equal? ty1 ty2)))))

@#,elem{@bold{@tt{statically-is-subclass?}} : @${\mathit{ClassName} \times \mathit{ClassName} \to \mathit{Bool}}}
(define statically-is-subclass?
  (lambda (name1 name2)
    (or
      (eqv? name1 name2)
      (let ((super-name
              (static-class->super-name
                (lookup-static-class name1))))
        (if super-name
          (statically-is-subclass? super-name name2)
          #f))
      (let ((interface-names
              (static-class->interface-names
                (lookup-static-class name1))))
        (memv name2 interface-names)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "TYPED-OO的子类型判定"))]
}

@nested[#:style eopl-figure]{
@codeblock[#:indent 11]{
(method-call-exp (obj-exp method-name rands)
  (let ((arg-types (types-of-exps rands tenv))
         (obj-type (type-of obj-exp tenv)))
    (type-of-call
      (find-method-type
        (type->class-name obj-type)
        method-name)
      arg-types
      rands
      exp)))

(super-call-exp (method-name rands)
  (let ((arg-types (types-of-exps rands tenv))
         (obj-type (apply-tenv tenv '%self)))
    (type-of-call
      (find-method-type
        (apply-tenv tenv '%super)
        method-name)
      arg-types
      rands
      exp)))

(new-object-exp (class-name rands)
  (let ((arg-types (types-of-exps rands tenv))
         (c (lookup-static-class class-name)))
    (cases static-class c
      (an-interface (method-tenv)
        (report-cant-instantiate-interface class-name))
      (a-static-class (super-name i-names
                        field-names field-types
                        method-tenv)
        (type-of-call
          (find-method-type
            class-name
            'initialize)
          arg-types
          rands
          exp)
        (class-type class-name)))))
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "面向对象表达式在" @tt{type-of} "中的对应语句，第2部分"))]
}

对接口声明，我们只需处理方法名与其类型。

一旦建立了静态类环境，我们可以检查每个类声明。这由@tt{check-class-decl!}（图9.19）
完成。对接口，什么都不必检查。对类声明，我们传递从静态类环境收集到的信息，检查每
个方法。最后，我们检查类是否实现了它声称实现的每个接口。

要检查方法声明，我们首先检查其主体是否符合声明类型。要这样做，我们建立一个类型环
境，该环境与主体求值时的环境相符。然后我们检查主体的结果类型是否是声明中结果类型
的子类型。

但还没完：如果这个方法覆盖了超类中的某个方法，我们要确保它的类型兼容超类中的方法
类型。之所以如此，是因为这个方法可能由另一方法调用，而另一方法只知道超类方法的类
型。这条规则的唯一例外是@tt{initialize}，它只在当前类中调用，且随继承改变类型
（见图9.12）。要这样做，它调用@tt{maybe-find-method-type}，后者返回已绑定方法的
类型，或者@tt{#f}。见图9.20。

如图9.21，过程@tt{check-if-implements?}取两个符号，分别为类名和接口名。它首先检
查两个符号确实为类名和接口名。然后，它遍历接口中的每个方法，检查类是否提供了同名
且类型兼容的方法。

为图9.12中示例程序生成的静态类环境如图9.22所示。静态类是逆序的，这反映了建立类环
境的顺序。三个类中的方法顺序相同，且类型相同，符合期望。

这样，检查器就完成了。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{add-class-decl-to-static-class-env!}} : @${\mathit{ClassDecl} \to \mathit{Unspecified}}}
(define add-class-decl-to-static-class-env!
  (lambda (c-decl)
    (cases class-decl c-decl
      (an-interface-decl (i-name abs-m-decls)
        (let ((m-tenv
                (abs-method-decls->method-tenv abs-m-decls)))
          (check-no-dups! (map car m-tenv) i-name)
          (add-static-class-binding!
            i-name (an-interface m-tenv))))
      (a-class-decl (c-name s-name i-names
                      f-types f-names m-decls)
        (let ((i-names
                (append
                  (static-class->interface-names
                    (lookup-static-class s-name))
                  i-names))
               (f-names
                 (append-field-names
                   (static-class->field-names
                     (lookup-static-class s-name))
                   f-names))
               (f-types
                 (append
                   (static-class->field-types
                     (lookup-static-class s-name))
                   f-types))
               (method-tenv
                 (let ((local-method-tenv
                         (method-decls->method-tenv m-decls)))
                   (check-no-dups!
                     (map car local-method-tenv) c-name)
                   (merge-method-tenvs
                     (static-class->method-tenv
                       (lookup-static-class s-name))
                     local-method-tenv))))
          (check-no-dups! i-names c-name)
          (check-no-dups! f-names c-name)
          (check-for-initialize! method-tenv c-name)
          (add-static-class-binding! c-name
            (a-static-class
              s-name i-names f-names f-types method-tenv)))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para @tt{add-class-decl-to-static-class-env!}))]
}

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{check-class-decl!}} : @${\mathit{ClassDecl} \to \mathit{Unspecified}}}
(define check-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (an-interface-decl (i-name abs-method-decls)
        #t)
      (a-class-decl (class-name super-name i-names
                      field-types field-names method-decls)
        (let ((sc (lookup-static-class class-name)))
          (for-each
            (lambda (method-decl)
              (check-method-decl! method-decl
                class-name super-name
                (static-class->field-names sc)
                (static-class->field-types sc)))
            method-decls))
        (for-each
          (lambda (i-name)
            (check-if-implements! class-name i-name))
          i-names)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para @tt{check-class-decl!}))]
}

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{check-method-decl!}} : @linebreak[] @${\phantom{x}\mathit{MethodDecl} \times \mathit{ClassName} \times \mathit{ClassName} \times \mathit{Listof(FieldName)} \times \mathit{Listof(Type)} \\ \phantom{xxxx}\to \mathit{Unspecified}}}
(define check-method-decl!
  (lambda (m-decl self-name s-name f-names f-types)
    (cases method-decl m-decl
      (a-method-decl (res-type m-name vars var-types body)
        (let ((tenv
                (extend-tenv
                  vars var-types
                  (extend-tenv-with-self-and-super
                    (class-type self-name)
                    s-name
                    (extend-tenv f-names f-types
                      (init-tenv))))))
          (let ((body-type (type-of body tenv)))
            (check-is-subtype! body-type res-type m-decl)
            (if (eqv? m-name 'initialize) #t
              (let ((maybe-super-type
                      (maybe-find-method-type
                        (static-class->method-tenv
                          (lookup-static-class s-name))
                        m-name)))
                (if maybe-super-type
                  (check-is-subtype!
                    (proc-type var-types res-type)
                    maybe-super-type body)
                  #t)))))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para @tt{check-method-decl!}))]
}

@exercise[#:level 1 #:tag "ex9.33"]{

扩展类型检查器，确保安全属性：@tt{instanceof}和@tt{cast}不会对不是对象的值或不是
类的类型执行。

}

@exercise[#:level 1 #:tag "ex9.34"]{

若@${e}的类型不是@${c}的后代或者祖先，则表达式@tt{cast @${e} @${c}}不会成功。
（为什么？）扩展类型检查器，确保程序只对满足这条属性的@tt{cast}表达式求值。再对
@tt{instanceof}检查做相应扩展。

}

@exercise[#:level 1 #:tag "ex9.35"]{

扩展类型检查器，保证安全属性：@tt{initialize}方法只从@tt{new-object-exp}内部调用。

}

@exercise[#:level 1 #:tag "ex9.36"]{

扩展语言，允许接口继承自其他接口。接口应要求实现父类要求实现的所有方法。

}

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{check-if-implements!}} : @${\mathit{ClassName} \times \mathit{InterfaceName} \to \mathit{Bool}}}
(define check-if-implements!
  (lambda (c-name i-name)
    (cases static-class (lookup-static-class i-name)
      (a-static-class (s-name i-names f-names f-types
                        m-tenv)
        (report-cant-implement-non-interface
          c-name i-name))
      (an-interface (method-tenv)
        (let ((class-method-tenv
                (static-class->method-tenv
                  (lookup-static-class c-name))))
          (for-each
            (lambda (method-binding)
              (let ((m-name (car method-binding))
                     (m-type (cadr method-binding)))
                (let ((c-method-type
                        (maybe-find-method-type
                          class-method-tenv
                          m-name)))
                  (if c-method-type
                    (check-is-subtype!
                      c-method-type m-type c-name)
                    (report-missing-method
                      c-name i-name m-name)))))
            method-tenv))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para @tt{check-if-implements!}))]
}

@exercise[#:level 2 #:tag "ex9.37"]{

我们的TYPED-OO语言使用动态分发。另一种方式是@emph{静态分发}。在静态分发中，方法
的选择依赖于对象的类型，而不是所属类。考虑例子

@nested[#:style 'code-inset]{
@verbatim|{
class c1 extends object
 method int initialize () 1
 method int m1 () 11
 staticmethod int m2 () 21
class c2 extends c1
 method void m1 () 12
 staticmethod int m2 () 22
let f = proc (x : c1) send x m1()
    g = proc (x : c1) send x m2()
    o = new c2()
in list((f o), (g o))
}|
}

当调用@tt{f}和@tt{g}时，@tt{x}类型为@tt{c1}，当绑定到类@tt{c2}的对象。方法
@tt{m1}使用动态分发，所以调用的是@tt{c2}的方法@tt{m1}，返回12。方法@tt{m2}使用静
态分发，所以给@tt{x}发消息@tt{m2}调用与@tt{x}类型相关的方法，在本例中，是@tt{c1}，
所以返回21。

修改 @secref{s9.5}中的解释器，处理静态分发。提示：考虑在环境中记录类型信息，那么
解释器就能在@tt{send}中找出目标表达式的类型。

}

@nested[#:style eopl-figure]{
@racketblock[
((leaf-node
   #(struct:a-static-class
      object
      (tree)
      (value)
      (#(struct:int-type))
      ((initialize #(struct:proc-type
                      (#(struct:int-type))
                      #(struct:void-type)))
        (sum #(struct:proc-type () #(struct:int-type)))
        (getvalue #(struct:proc-type () #(struct:int-type)))
        (equal #(struct:proc-type
                  (#(struct:class-type tree))
                  #(struct:bool-type))))))
  (interior-node
    #(struct:a-static-class
       object
       (tree)
       (left right)
       (#(struct:class-type tree) #(struct:class-type tree))
       ((initialize #(struct:proc-type
                       (#(struct:class-type tree)
                         #(struct:class-type tree))
                       #(struct:void-type)))
         (getleft #(struct:proc-type ()
                     #(struct:class-type tree)))
         (getright #(struct:proc-type ()
                      #(struct:class-type tree)))
         (sum #(struct:proc-type () #(struct:int-type)))
         (equal #(struct:proc-type
                   (#(struct:class-type tree))
                   #(struct:bool-type))))))
  (tree
    #(struct:an-interface
       ((sum #(struct:proc-type () #(struct:int-type)))
         (equal #(struct:proc-type
                   (#(struct:class-type tree))
                   #(struct:bool-type))))))
  (object
    #(struct:a-static-class #f () () () ())))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "为示例程序生成的静态类环境"))]
}
