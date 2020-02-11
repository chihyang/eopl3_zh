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

@title[#:style 'numbered #:tag "cpi"]{传递续文的解释器}

在@secref{expr}，我们用环境的概念探讨绑定行为，建立每部分程序执行的数据上下文。
这里，我们将用类似方式探讨每部分程序执行的@emph{控制语境} (@emph{control
context})。我们将介绍@emph{续文} (@emph{continuation})的概念，用来抽象控制语境。
我们将要编写的解释器会取一续文参数，从而突出控制语境。

考虑下面的Scheme阶乘函数定义。

@racketblock[
(define fact
  (lambda (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))
]

我们可以用推导模拟@tt{fact}的计算过程：

@nested[#:style 'code-inset]{
@verbatim|{
  (fact 4)
= (* 4 (fact 3))
= (* 4 (* 3 (fact 2)))
= (* 4 (* 3 (* 2 (fact 1))))
= (* 4 (* 3 (* 2 (* 1 (fact 0)))))
= (* 4 (* 3 (* 2 (* 1 1))))
= (* 4 (* 3 (* 2 1)))
= (* 4 (* 3 2))
= (* 4 6)
= 24
}|
}

这是阶乘的自然递归定义。每次调用@tt{fact}都保证返回值与调用处的@tt{n}相乘。这样，
随着计算进行，@tt{fact}在越来越大的@emph{控制语境}中调用。比较这一行为与下列过程。

@nested{
@racketblock[
(define fact-iter
  (lambda (n)
    (fact-iter-acc n 1)))

(define fact-iter-acc
  (lambda (n a)
    (if (zero? n) a (fact-iter-acc (- n 1) (* n a)))))
]

用这个定义，我们计算：

@nested[#:style 'code-inset]{
@verbatim|{
  (fact-iter 4)
= (fact-iter-acc 4 1)
= (fact-iter-acc 3 4)
= (fact-iter-acc 2 12)
= (fact-iter-acc 1 24)
= (fact-iter-acc 0 24)
= 24
}|
}
}

这里，@tt{fact-iter-acc}总是在同样的控制语境内调用：在本例中，是没有任何语境。当
@tt{fact-iter-acc}调用自身时，它在@tt{fact-iter-acc}执行的“尾端”，除了把返回值
作为@tt{fact-iter-acc}调用的结果，不须再做任何保证。我们称之为@emph{尾调用}
(@emph{tail call})。这样，上述推导中的每一步都形如@tt{(fact-iter-acc @${n}
@${a})}。

当@tt{fact}这样的过程执行时，每次递归调用都要记录额外的控制信息，此信息保留到调
用返回为止。在上面的第一个推导中，这反映了控制语境的增长。这样的过程呈现@emph{递
归性控制行为} (@emph{recursive control behavior})。

与之相对，@tt{fact-iter-acc}调用自身时，不须记录额外的控制信息。递归调用发生在表
达式的同一层（上述推导的最外层）反映了这一点。在这种情况下，当递归深度（没有对应
返回的递归调用数目）增加时，系统不需要不断增长的内存安放控制语境。只需使用有限内
存安放控制信息的过程呈现出@emph{迭代性控制行为} (@emph{iterative control
behavior})。

为什么这些程序呈现出不同的控制行为呢？在阶乘的递归定义中，过程@tt{fact}在
@emph{操作数位置} (@emph{operand position})调用。我们需要保存这个调用的语境，因
为我们需要记住，过程调用执行完毕之后，我们仍需求出操作数的值，并执行外层调用，在
本例中，是完成待做的乘法。这给出一条重要原则：

@nested[#:style tip]{
 @centered{@bold{不是过程调用导致控制语境扩大，而是操作数的求值。}}
}

本章，我们学习如何跟踪和操作控制语境。我们的核心工具是名为@emph{续文}
(@emph{continuation})的数据类型。续文是控制语境的抽象表示，就像环境是数据语境的
抽象表示。我们将探索续文，编写直接传递续文参数的解释器，就像之前直接传递环境参数
的解释器。一旦处理了简单情况，我们就能明白如何给语言添加组件，以更加复杂的方式处
理控制语境，譬如异常和线程。

在@secref{cps}，我们展示如何用转换解释器的技术转换所有程序。我们说以这种方式转换
的程序具有@emph{续文传递风格} (@emph{continuation-passing style})。@secref{cps}
还展示了续文的其他一些重要应用。

@section[#:tag "s5.1"]{传递续文的解释器}

在我们的新解释器中，诸如@tt{value-of}等主要过程将取第三个参数。这一参数，
@emph{续文}，用来抽象每个表达式求值时的控制语境。

我们从图5.1，即@secref{s3.4}中的LETREC语言解释器入手。我们把
@tt{value-of-program}的结果叫做@${FinalAnswer}，以强调这个表达值是程序的最终值。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@${\mathit{FinalAnswer}} = @${\mathit{ExpVal}}}

@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{FinalAnswer}}}
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

@#,elem{@bold{@tt{value-of}} : @${\mathit{Exp} \times \mathit{Env} \to \mathit{ExpVal}}}
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
        (let ((num1 (expval->num (value-of exp1 env)))
              (num2 (expval->num (value-of exp2 env))))
          (num-val (- num1 num2))))
      (zero?-exp (exp1)
        (let ((num1 (expval->num (value-of exp1 env))))
          (if (zero? num1) (bool-val #t) (bool-val #f))))
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
          (value-of exp2 env)
          (value-of exp3 env)))
      (let-exp (var exp1 body)
        (let ((val1 (value-of exp1 env)))
          (value-of body (extend-env var val1 env))))
      (proc-exp (var body)
        (proc-val (procedure var body env)))
      (call-exp (rator rand)
        (let ((proc1 (expval->proc (value-of rator env)))
              (arg (value-of rand env)))
          (apply-procedure proc1 arg)))
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of letrec-body
          (extend-env-rec p-name b-var p-body env))))))

@#,elem{@bold{@tt{apply-procedure}} : @${\mathit{Proc} \times \mathit{ExpVal} \to \mathit{ExpVal}}}
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extend-env var val saved-env))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "传递环境的解释器"))]
}

我们的目标是重写解释器，避免在调用@tt{value-of}时产生控制语境。当控制语境需要增
加时，我们扩展续文参数，就像在@secref{expr}，程序产生数据语境时，扩展解释器的环
境一样。突出控制语境，我们能看到它如何消长。之后，从@secref{s5.4}到@secref{s5.5}，
我们将用它给我们的语言添加新的控制行为。

现在，我们知道环境表示一个从符号到指代值的函数。续文表示什么呢？表达式的续文表示
一个过程，它取表达式的结果，完成计算。所以我们的接口必须包含一个过程
@tt{apply-cont}，它取一续文@tt{cont}，一个表达值@tt{val}，完成由@tt{cont}指定的
计算。@tt{apply-cont}的合约为：

@nested{
@racketblock[
@#,elem{@${\mathit{FinalAnswer}} = @${\mathit{ExpVal}}}
@#,elem{@bold{@tt{apply-cont}} : @${\mathit{Cont} \times \mathit{ExpVal} \to \mathit{FinalAnswer}}}
]

我们把@tt{apply-cont}的结果叫做@${FinalAnswer}是为了提醒自己，它是计算最终的值：
程序的其他部分都不用它。

}

接口应该包含什么样的续文构造器？随我们分析解释器，这些续文构造器自会显现。首先，
我们需要一个续文构造器，生成不需再对计算值进行操作的语境。我们把这个续文叫做
@tt{(end-cont)}，定义为：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (end-cont) |@${val})
= (begin
    (eopl:printf "计算结束.~%")
    |@${val})
}|
}

调用@tt{(end-cont)}打印出一条计算结束消息，并返回程序的值。因为@tt{(end-cont)}打
印出一条消息，我们可以看出它调用了多少次。在正确的计算中，它只应调用一次。

}

我们把@tt{value-of-program}重写为：

@racketblock[
@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{FinalAnswer}}}
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env) (end-cont))))))
]

现在我们可以写出@tt{value-of/k}。我们一次考虑@tt{value-of}的每个分支。
@tt{value-of}的前几行只是算出一个值，然后返回，不会再次调用@tt{value-of}。在传递
续文的解释器中，这些行调用@tt{apply-cont}，把对应的值传给续文：

@racketblock[
@#,elem{@bold{@tt{value-of/k}} : @${\mathit{Exp} \times \mathit{Env} \times \mathit{Cont} \to \mathit{ExpVal}}}
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (var body)
        (apply-cont cont
          (proc-val (procedure var body env))))
      ...)))
]

目前，@tt{cont}唯一可能的值是终止续文，但这马上就会改变。很容易看出，如果程序为
上述表达式之一，表达式的值将传给@tt{end-cont}（通过@tt{apply-cont}）。

@tt{letrec}的行为也不复杂：它不调用@tt{value-of}，而是创建一个新环境，然后在新环
境中求主体的值。主体的值就是整个表达式的值。这表明主体和整个表达式在同样的控制语
境中执行。因此，主体的值应返还给整个表达式的续文。所以我们写：

@codeblock[#:indent 11]{
(letrec-exp (p-name p-var p-body letrec-body)
  (value-of/k letrec-body
    (extend-env-rec p-name p-var p-body env)
    cont))
}

这解释了一条通用原则：

@nested[#:style tip]{
 @centered{@bold{尾调用不扩大续文}}

 @para[#:style tip-content]{若@${exp_1}的值作为@${exp_2}的值返回，则@${exp_1}和
 @${exp_2}应在同样的续文中执行。
 }
}

写成这样是不对的：

@nested{
@codeblock[#:indent 11]{
(letrec-exp (p-name p-var p-body letrec-body)
  (apply-cont cont
    (value-of/k letrec-body
      (extend-env-rec p-name p-var p-body env)
      (end-cont))))
}

因为调用@tt{value-of/k}是在操作数位置：它要作为@tt{apply-cont}的操作数。此外，由
于使用续文@tt{(end-cont)}会在计算完成之前打印出计算结束消息，这种错误很容易排查。

}

接下来我们考虑@tt{zero?}表达式。在@tt{zero?}表达式中，我们得求出实参的值，然后返
还给依赖该值的续文。所以我们要在新的续文中求实参的值，这个续文会取得返回值，然后
做适当处理。

那么，在@tt{value-of/k}中，我们写：

@nested{
@codeblock[#:indent 11]{
(zero?-exp (exp1)
  (value-of/k exp1 env
    (zero1-cont cont)))
}

其中，@tt{(zero1-cont cont)}是一续文，具有如下属性：

@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (zero1-cont |@${cont}) |@${val})
= (apply-cont |@${cont}
    (bool-val
      (zero? (expval->num |@${val}))))
}|
}

}

就像@tt{letrec}，我们不能把@tt{value-of/k}写成：

@nested{
@codeblock[#:indent 11]{
(zero?-exp (exp1)
  (let ((val (value-of/k exp1 env (end-cont))))
    (apply-cont cont
      (bool-val
        (zero? (expval->num val))))))
}

因为调用@tt{value-of/k}是在操作数位置。@tt{let}声明的右边是在操作数位置，因为
@tt{(let ((@${var} @${exp_1})) @${exp_2})}等效于@tt{((lambda (@${var})
@${exp_2}) @${exp_1})}。@tt{value-of/k}调用的值最终成为@tt{expval->num}的操作数。
像之前那样，如果我们运行这段代码，计算结束消息会出现两次：一次在计算中间，一次在
真正结束时。

}

@tt{let}表达式只比@tt{zero?}表达式稍微复杂一点：求值声明右侧之后，我们在适当的扩
展环境内求主体的值。原来的@tt{let}代码为：

@nested{
@codeblock[#:indent 11]{
(let-exp (var exp1 body)
  (let ((val1 (value-of exp1 env)))
    (value-of body
      (extend-env var val1 env))))
}

在传递续文的解释器中，求值@${exp_1}所在的语境应完成计算。所以，在@tt{value-of/k}
中我们写：

@codeblock[#:indent 11]{
(let-exp (var exp1 body)
  (value-of/k exp1 env
    (let-exp-cont var body env cont)))
}

然后给续文的接口添加规范：

@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (let-exp-cont |@${var} |@${body} |@${env} |@${cont}) |@${val})
= (value-of/k |@${body} (extend-env |@${var} |@${val} |@${env}) |@${cont})
}|
}

}

@tt{let}表达式主体的值称为@tt{let}表达式的值，所以求值@tt{let}表达式主体时的续文
与求值整个@tt{let}表达式相同。这是@bold{尾调用不扩大续文}原则的又一例子。

下面我们处理@tt{if}表达式。在@tt{if}表达式中，首先要求值条件，但条件的结果不是整
个表达式的值。我们要新生成一个续文，查看条件表达式的结果是否为真，然后求值真值表
达式或假值表达式。所以在@tt{value-of/k}中我们写：

@nested{
@codeblock[#:indent 11]{
(if-exp (exp1 exp2 exp3)
  (value-of/k exp1 env
    (if-test-cont exp2 exp3 body env cont)))
}

其中，@tt{if-test-cont}是另一个续文构造器，满足如下规范：

@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (if-test-cont |@${exp_2} |@${exp_3} |@${env} |@${cont}) |@${val})
= (if (expval->bool |@${val})
    (value-of/k |@${exp_2} |@${env} |@${cont})
    (value-of/k |@${exp_3} |@${env} |@${cont}))
}|
}
}

现在，我们有了四个续文构造器。我们可以用过程表示法或者数据结构表示法实现它们。过
程表示法如图5.2所示，数据结构表示法使用@tt{define-datatype}，如图5.3所示。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@${\mathit{Cont}} = @${\mathit{ExpVal}} -> @${\mathit{FinalAnswer}}}

@#,elem{@bold{@tt{end-cont}} : @${\mathit{()} \to \mathit{Cont}}}
(define end-cont
  (lambda ()
    (lambda (val)
      (begin
        (eopl:printf "计算结束.~%")
        val))))

@#,elem{@bold{@tt{zero1-cont}} : @${\mathit{Cont} \to \mathit{Cont}}}
(define zero1-cont
  (lambda (cont)
    (lambda (val)
      (apply-cont cont
        (bool-val
          (zero? (expval->num val)))))))

@#,elem{@bold{@tt{let-exp-cont}} : @${\mathit{Var} \times \mathit{Exp} \times \mathit{Env} \times \mathit{Cont} \to \mathit{Cont}}}
(define let-exp-cont
  (lambda (var body env cont)
    (lambda (val)
      (value-of/k body (extend-env var val env) cont))))

@#,elem{@bold{@tt{if-test-cont}} : @${\mathit{Exp} \times \mathit{Exp} \times \mathit{Env} \times \mathit{Cont} \to \mathit{Cont}}}
 : Exp × Exp × Env × Cont → Cont
(define if-test-cont
  (lambda (exp2 exp3 env cont)
    (lambda (val)
      (if (expval->bool val)
        (value-of/k exp2 env cont)
        (value-of/k exp3 env cont)))))

@#,elem{@bold{@tt{apply-cont}} : @${\mathit{Cont} \times \mathit{ExpVal} \to \mathit{FinalAnswer}}}
(define apply-cont
  (lambda (cont v)
    (cont v)))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "用过程表示续文"))]
}

@nested[#:style eopl-figure]{
@racketblock[
(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
    (cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?)))

@#,elem{@bold{@tt{apply-cont}} : @${\mathit{Cont} \times \mathit{ExpVal} \to \mathit{FinalAnswer}}}
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (begin
          (eopl:printf "计算结束.~%")
          val))
      (zero1-cont (saved-cont)
        (apply-cont saved-cont
          (bool-val
            (zero? (expval->num val)))))
      (let-exp-cont (var body saved-env saved-cont)
        (value-of/k body
          (extend-env var val saved-env) saved-cont))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (if (expval->bool val)
          (value-of/k exp2 saved-env saved-cont)
          (value-of/k exp3 saved-env saved-cont))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "用数据结构表示续文"))]
}

下面这个简单算例展示了各部分如何结合在一起。像在@secref{s3.3}那样，我们用
@${<<exp>>}指代表达式@${exp}的抽象语法树。设@${\rho_0}是一环境，@tt{b}在其中绑定
到@tt{(bool-val #t)}；@${cont_0}是初始续文，即@tt{(end-cont)}的值。注释说明应与
@tt{value-of/k}的定义和@tt{apply-cont}的规范对读。这个例子是预测性的，因为我们让
@tt{letrec}引入了过程，但还不知道如何调用它。

@nested[#:style 'code-inset]{
@verbatim|{
(value-of/k <<letrec p(x) = x in if b then 3 else 4>>
  |@${\rho_0} |@${cont_0})
= |@smaller{@emph{令@${\rho_0}为}@tt{(extend-env-rec ... @${\rho_0})}}
(value-of/k <<if b then 3 else 4>> |@${\rho_1} |@${cont_0})
= |@smaller{@emph{然后，求条件表达式的值}}
(value-of/k <<b>> |@${\rho_1} (test-cont <<3>> <<4>> |@${\rho_1} |@${cont_0}))
= |@smaller{@emph{把}@tt{b}@emph{的值传给续文}}
(apply-cont (test-cont <<3>> <<4>> |@${\rho_1} |@${cont_0})
            (bool-val #f))
= |@smaller{@emph{求真值表达式}}
(value-of/k <<3>> |@${\rho_1} |@${cont_0})
= |@smaller{@emph{把表达式的值传给续文}}
(apply-cont |@${cont_0} (num-val 3))
= |@smaller{@emph{在最后的续文中处理最终答案}}
(begin (eopl:printf ...) (num-val 3))
}|
}

差值表达式给我们的解释器带来了新困难，因为它得求两个操作数的值。我们还像@tt{if}
那样开始，先求第一个实参：

@codeblock[#:indent 11]{
(diff-exp (exp1 exp2)
  (value-of/k exp1 env
    (diff1-cont exp2 env cont)))
}

当@tt{(diff1-cont exp2 env cont)}收到一个值，它求值@tt{exp2}时的语境应保存
@tt{exp1}的值。我们将其定义为：

@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (diff1-cont |@${exp_2} |@${env} |@${cont}) |@${val1})
= (value-of/k |@${exp_2} |@${env}
    (diff2-cont |@${val1} |@${cont}))
}|
}

当@tt{(diff2-cont val1 cont)}收到一个值，我们得到了两个操作数的值，所以，我们可
以把二者的差继续传给等待中的@tt{cont}。定义为：

@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (diff2-cont |@${val1} |@${cont}) |@${val2})
= (let ((num1 (expval->num |@${val1}))
        (num2 (expval->num |@${val2})))
    (apply-cont |@${cont}
      (num-val (- num1 num2))))
}|
}

让我们看看该系统的例子。

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(value-of/k
  <<-(-(44,11),3)>>
  |@${\rho_0}
  #(struct:end-cont))
= |@smaller{@emph{开始处理第一个操作数}}
(value-of/k
  <<-(44,11)>>
  |@${\rho_0}
  #(struct:diff1-cont <<3>> |@${\rho_0}
     #(struct:end-cont)))
= |@smaller{@emph{开始处理第一个操作数}}
(value-of/k
  <<44>>
  |@${\rho_0}
  #(struct:diff1-cont <<11>> |@${\rho_0}
     #(struct:diff1-cont <<3>> |@${\rho_0}
        #(struct:end-cont))))
= |@smaller{@emph{把}@tt{<<44>>}的值传给续文}
(apply-cont
  #(struct:diff1-cont <<11>> |@${\rho_0}
    #(struct:diff1-cont <<3>> |@${\rho_0}
       #(struct:end-cont)))
  (num-val 44))
= |@smaller{@emph{现在，开始处理第二个操作数}}
(value-of/k
  <<11>>
  |@${\rho_0}
  #(struct:diff2-cont (num-val 44)
     #(struct:diff1-cont <<3>> |@${\rho_0}
        #(struct:end-cont))))
= |@smaller{@emph{把值传给续文}}
(apply-cont
  #(struct:diff2-cont (num-val 44)
     #(struct:diff1-cont <<3>> |@${\rho_0}
        #(struct:end-cont)))
  (num-val 11))
= |@smaller{@emph{@${44-11}等于@${33}，传给续文}}
(apply-cont
  #(struct:diff1-cont <<3>> |@${\rho_0}
     #(struct:end-cont))
  (num-val 33))
= |@smaller{@emph{开始处理第二个操作数}@tt{<<3>>}}
(value-of/k
  <<3>>
  |@${\rho_0}
  #(struct:diff2-cont (num-val 33)
     #(struct:end-cont)))
= |@smaller{@emph{把值传给续文}}
(apply-cont
  #(struct:diff2-cont (num-val 33)
     #(struct:end-cont))
  (num-val 3))
= |@smaller{@emph{@${33-3}等于@${30}，传给续文}}
(apply-cont
  #(struct:end-cont)
  (num-val 30))
}|
}

@tt{apply-cont}打印出消息“计算结束”，返回计算的最终结果@tt{(num-val 30)}。

}

我们的语言中最后要处理的是过程调用。在传递环境的解释器中，我们写：

@codeblock[#:indent 11]{
(call-exp (rator rand)
  (let ((proc1 (expval->proc (value-of rator env)))
        (arg (value-of rand env)))
    (apply-procedure proc1 arg)))
}

就像在@tt{diff-exp}中一样，这里要处理两个调用。所以我们必须先选其一，然后转换余
下部分，再处理第二个。此外，我们必须把续文传给@tt{apply-procedure}，因为
@tt{apply-procedure}要掉用@tt{value-of/k}。

我们选择先求操作符的值，所以在@tt{value-of/k}中我们写：

@nested{
@codeblock[#:indent 11]{
(call-exp (rator rand)
  (value-of/k rator
    (rator-cont rand env cont)))
}

就像@tt{diff-exp}，@tt{rator-cont}在适当的环境中求值操作数：

@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (rator-cont |@${rand} |@${env} |@${cont}) |@${val1})
= (value-of/k |@${rand} |@${env}
    (rand-cont |@${val1} |@${cont}))
}|
}

当@tt{rand-cont}收到一个值，它就可以调用过程了：

@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (rand-cont |@${val1} |@${cont}) |@${val2})
= (let ((proc1 (expval->proc |@${val1})))
    (apply-procedure/k proc1 |@${val2} |@${cont}))
}|
}

最后，我们还要修改@tt{apply-procedure}，以符合续文传递风格：

@racketblock[
@#,elem{@bold{@tt{apply-procedure/k}} : @${\mathit{Proc} \times \mathit{ExpVal} \times \mathit{Cont} \to \mathit{FinalAnswer}}}
(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of/k body
          (extend-env var val saved-env)
          cont)))))
]
}

传递续文的解释器展示完毕。完整的解释器如图5.4和图5.5所示。续文的完整规范如图5.6
所示。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{FinalAnswer}}}
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env) (end-cont))))))

@#,elem{@bold{@tt{value-of/k}} : @${\mathit{Exp} \times \mathit{Env} \times \mathit{Cont} \to \mathit{FinalAnswer}}}
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (var body)
        (apply-cont cont
          (proc-val
            (procedure var body env))))
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k letrec-body
          (extend-env-rec p-name b-var p-body env)
          cont))
      (zero?-exp (exp1)
        (value-of/k exp1 env
          (zero1-cont cont)))
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env
          (if-test-cont exp2 exp3 env cont)))
      (let-exp (var exp1 body)
        (value-of/k exp1 env
          (let-exp-cont var body env cont)))
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env
          (diff1-cont exp2 env cont)))
      (call-exp (rator rand)
        (value-of/k rator env
          (rator-cont rand env cont))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "传递续文的解释器（第1部分）"))]
}

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{apply-procedure}} : @${\mathit{Proc} \times \mathit{ExpVal} \times \mathit{FinalAnswer} \to \mathit{FinalAnswer}}}
(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of/k body
          (extend-env var val saved-env)
          cont)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "传递续文的解释器（第2部分）"))]
}

现在我们可以验证断言：不是过程调用，而是实际参数的求值扩大了控制语境。具体来说，
如果我们在某个续文@${cont_1}中求过程调用@tt{(@${exp_1} @${exp_2})}的值，求
@${exp_1}得到的过程主体也将在@${cont_1}中求值。

但过程调用本身不会增大控制语境。考虑@tt{(@${exp_1} @${exp_2})}的求值，其中
@${exp_1}的值是一个过程@${proc_1}，@${exp_2}的值是某个表达值@${val_2}。

@nested[#:style 'code-inset]{
@verbatim|{
(value-of/k <<(|@${exp_1} |@${exp_2})>> |@${\rho_1} |@${cont_1})
= |@smaller{@emph{求操作符的值}}
(value-of/k <<|@${exp_1}>> |@${\rho_1}
  (rator-cont <<|@${exp_2}>> |@${\rho_1} |@${cont_1}))
= |@smaller{@emph{把过程值传给续文}}
(apply-cont
  (rator-cont <<|@${exp_2}>> |@${\rho_1} |@${cont_1})
  |@${proc_1})
= |@smaller{@emph{求操作符的值}}
(value-of/k <<|@${exp_2}>> |@${\rho_1}
  (rand-cont <<|@${proc_1}>> |@${cont_1}))
= |@smaller{@emph{把参数值传给续文}}
(apply-cont
  (rand-cont <<|@${proc_1}>> |@${cont_1})
  |@${val_2})
= |@smaller{@emph{调用过程}}
(apply-procedure/k |@${proc_1} |@${val_2} |@${cont_1})
}|
}

所以，过程调用时，主体在调用所在的续文中求值。操作数的求值需要控制语境，进入过程
主体不需要。

@exercise[#:level 1 #:tag "ex5.1"]{

用过程表示法实现续文数据类型。

}

@exercise[#:level 1 #:tag "ex5.2"]{

用数据结构表示法实现续文数据类型。

}

@exercise[#:level 1 #:tag "ex5.3"]{

给解释器添加@tt{let2}。@tt{let2}表达式就像@tt{let}表达式，但要指定两个变量。

}

@exercise[#:level 1 #:tag "ex5.4"]{

给解释器添加@tt{let3}。@tt{let3}表达式就像@tt{let}表达式，但要指定三个变量。

}

@exercise[#:level 1 #:tag "ex5.5"]{

给语言添加练习3.9中的列表。

}

@exercise[#:level 2 #:tag "ex5.6"]{

给语言添加练习3.10中的@tt{list}表达式。提示：添加两个续文构造器，一个用来求列表
首元素的值，一个用来求列表剩余元素的值。

}

@exercise[#:level 2 #:tag "ex5.7"]{

给解释器添加多声明@tt{let}（练习3.16）。

}

@exercise[#:level 2 #:tag "ex5.8"]{

给解释器添加多参数过程（练习3.21）。

}

@exercise[#:level 2 #:tag "ex5.9"]{

修改这个解释器，实现IMPLICIT-REFS语言。提示：添加新的续文构造器@tt{(set-rhs-cont
env var cont)}。

}

@exercise[#:level 2 #:tag "ex5.10"]{

修改前一题的解答，不要在续文中保存环境。

}

@exercise[#:level 2 #:tag "ex5.11"]{

给传递续文的解释器添加练习4.4中的@tt{begin}表达式。确保@tt{value-of}和
@tt{value-of-rands}不在需要生成控制语境的位置调用。

}

@exercise[#:level 1 #:tag "ex5.12"]{

给图5.4-5.6的解释器添加辅助过程，生成类似@elem[#:style question]{150页}计算的输
出。

}

@exercise[#:level 1 #:tag "ex5.13"]{

把@tt{fact}和@tt{fact-iter}翻译为LETREC语言。你可以给语言添加乘法操作符。然后，
用前一道练习中添加了辅助组件解释器计算@tt{(fact 4)}和@tt{(fact-iter 4)}。将它们
和本章开头的计算比较。在@tt{(fact 4)}的跟踪日志中找出@tt{(* 4 (* 3 (* 2 (fact
1))))}。调用@tt{(fact 1)}时，@tt{apply-procedure/k}的续文是什么？

}

@exercise[#:level 1 #:tag "ex5.14"]{

前面练习中的辅助组件产生大量输出。修改辅助组件，只跟踪计算过程中最大续文的
@emph{尺寸}。我们用续文构造器的使用次数衡量续文的大小，所以@elem[#:style
question]{150页}计算中最大续文的尺寸是3。然后，用@tt{fact}和@tt{fact-iter}计算几
个操作数的值。验证@tt{fact}使用的最大续文尺寸随其参数递增，但@tt{fact-iter}使用
的最大续文尺寸是常数。

}

@exercise[#:level 1 #:tag "ex5.15"]{

我们的续文数据类型只有一个常量@tt{end-cont}，所有其他续文构造器都有一个续文参数。
用列表表示和实现续文。用空列表表示@tt{end-cont}，用首项为其他数据结构（名为
@emph{帧} (@emph{frame})或@emph{活跃记录} (@emph{activation record})），余项为已
保存续文的非空列表表示其他续文。观察可知，解释器把这些列表当成（帧的）堆栈。

}

@exercise[#:level 2 #:tag "ex5.16"]{

扩展传递续文的解释器，处理练习4.22中的语言。给@tt{result-of}传递一个续文参数，确
保@tt{result-of}不在扩大控制语境的位置调用。因为语句不返回值，需要区分普通续文和
语句续文；后者通常叫@emph{命令续文} (@emph{command continuation})。续文接口应包
含过程@tt{apply-command-cont}，它取一命令续文并使用它。用数据结构和无参数过程两
种方式实现命令续文。

}

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (end-cont) |@${val})
= (begin
    (eopl:printf
      "计算结束.~%")
    |@${val})

(apply-cont (diff1-cont |@${exp_2} |@${env} |@${cont}) |@${val1})
= (value-of/k |@${exp_2} |@${env} (diff2-cont |@${val1} |@${cont}))

(apply-cont (diff2-cont |@${val1} |@${cont}) |@${val2})
= (let ((num1 (expval->num |@${val1}))
        (num2 (expval->num |@${val2})))
    (apply-cont |@${cont} (num-val (- num1 num2))))

(apply-cont (rator-cont |@${rand} |@${env} |@${cont}) |@${val1})
= (value-of/k |@${rand} |@${env} (rand-cont |@${val1} |@${cont}))

(apply-cont (rand-cont |@${val1} |@${cont}) |@${val2})
= (let ((proc1 (expval->proc |@${val1})))
    (apply-procedure/k proc1 |@${val2} |@${cont}))

(apply-cont (zero1-cont |@${cont}) |@${val})
= (apply-cont |@${cont} (bool-val (zero? (expval->num |@${val}))))

(apply-cont (if-test-cont |@${exp_2} |@${exp_3} |@${env} |@${cont}) |@${val})
= (if (expval->bool |@${val})
    (value-of/k |@${exp_2} |@${env} |@${cont})
    (value-of/k |@${exp_3} |@${env} |@${cont}))

(apply-cont (let-exp-cont |@${var} |@${body} |@${env} |@${cont}) |@${val1})
= (value-of/k |@${body} (extend-env |@${var} |@${val1} |@${env}) |@${cont})
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "图5.4中续文的规范"))]
}

@section[#:tag "s5.2"]{跳跃式解释器}

有人可能想用普通的过程式语言转写解释器，使用数据结构表示续文，从而避免高阶函数。
但是，用大多数过程式语言做这种翻译都很困难：它们不是只在必要时才扩大控制语境，而
是在每个函数调用处扩大控制语境（即堆栈！）。由于我们的系统中，过程调用在计算结束
之前决不返回，系统的堆栈将一直增高，直到那时为止。

这种行为不无道理：在这种语言中，几乎所有的过程调用都出现在赋值语句的右边，所以，
几乎每个过程调用都要扩大控制语境，以便记住待做的乘法。因此，系统结构为这种最常见
的情况进行了优化。而且，大多数语言在堆栈中存储环境信息，所以，每个过程调用生成的
控制语境还要记住移除堆栈上的环境信息。

在这种语言中，一种解决方案是使用@emph{跳跃} (@emph{trampolining})技术。为了避免
产生无限长的调用链，我们把调用链打断，让解释器中的某个过程返回一个无参数过程。这
个过程在调用时继续进行计算。整个计算由一个名叫@emph{跳床} (@emph{trampoline})的
过程驱动，它从一个过程调用跳到另一个。例如，我们可以在@tt{apply-procedure/k}的主
体周围插入一个@tt{(lambda () ...)}，因为在我们的语言中，只要不执行过程调用，表达
式的运行时间不会超出某个界限。

得出的代码如图5.7所示，它还展示了解释器中所有的尾调用。因为我们修改了
@tt{apply-procedure/k}，不再让它返回一个@${ExpVal}，而是返回一个过程，我们得重写
它和它调用的所有过程的合约。因此，我们必须检查解释器中所有过程的合约。

我们从@tt{value-of-program}开始。由于这是调用解释器的过程，它的合约保持不变。它
调用@tt{value-of/k}，把结果传给@tt{trampoline}。因为我们在处理@tt{value-of/k}的
结果，它不是@${FinalAnswer}。我们明明没有修改@tt{value-of/k}的代码，怎么会这样呢？
过程@tt{value-of/k}在尾部递归调用@tt{apply-cont}，@tt{apply-cont}在尾部递归调用
@tt{apply-procedure/k}，所以@tt{apply-procedure/k}的任何结果都可能作为
@tt{value-of/k}的结果。而我们修改了@tt{apply-procedure/k}，其返回值与之前不同。

我们引入@emph{弹珠} (@${Bounce})，作为@tt{value-of/k}的可能结果。（我们叫它弹珠，
因为它是跳床的输入。）这一集合的值是什么呢？@tt{value-of/k}在尾部递归调用它自己
和@tt{apply-cont}，这些是它里面所有的尾递归。所以能成为@tt{value-of/k}结果的值只
能是@tt{apply-cont}的结果。而且，@tt{apply-procedure/k}在尾部递归调用
@tt{value-of/k}，所以不论@${Bounce}是什么，它是@tt{value-of/k}、@tt{apply-cont}
和@tt{apply-procedure/k}结果的集合。

过程@tt{value-of/k}和@tt{apply-cont}只是在尾部调用其他过程。真正把值放入
@${Bound}中的是@tt{apply-procedure/k}。这些是什么样的值呢？我们来看代码。

@racketblock[
(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
      (... (value-of/k body ...)))))
]

已知@tt{apply-procedure/k}返回无参数的过程，该过程在调用时返回一个
@${\mathit{ExpVal}}或调用@tt{value-of/k}、@tt{apply-cont}和
@tt{apply-procedure/k}之一的结果，也就是@${Bounce}。所以，@tt{apply-procedure/k}
可能的取值由如下集合描述：

@nested{
@$${\mathit{ExpVal} \cup (() \to (\mathit{Bounce}))}

这和@tt{value-of/k}的可能结果相同，所以我们得出结论：

@$${\mathit{Bounce} = \mathit{ExpVal} \cup (() \to (\mathit{Bounce}))}

合约为：

@$${
\begin{alignedat}{-1}
&@tt{@bold{value-of-program}} : \mathit{Program} \to \mathit{FinalAnswer} \\
&@tt{@bold{trampoline}} : \mathit{Bounce} \to \mathit{FinalAnswer} \\
&@tt{@bold{value-of/k}} : \mathit{Exp} \times \mathit{Env} \times \mathit{Cont} \to \mathit{Bounce} \\
&@tt{@bold{apply-cont}} : \mathit{Cont} \times \mathit{ExpVal} \to \mathit{Bounce} \\
&@tt{@bold{apply-procedure/k}} : \mathit{Proc} \times \mathit{ExpVal} \times \mathit{FinalAnswer} \to \mathit{Bounce}
\end{alignedat}
}

}

过程@tt{trampoline}满足其合约：首先给它传入一个@${Bounce}。如果其参数是一个
@${ExpVal}（也是@${FinalAnswer}），那么返回；否则，参数一定是一个返回值为
@${Bounce}的过程。所以，它调用这个无参数过程，然后调用自身处理其结果，返回值总是
一个@${Bounce}。（在@elem[#:style question]{7.4节}我们将看到如何自动完成这个推理
过程。）

@tt{apply-procedure/k}返回的每个无参数函数都表示计算过程的一个快照。我们可以在计
算中的不同位置返回这样的快照。在@secref{s5.5}，我们将看到如何用这一思想模拟多线
程程序中的原子操作。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@${\mathit{Bounce}} = @${\mathit{ExpVal} \cup (() \to (\mathit{Bounce}))}}

@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{FinalAnswer}}}
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
        (trampoline
          (value-of/k exp (init-env) (end-cont)))))))

@#,elem{@bold{@tt{trampoline}} : @${\mathit{Bounce} \to \mathit{FinalAnswer}}}
(define trampoline
  (lambda (bounce)
    (if (expval? bounce)
      bounce
      (trampoline (bounce)))))

@#,elem{@bold{@tt{value-of/k}} : @${\mathit{Exp} \times \mathit{Env} \times \mathit{Cont} \to \mathit{Bounce}}}
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (... (value-of/k ...))
      (... (apply-cont ...)))))

@#,elem{@bold{@tt{apply-cont}} : @${\mathit{Cont} \times \mathit{ExpVal} \to \mathit{Bounce}}}
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (... val)
      (... (value-of/k ...))
      (... (apply-cont ...))
      (... (apply-procedure/k ...)))))

@#,elem{@bold{@tt{apply-procedure/k}} : @${\mathit{Proc} \times \mathit{ExpVal} \times \mathit{Cont} \to \mathit{Bounce}}}
(define apply-procedure/k
  (lambda (proc1 val cont)
    (lambda ()
      (cases procedure proc1
        (... (value-of/k ...))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "用过程表示跳床"))]
}

@exercise[#:level 1 #:tag "ex5.17"]{

修改跳跃式解释器，把每个调用@tt{apply-procedure/k}的地方（只有一处）放入
@tt{(lambda () ...)}中。这个修改需要更改合约吗？

}

@exercise[#:level 1 #:tag "ex5.18"]{

图5.7中的跳床系统使用过程表示@${Bounce}。改用数据结构表示法。

}

@exercise[#:level 1 #:tag "ex5.19"]{

不要在@tt{apply-procedure/k}主体周围插入@tt{(lambda () ...)}，改为在
@tt{apply-cont}的主体周围插入。修改合约，使之符合这一更改。@${Bounce}的定义需要
修改吗？然后，用数据结构表示法替换过程表示法表示@${Bounce}，像练习5.18那样。

}

@exercise[#:level 1 #:tag "ex5.20"]{

在练习5.18中，@tt{trampoline}返回@${FinalAnswer}之前的最后一颗弹珠形如
@tt{(apply-cont (end-cont) @${val})}，其中，@${val}是@${ExpVal}。利用这一点优化
练习5.19中弹珠的表示。

}

@exercise[#:level 2 #:tag "ex5.21"]{

用普通的过程式语言实现跳跃式解释器。用练习5.18中的数据结构表示快照，把
@tt{trampoline}中对自身的递归调用替换为普通的@tt{while}或其它循环结构。

}

@exercise[#:level 3 #:tag "ex5.22"]{

有人可能想用普通的过程式语言转写@secref{expr}中传递环境的解释器。同样是因为上述
原因，除了最简单的情况，这种转换都会失败。跳跃技术在这种情况下也有效吗？

}

@section[#:tag "s5.3"]{指令式解释器}

在@secref{state}中我们看到，给共享变量赋值有时可以替代绑定。考虑图5.8顶部的老例
子@tt{even}和@tt{odd}。可以用图5.8中间的程序替代它们。其中，共享变量@tt{x}供两个
过程交换信息。在顶部的例子中，过程主体在环境中查找相关数据；在另一个程序中，它们
从存储器中查找相关数据。

考虑图5.8底部的计算跟踪日志。两个程序的计算跟踪日志都是可以是它。我们记录调用的
过程和实参时，它是第一个计算的跟踪日志；我们记录调用的过程和寄存器@tt{x}的值时，
它是第二个计算的跟踪日志。

而当我们记录程序计数器的位置和寄存器@tt{x}的内容时，这又可以解释为
@emph{goto}（名为流程图程序）的跟踪日志。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
letrec
 even(x) = if zero?(x)
           then 1
           else (odd sub1(x))
 odd(x) = if zero?(x)
          then 0
          else (even sub1(x))
in (odd 13)
}|
}

@${\rule{\linewidth}{1pt}}

@nested[#:style 'code-inset]{
@verbatim|{
let x = 0
in letrec
    even() = if zero?(x)
             then 1
             else let d = set x = sub1(x)
                  in (odd)
    odd() = if zero?(x)
            then 0
            else let d = set x = sub1(x)
                 in (even)
   in let d = set x = 13
      in (odd)
}|
}

@${\rule{\linewidth}{0.5pt}}

@nested[#:style 'code-inset]{
@verbatim|{
      x = 13;
      goto odd;
even: if (x=0) then return(1)
               else {x = x-1;
                     goto odd;}
odd:  if (x=0) then return(0)
               else {x = x-1;
                     goto even;}
}|
}

@${\rule{\linewidth}{0.5pt}}

@nested[#:style 'code-inset]{
@verbatim|{
  (odd 13)
= (even 12)
= (odd 11)
...
= (odd 1)
= (even 0)
= 1
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "跟踪日志相同的三个程序"))]
}

能这样，只是因为原代码中@tt{even}和@tt{odd}的调用不扩大控制语境：它们是尾调用。
我们不能这样转换@tt{fact}，因为@tt{fact}的跟踪日志无限增长：不像这里，“程序计数
器”出现在跟踪日的最外层，而是在控制语境中。

任何不需要控制语境的程序都可以这样转换。这给了我们一条重要原则：

@nested[#:style tip]{
 @centered{@bold{无参数的尾调用等同于跳转。}}
}

如果一组过程只通过尾调用互相调用，那么我们翻译程序，用赋值代替绑定，然后把赋值程
序转译为流程图程序，就像图5.8那样。

本节，我们用这一原则翻译传递续文的解释器，将其转换为适合无高阶过程语言的形式。

我们首先从图5.4和5.5的解释器开始，用数据结构表示续文。续文的数据结构表示如图5.9
和5.10所示。

@nested[#:style eopl-figure]{
@racketblock[
(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
    (saved-cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (diff1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (diff2-cont
    (val1 expval?)
    (saved-cont continuation?))
  (rator-cont
    (rand expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (rand-cont
    (val1 expval?)
    (saved-cont continuation?)))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "用数据结构实现的续文（第1部分）"))]

}

我们的第一个任务是列出需要通过共享寄存器通信的过程。这些过程及其形参为：

@nested{
@codeblock[#:indent 0]{
(value-of/k exp env cont)
(apply-cont cont val)
(apply-procedure/k proc1 val cont)
}
}

所以我们需要五个寄存器：@tt{exp}，@tt{env}，@tt{cont}，@tt{val}和@tt{proc1}。上
面的三个过程各改为一个无参数过程，每个实参的值存入对应的寄存器，调用无参数过程，
换掉上述过程的调用。所以，这段代码

@nested{
@racketblock[
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      ...)))
]

可以替换为：

@racketblock[
(define value-of/k
  (lambda ()
    (cases expression exp
      (const-exp (num)
        (set! cont cont)
        (set! val (num-val num))
        (apply-cont))
      ...)))
]

}

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{apply-cont}} : @${\mathit{Cont} \times \mathit{ExpVal} \to \mathit{Bounce}}}
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (begin
          (eopl:printf
            "计算结束.~%")
          val))
      (zero1-cont (saved-cont)
        (apply-cont saved-cont
          (bool-val
            (zero? (expval->num val)))))
      (let-exp-cont (var body saved-env saved-cont)
        (value-of/k body
          (extend-env var val saved-env) saved-cont))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (if (expval->bool val)
          (value-of/k exp2 saved-env saved-cont)
          (value-of/k exp3 saved-env saved-cont)))
      (diff1-cont (exp2 saved-env saved-cont)
        (value-of/k exp2
          saved-env (diff2-cont val saved-cont)))
      (diff2-cont (val1 saved-cont)
        (let ((num1 (expval->num val1))
              (num2 (expval->num val)))
          (apply-cont saved-cont
            (num-val (- num1 num2)))))
      (rator-cont (rand saved-env saved-cont)
        (value-of/k rand saved-env
          (rand-cont val saved-cont)))
      (rand-cont (val1 saved-cont)
        (let ((proc (expval->proc val1)))
          (apply-procedure/k proc val saved-cont))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "用数据结构实现的续文（第2部分）"))]

}

现在，我们依次转换四个过程。我们还要修改@tt{value-of-program}的主体，因为那是最
初调用@tt{value-of/k}的地方。只有三点小麻烦：

@itemlist[#:style 'ordered

 @item{从一个过程调用到另一个时，常常有存储器保持不变。这就得出应上例中的
 @tt{(set! cont cont)}赋值。我们大可移除这样的赋值。}

 @item{我们必须确保@tt{cases}表达式中的字段不与寄存器重名。否则字段会遮蔽寄存器，
 寄存器就无法访问。例如，在@tt{value-of-program}中，我们如果写：

 @codeblock[#:indent 5]{
 (cases program pgn
   (a-program (exp)
     (value-of/k exp (init-env) (end-cont))))
 }

 那么@tt{exp}绑定到局部变量，我们无法给全局寄存器@tt{exp}赋值。解决方法是重命名
 局部变量，避免冲突：

 @codeblock[#:indent 5]{
 (cases program pgn
   (a-program (exp1)
     (value-of/k exp1 (init-env) (end-cont))))
 }

 然后，可以写：

 @codeblock[#:indent 5]{
 (cases program pgn
   (a-program (exp1)
     (set! cont (end-cont))
     (set! exp exp1)
     (set! env (init-env))
     (value-of/k)))
 }

 我们已仔细挑选数据类型中的字段名，避免这种冲突。

 }

 @item{一次调用中如果两次使用同一寄存器，又会造成一点麻烦。考虑转换@tt{(cons (f
 (car x)) (f (cdr x)))}中的第一个调用，其中，@tt{x}是@tt{f}的形式参数。不假思索
 的话，调用可以转换为：

 @racketblock[
 (begin
   (set! x (car x))
   (set! cont (arg1-cont x cont))
   (f))
 ]

 但这是不对的，因为它给寄存器@tt{x}赋了新值，但@tt{x}原先的值还有用。解决方法是
 调整赋值顺序，把正确的值放入寄存器中，或者，使用临时变量。大多情况下，要避免这
 种问题，可以先给续文变量赋值：

 @racketblock[
 (begin
   (set! cont (arg1-cont x cont))
   (set! x (car x))
   (f))
 ]

 有时临时变量无法避免；考虑@tt{(f y x)}，其中@tt{x}和@tt{y}是@tt{f}的形式参数。
 我们的例子中还没有这种麻烦。}

]

翻译完成的解释器如图5.11-5.14所示。这个过程叫做@emph{寄存}
(@emph{registerization})。很容易把它翻译成支持跳转的命令式语言。

@exercise[#:level 1 #:tag "ex5.23"]{

如果删去解释器某一分支中的“goto”会怎样？解释器究竟出什么错？

}

@exercise[#:level 1 #:tag "ex5.24"]{

设计一些例子，解释上文提到的每个麻烦。

}

@exercise[#:level 2 #:tag "ex5.25"]{

寄存多参数过程的解释器（练习3.21）。

}

@exercise[#:level 1 #:tag "ex5.26"]{

用跳床转换这个解释器，用@tt{(set! pc apply-procedure/k)}替换
@tt{apply-procedure/k}的调用，并使用下面这样的驱动器：

@racketblock[
(define trampoline
  (lambda (pc)
    (if pc (trampoline (pc)) val)))
]

}

@exercise[#:level 1 #:tag "ex5.27"]{

设计一个语言特性，导致最后给@tt{cont}赋值时，必须用临时变量。

}

@exercise[#:level 1 #:tag "ex5.28"]{

给这个解释器添加练习5.12中的辅助组件。由于续文表示方式相同，可以复用那里的代码。
验证本节的命令式解释器生成的跟踪日志与练习5.12中的解释器@emph{完全}相同。

}

@exercise[#:level 1 #:tag "ex5.29"]{

转换本节的@tt{fact-iter}（@elem[#:style question]{139页}）。

}

@exercise[#:level 2 #:tag "ex5.30"]{

修改本节的解释器，让过程使用练习3.28中的动态绑定。提示：像本章这样转换练习3.28中
的解释器；只有原解释器与本节解释器不同的部分，转换后的部分才会不同。像练习5.28那
样给解释器添加辅助组件。观察可知，就像当前状态中只有一个续文，也只有一个压入和弹
出的环境，而且它与续文同时压入弹出。由此可得，动态绑定具有 @emph{动态期限}
(@emph{dynamic extent})：即，形式参数的绑定保留到过程返回为止。词法绑定则与之不
同，牵涉闭包时可以一直保留。

}

@exercise[#:level 1 #:tag "ex5.31"]{

添加全局寄存器，排除本节代码中剩下的@tt{let}表达式。

}

@exercise[#:level 2 #:tag "ex5.32"]{

改进前一题的解答，尽可能减少全局寄存器的数量。不到5个就可以。除了本节解释器中已
经用到的外，不要使用其他数据结构。

}

@exercise[#:level 2 #:tag "ex5.33"]{

把本节的解释器翻译为命令式语言。做两次，一次使用宿主语言中的无参数过程调用，一次
使用@tt{goto}。计算量增加时，这二者性能如何？

}

@exercise[#:level 2 #:tag "ex5.34"]{

如@elem[#:style question]{157页}所述，大多数命令式语言都很难完成这种翻译，因为不
论哪种过程调用它们都要使用堆栈，即使是尾调用。而且，对大型解释器，使用@tt{goto}
链接的程序可能太过庞大，以至编译器无法处理。把本节的解释器翻译为命令式语言，用练
习5.26中的跳跃技术避免这种困难。

}
