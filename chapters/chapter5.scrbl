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
