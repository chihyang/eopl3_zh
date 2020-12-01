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
          scribble/latex-properties
          racket/sandbox)

@title[#:style part-title-style-numbered #:tag "cpi"]{传递续文的解释器}

@eopl-index[#:range-mark 'start "Control context"]
在@secref{expr}，我们用环境的概念探讨绑定行为，建立每部分程序执行的数据上下文。
这里，我们将用类似方式探讨每部分程序执行的@term["control context"]{控制上下文}。
我们将介绍@term["continuation"]{续文} 的概念，用来抽象控制上下文。我们将要编写的
解释器会取一续文参数，从而彰显控制上下文。

考虑下面的Scheme阶乘函数定义。

@eopl-code{
@racketblock[
(define fact
  (lambda (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))
]
}

我们可以用推导建模 @tt{fact} 的计算过程：

@eopl-code{
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

这是阶乘的自然递归定义。每次调用 @tt{fact} 都保证返回值与调用处的 @tt{n} 相乘。
这样，随着计算进行，@tt{fact} 在越来越大的@emph{控制上下文} 中调用。比较这一行为
与下列过程。

@nested{
@eopl-code{
@racketblock[
(define fact-iter
  (lambda (n)
    (fact-iter-acc n 1)))

(define fact-iter-acc
  (lambda (n a)
    (if (zero? n) a (fact-iter-acc (- n 1) (* n a)))))
]
}

用这个定义，我们计算：

@eopl-code{
@verbatim|{
  (|@elemtag["fact-iter"]{}fact-iter 4)
= (fact-iter-acc 4 1)
= (fact-iter-acc 3 4)
= (fact-iter-acc 2 12)
= (fact-iter-acc 1 24)
= (fact-iter-acc 0 24)
= 24
}|
}
}

这里，@tt{fact-iter-acc} 总是在同样的控制上下文内调用：在本例中，是没有任何上下
文。当 @tt{fact-iter-acc} 调用自身时，它在 @tt{fact-iter-acc} 执行的
@exact-elem{“}尾端@exact-elem{”}，除了把返回值作为 @tt{fact-iter-acc} 调用的结
果，不需再做任何保证。我们称之为@term["tail call"]{尾调用}。这样，上述推导中的每
一步都形如 @tt{(fact-iter-acc @${n} @${a})}。

当 @tt{fact} 这样的过程执行时，每次递归调用都要记录额外的控制信息，此信息保留到
调用返回为止。在上面的第一个推导中，这反映了控制上下文的增长。这样的过程呈现
出@term["recursive control behavior"]{递归性控制行为}。

与之相对，@tt{fact-iter-acc} 调用自身时，不需记录额外的控制信息。递归调用发生在
表达式的同一层（上述推导的最外层）反映了这一点。在这种情况下，当递归深度（没有对
应返回的递归调用数目）增加时，系统不需要不断增长的内存存放控制上下文。只需使用有
限内存存放控制信息的过程呈现出@term["iterative control behavior"]{迭代性控制行为}。

为什么这些程序呈现出不同的控制行为呢？在阶乘的递归定义中，过程 @tt{fact}
在@term["operand position"]{操作数位置} 调用。我们需要保存这个调用的上下文，
因为我们需要记住，过程调用执行完毕之后，仍需求出操作数的值，并执行外层调用，在本
例中，是完成待做的乘法。这给出一条重要原则：

@nested[#:style tip]{
 @centered{@bold{不是过程调用，而是操作数的求值导致控制上下文扩大。}}
}

本章，我们学习如何跟踪和操作控制上下文。我们的核心工具是
名为@term["continuation"]{续文} 的数据类型。就像环境是数据上下文的抽象表示，续文
是控制上下文的抽象表示。我们将探索续文，编写直接传递续文参数的解释器，就像之前直
接传递环境参数的解释器。一旦处理了简单情况，我们就能明白如何给语言添加组件，以更
加复杂的方式处理控制上下文，譬如异常和线程。

在@secref{cps}，我们展示如何用转换解释器的技术转换所有程序。我们说以这种方式转换
而得的程序具有@term["continuation-passing style"]{续文传递风格}。@secref{cps}还
展示了续文的其他一些重要应用。
@eopl-index["Continuation-passing style"]
@eopl-index[#:range-mark 'end "Control context"]

@section[#:style section-title-style-numbered #:tag "s5.1"]{传递续文的解释器}

@eopl-index[#:range-mark 'start "Continuations"]
在我们的新解释器中，@tt{value-of} 等主要过程将取第三个参数。这一参数——@emph{续
文}——用来抽象每个表达式求值时的控制上下文。

我们从@figure-ref{fig-5.1}，即@secref{s3.4}中的 LETREC 语言解释器入手。我们把
@tt{value-of-program} 的结果叫做 @${\mathit{FinalAnswer}}，以强调这个表达值是程
序的最终值。

@eopl-figure{
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

@eopl-caption["fig-5.1"]{传递环境的解释器}
}

我们的目标是重写解释器，避免在调用 @tt{value-of} 时产生控制上下文。当控制上下文
需要增加时，我们扩展续文参数，就像在@secref{expr}中，程序产生数据上下文时，扩展
解释器的环境一样。彰显控制上下文，我们就能看到它如何消长。之后，
从@secref{s5.4}到@secref{s5.5}，我们将用它给我们的语言添加新的控制行为。

现在，我们知道环境表示一个从符号到指代值的函数。续文表示什么呢？表达式的续文表示
一个过程，它取表达式的结果，完成计算。所以我们的接口必须包含一个过程
@tt{apply-cont}，它取一续文 @tt{cont}，一个表达值 @tt{val}，完成由 @tt{cont} 指
定的计算。@tt{apply-cont} 的合约为：

@nested{
@eopl-code{
@racketblock[
@#,elem{@${\mathit{FinalAnswer}} = @${\mathit{ExpVal}}}
@#,elem{@bold{@tt{apply-cont}} : @${\mathit{Cont} \times \mathit{ExpVal} \to \mathit{FinalAnswer}}}
]
}

我们把 @tt{apply-cont} 的结果叫做 @${\mathit{FinalAnswer}} 是为了提醒自己，它是
计算最终的值：程序的其他部分都不用它。

}

接口应该包含什么样的续文构造器？随着我们分析解释器，这些续文构造器自会显现。首先，
我们需要一个续文构造器，在不需再对计算值进行操作时生成上下文。我们把这个续文叫做
@tt{(end-cont)}，其定义为：

@nested{
@eopl-equation{
@verbatim|{
(apply-cont (end-cont) |@${val})
= (begin
    (eopl:printf "计算结束.~%")
    |@${val})
}|
}

调用 @tt{(end-cont)} 打印出一条计算结束消息，并返回程序的值。因为
@tt{(end-cont)} 打印出一条消息，我们可以看出它调用了多少次。在正确的计算中，它只
应调用一次。

}

我们把 @tt{value-of-program} 重写为：

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{FinalAnswer}}}
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of/k exp1 (init-env) (end-cont))))))
]
}

现在我们可以写出 @tt{value-of/k}。我们依次考虑 @tt{value-of} 的各个分支。
@tt{value-of} 的前几行只是算出一个值，然后返回，不会再次调用 @tt{value-of}。在传
递续文的解释器中，这些行调用 @tt{apply-cont}，把对应的值传给续文：

@eopl-code{
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
}

目前，@tt{cont} 唯一可能的值是终止续文，但这马上就会改变。很容易看出，如果程序为
上述表达式之一，表达式的值将传给 @tt{end-cont}（通过 @tt{apply-cont}）。

@tt{letrec} 的行为也不复杂：它不调用 @tt{value-of}，而是创建一个新环境，然后在新
环境中求主体的值，主体的值就是整个表达式的值。这表明主体和整个表达式在同样的控制
上下文中执行。因此，主体的值应返回给整个表达式的续文。所以我们写：
@eopl-index["Control context"]

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(letrec-exp (p-name p-var p-body letrec-body)
  (value-of/k letrec-body
    (extend-env-rec p-name p-var p-body env)
    cont))
}
}

这解释了一条通用原则：

@nested[#:style tip]{
 @centered{@bold{尾调用不扩大续文}}

 @para[#:style tip-content]{若 @${exp_1} 的值作为 @${exp_2} 的值返回，则
 @${exp_1} 和@${exp_2} 应在同样的续文中执行。} }

写成这样是不对的：

@nested{
@eopl-code{
@codeblock[#:indent racket-block-offset]{
(letrec-exp (p-name p-var p-body letrec-body)
  (apply-cont cont
    (value-of/k letrec-body
      (extend-env-rec p-name p-var p-body env)
      (end-cont))))
}
}

因为调用 @tt{value-of/k} 是在操作数位置：它要作为 @tt{apply-cont} 的操作数。另外，
由于使用续文 @tt{(end-cont)} 会在计算完成之前打印出计算结束消息，这种错误很容易
排查。

}

接下来我们考虑 @tt{zero?} 表达式。在 @tt{zero?} 表达式中，我们得求出实参的值，然
后返回给依赖该值的续文。所以我们要在新的续文中求实参的值，这个续文会取得返回值，
然后做适当处理。

那么，在 @tt{value-of/k} 中，我们写：

@nested{
@eopl-code{
@codeblock[#:indent racket-block-offset]{
(zero?-exp (exp1)
  (value-of/k exp1 env
    (zero1-cont cont)))
}
}

其中，@tt{(zero1-cont cont)} 是一续文，具有如下性质：

@eopl-equation{
@verbatim|{
(apply-cont (zero1-cont |@${cont}) |@${val})
= (apply-cont |@${cont}
    (bool-val
      (zero? (expval->num |@${val}))))
}|
}

}

就像 @tt{letrec}，我们不能把 @tt{value-of/k} 写成：

@nested{
@eopl-code{
@codeblock[#:indent racket-block-offset]{
(zero?-exp (exp1)
  (let ((val (value-of/k exp1 env (end-cont))))
    (apply-cont cont
      (bool-val
        (zero? (expval->num val))))))
}
}

因为调用 @tt{value-of/k} 是在操作数位置。@tt{let} 声明的右边是在操作数位置，因为
@tt{(let ((@${var} @${exp_1})) @${exp_2})} 等效于 @tt{((lambda (@${var})
@${exp_2}) @${exp_1})}。@tt{value-of/k} 调用的值最终成为 @tt{expval->num} 的操作
数。像之前那样，如果我们运行这段代码，计算结束消息会出现两次：一次在计算中间，一
次在真正结束时。

}

@tt{let} 表达式只比 @tt{zero?} 表达式稍微复杂一点：求出声明右侧的值之后，我们在
适当的扩展环境内求主体的值。原来的 @tt{let} 代码为：

@nested{
@eopl-code{
@codeblock[#:indent racket-block-offset]{
(let-exp (var exp1 body)
  (let ((val1 (value-of exp1 env)))
    (value-of body
      (extend-env var val1 env))))
}
}

在传递续文的解释器中，我们在完成剩余计算的上下文中求 @${exp_1} 的值。所以，在
@tt{value-of/k} 中我们写：

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(let-exp (var exp1 body)
  (value-of/k exp1 env
    (let-exp-cont var body env cont)))
}
}

然后我们给续文的接口添加规范：

@eopl-equation{
@verbatim|{
(apply-cont (let-exp-cont |@${var} |@${body} |@${env} |@${cont}) |@${val})
= (value-of/k |@${body} (extend-env |@${var} |@${val} |@${env}) |@${cont})
}|
}

}

@tt{let} 表达式主体的值成为 @tt{let} 表达式的值，所以求 @tt{let} 表达式主体时的
续文与求整个 @tt{let} 表达式的相同。这是@bold{尾调用不扩大续文}原则的又一例子。

@eopl-index["Conditionals"]
下面我们处理 @tt{if} 表达式。在 @tt{if} 表达式中，我们首先求条件的值，但条件的结
果不是整个表达式的值。我们要新生成一个续文，查看条件表达式的结果是否为真，然后求
真值表达式或假值表达式的值。所以在 @tt{value-of/k} 中我们写：

@nested{
@eopl-code{
@codeblock[#:indent racket-block-offset]{
(if-exp (exp1 exp2 exp3)
  (value-of/k exp1 env
    (if-test-cont exp2 exp3 body env cont)))
}
}

其中，@tt{if-test-cont} 是另一个续文构造器，满足如下规范：

@eopl-equation{
@verbatim|{
(apply-cont (if-test-cont |@${exp_2} |@${exp_3} |@${env} |@${cont}) |@${val})
= (if (expval->bool |@${val})
    (value-of/k |@${exp_2} |@${env} |@${cont})
    (value-of/k |@${exp_3} |@${env} |@${cont}))
}|
}
}

现在，我们有了四个续文构造器。我们可以用过程表示法或者数据结构表示法实现它们。过
程表示法如@figure-ref{fig-5.2} 所示，数据结构表示法使用 @tt{define-datatype}，
如@figure-ref{fig-5.3} 所示。
@eopl-index["Continuations" "data structure representation of"]
@eopl-index["Continuations" "procedural representation of"]
@eopl-index["Data structure representation" @eopl-index-entry["of continuations" "continuations"]]

@eopl-figure{
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

@eopl-caption["fig-5.2"]{用过程表示续文
                         @eopl-index["Continuations" "procedural representation of"]}
}

@eopl-figure{
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

@eopl-caption["fig-5.3"]{用数据结构表示续文
                         @eopl-index["Continuations" "data structure representation of"]
                         @eopl-index["Data structure representation" @eopl-index-entry["of continuations" "continuations"]]}
}

下面这个简单算例展示了各部分如何配合。像 @secref{s3.3}那样，我们用
@${\textnormal{\guillemotleft} exp \textnormal{\guillemotright}} 指代表达式
@${exp} 的抽象语法树。设 @${\rho_0} 是一环境，@tt{b} 在其中绑定到 @tt{(bool-val
#t)}；@${cont_0} 是初始续文，即 @tt{(end-cont)} 的值。注释说明不是正式的，应与
@tt{value-of/k} 的定义和 @tt{apply-cont} 的规范对照阅读。这个例子是预测性的，因
为我们让 @tt{letrec} 引入了过程，但还不知道如何调用它。

@eopl-equation{
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

@eopl-index["Difference expressions"]
差值表达式给我们的解释器带来了新困难，因为它得求两个操作数的值。我们还像 @tt{if}
那样开始，先求第一个实参：

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(diff-exp (exp1 exp2)
  (value-of/k exp1 env
    (diff1-cont exp2 env cont)))
}
}

当 @tt{(diff1-cont exp2 env cont)} 收到一个值，它应求 @tt{exp2} 的值，求值时的上
下文应保存 @tt{exp1} 的值。我们将其定义为：

@eopl-equation{
@verbatim|{
(apply-cont (diff1-cont |@${exp_2} |@${env} |@${cont}) |@${val1})
= (value-of/k |@${exp_2} |@${env}
    (diff2-cont |@${val1} |@${cont}))
}|
}

当 @tt{(diff2-cont val1 cont)} 收到一个值，我们得到了两个操作数的值，所以，我们
可以把二者的差继续传给等待中的 @tt{cont}。定义为：

@eopl-equation{
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
@eopl-computation{
@verbatim|{
|@elemtag["cps-computation"]{}(value-of/k
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

@tt{apply-cont} 打印出消息@exact-elem{“}计算结束@exact-elem{”}，返回计算的最终
结果 @tt{(num-val 30)}。

}

我们的语言中最后要处理的是过程调用。在传递环境的解释器中，我们写：

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(call-exp (rator rand)
  (let ((proc1 (expval->proc (value-of rator env)))
        (arg (value-of rand env)))
    (apply-procedure proc1 arg)))
}
}

就像在 @tt{diff-exp} 中一样，这里要处理两个调用。所以我们必须先择其一，然后转换
余下部分来处理第二个。此外，我们必须把续文传给 @tt{apply-procedure}，因为
@tt{apply-procedure} 要调用 @tt{value-of/k}。

我们选择先求操作符的值，所以在 @tt{value-of/k} 中我们写：

@nested{
@eopl-code{
@codeblock[#:indent racket-block-offset]{
(call-exp (rator rand)
  (value-of/k rator
    (rator-cont rand env cont)))
}
}

就像 @tt{diff-exp}，@tt{rator-cont} 在适当的环境中求操作数的值：

@eopl-equation{
@verbatim|{
(apply-cont (rator-cont |@${rand} |@${env} |@${cont}) |@${val1})
= (value-of/k |@${rand} |@${env}
    (rand-cont |@${val1} |@${cont}))
}|
}

当 @tt{rand-cont} 收到一个值，它就可以调用过程了：

@eopl-equation{
@verbatim|{
(apply-cont (rand-cont |@${val1} |@${cont}) |@${val2})
= (let ((proc1 (expval->proc |@${val1})))
    (apply-procedure/k proc1 |@${val2} |@${cont}))
}|
}

最后，我们还要修改 @tt{apply-procedure}，以符合续文传递风格：

@eopl-code{
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
}

传递续文的解释器展示完毕。完整的解释器如@figure-ref{fig-5.4} 和
@countref{fig-5.5} 所示。续文的完整规范如@figure-ref{fig-5.6} 所示。

@eopl-figure{
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

@eopl-caption["fig-5.4"]{传递续文的解释器（第1部分）
                         @eopl-index["Continuations"]}
}

@eopl-figure{
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

@eopl-caption["fig-5.5"]{传递续文的解释器（第2部分）}
}

@elemtag["tail-call-explain"]{现在我们可以验证断言}：不是过程调用，而是实参的求
值扩大了控制上下文。具体来说，如果我们在某个续文 @${cont_1} 中求过程调用
@tt{(@${exp_1} @${exp_2})} 的值，求 @${exp_1} 得到的过程主体也将在 @${cont_1} 中
求值。

但过程调用本身不会增大控制上下文。考虑 @tt{(@${exp_1} @${exp_2})} 的求值，其中
@${exp_1} 的值是一个过程 @${proc_1}，@${exp_2} 的值是某个表达值 @${val_2}。

@eopl-computation{
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

所以，过程调用时，过程主体在过程调用所在的续文中求值。操作数的求值需要控制上下文，
进入过程主体则不需要。
@eopl-index[#:range-mark 'end "Continuations"]

@exercise[#:level 1 #:tag "ex5.1"]{

@eopl-index[#:suffix @exer-ref-range["ex5.1"] "Continuations" "procedural representation of"]
用过程表示法实现续文数据类型。

}

@exercise[#:level 1 #:tag "ex5.2"]{

@eopl-index[#:suffix @exer-ref-range["ex5.2"] "Continuations" "data structure representation of"]
@eopl-index[#:suffix @exer-ref-range["ex5.2"] "Data structure representation" @eopl-index-entry["of continuations" "continuations"]]
用数据结构表示法实现续文数据类型。

}

@exercise[#:level 1 #:tag "ex5.3"]{

给解释器添加 @tt{let2}。@tt{let2} 表达式就像 @tt{let} 表达式，但要指定两个变量。

}

@exercise[#:level 1 #:tag "ex5.4"]{

给解释器添加 @tt{let3}。@tt{let3} 表达式就像 @tt{let} 表达式，但要指定三个变量。

}

@exercise[#:level 1 #:tag "ex5.5"]{

给语言添加@exercise-ref{ex3.9} 中的列表。

}

@exercise[#:level 2 #:tag "ex5.6"]{

给语言添加@exercise-ref{ex3.10} 中的 @tt{list} 表达式。提示：添加两个续文构造器，
一个用来求列表首元素的值，一个用来求列表剩余元素的值。

}

@exercise[#:level 2 #:tag "ex5.7"]{

给解释器添加多声明的 @tt{let}（@exercise-ref{ex3.16}）。

}

@exercise[#:level 2 #:tag "ex5.8"]{

给解释器添加多参数过程（@exercise-ref{ex3.21}）。

}

@exercise[#:level 2 #:tag "ex5.9"]{

修改这个解释器，实现 IMPLICIT-REFS 语言。提示：添加新的续文构造器
@tt{(set-rhs-cont env var cont)}。

}

@exercise[#:level 2 #:tag "ex5.10"]{

修改前一题的解答，不要在续文中保存环境。

}

@exercise[#:level 2 #:tag "ex5.11"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex5.11"] (eopl-index-entry @elem{@tt{begin} expression} "beginexpression")]
给传递续文的解释器添加@exercise-ref{ex4.4} 中的 @tt{begin} 表达式。确保调用
@tt{value-of} 和 @tt{value-of-rands} 时不需要生成控制上下文。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex5.11"] (eopl-index-entry @elem{@tt{begin} expression} "beginexpression")]

}

@exercise[#:level 1 #:tag "ex5.12"]{

给@figure-ref{fig-5.4}--@countref{fig-5.6} 的解释器添加辅助过程，生成类似
@pageref{cps-computation} 计算的输出。

}

@exercise[#:level 1 #:tag "ex5.13"]{

把 @tt{fact} 和 @tt{fact-iter} 翻译为 LETREC 语言。你可以给语言添加乘法操作符。
然后，用前一道练习中带有辅助组件的解释器计算 @tt{(fact 4)} 和 @tt{(fact-iter 4)}。
将它们和本章开头的计算比较。在 @tt{(fact 4)} 的跟踪日志中找出 @tt{(* 4 (* 3 (* 2
(fact 1))))}。调用 @tt{(fact 1)} 时，@tt{apply-procedure/k} 的续文是什么？

}

@exercise[#:level 1 #:tag "ex5.14"]{

前面练习中的辅助组件产生大量输出。修改辅助组件，只跟踪计算过程中最大续文
的@emph{尺寸}。我们用续文构造器的使用次数衡量续文的大小，所
以@pageref{cps-computation}的计算中，续文最大尺寸是 3。然后，用 @tt{fact} 和
@tt{fact-iter} 计算几个操作数的值。验证 @tt{fact} 使用的续文最大尺寸随其参数递增，
但 @tt{fact-iter} 使用的续文最大尺寸是常数。

}

@exercise[#:level 1 #:tag "ex5.15"]{

@eopl-index[#:suffix @elem{ex. @countref{ex5.15}} "Activation record"]
我们的续文数据类型只有一个常量 @tt{end-cont}，所有其他续文构造器都有一个续文参数。
用列表表示和实现续文。用空列表表示 @tt{end-cont}，用首项为其他数据结构
（名为@term["frame"]{帧} 或@term["activation record"]{活跃记录}），余项为已保存
续文的非空列表表示其他续文。观察可知，解释器把这些列表当成（帧的）堆栈。

}

@exercise[#:level 2 #:tag "ex5.16"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex5.16"] "Command continuations"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex5.16"] "Continuations" "command continuations"]
扩展传递续文的解释器，处理@exercise-ref{ex4.22} 中的语言。给 @tt{result-of} 传递
一个续文参数，确保 @tt{result-of} 不在扩大控制上下文的位置调用。因为语句不返回值，
需要区分普通续文和语句续文；后者通常叫@term["command continuation"]{命令续文}。
续文接口应包含过程 @tt{apply-command-cont}，它取一命令续文并使用它。用数据结构和
无参数过程两种方式实现命令续文。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex5.16"] "Command continuations"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex5.16"] "Continuations" "command continuations"]

}

@eopl-figure{
@eopl-equation{
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

@eopl-caption["fig-5.6"]{@figure-ref{fig-5.4} 中续文的规范}
}

@section[#:style section-title-style-numbered #:tag "s5.2"]{跳跃式解释器}

@eopl-index["Defunctionalization"]
有人可能想用普通的过程式语言转译解释器，使用数据结构表示续文，从而避免高阶函数。
但是，用大多数过程式语言做这种翻译都很困难：@elemtag["imper-reason"]{它们}不只在
必要时才扩大控制上下文，而且在每个函数调用处扩大控制上下文（即堆栈！）。在我们的
系统中，由于过程调用在计算结束之前不返回，在那之前，系统的堆栈将一直增高。

@eopl-index[#:range-mark 'start "Defunctionalization"]
@elemtag["imperative-lang"]{这种行为不无道理}：在这种语言中，几乎所有的过程调用
都出现在赋值语句的右边，所以几乎所有过程调用都要扩大控制上下文，以便记录待完成的
赋值。因此，体系结构为这种最常见的情形做了优化。而且，由于大多数语言在堆栈中存储
环境信息，所有过程调用生成的控制上下文都不能忘了移除这一信息。

在这种语言中，一种解决方案是使用@term["trampolining"]{跳跃} 技术。为了避免产生无
限长的调用链，我们把调用链打断，让解释器中的某个过程返回一个无参数过程。调用这个
过程在将继续进行计算。整个计算由一个名叫@term["trampoline"]{跳床} 的过程驱动，它
从一个过程调用弹射到另一个。例如，我们可以在 @tt{apply-procedure/k}的主体周围插
入一个 @tt{(lambda () ...)}，因为在我们的语言中，只要不执行过程调用，表达式的运
行时间就是有限的。

得出的代码如@figure-ref{fig-5.7} 所示，它也展示了解释器中所有的尾调用。因为我们
修改了 @tt{apply-procedure/k}，不再让它返回一个 @${\mathit{ExpVal}}，而是返回一
个过程，我们得重写它和它所调用所有过程的合约。所以，我们必须检查解释器中所有过程
的合约。

我们从 @tt{value-of-program} 开始。由于这是调用解释器的过程，它的合约保持不变。
它调用 @tt{value-of/k}，把结果传给 @tt{trampoline}。因为我们要操作
@tt{value-of/k} 的结果，所以它不是 @${\mathit{FinalAnswer}}。我们明明没有修改
@tt{value-of/k} 的代码，怎么会这样呢？过程 @tt{value-of/k} 在尾部递归调用
@tt{apply-cont}，@tt{apply-cont} 在尾部递归调用 @tt{apply-procedure/k}，所以
@tt{apply-procedure/k} 的任何结果都可能成为 @tt{value-of/k} 的结果。而我们修改了
@tt{apply-procedure/k}，它的返回值与之前不同。

我们引入@term[@${\mathit{Bounce}}]{弹球}，作为 @tt{value-of/k}的可能结果（我们叫
它弹球，因为它是跳床的输入）。这一集合的值是什么呢？@tt{value-of/k} 在尾部递归调
用自身和 @tt{apply-cont}，这些是它里面所有的尾递归。所以能成为 @tt{value-of/k}结
果的值只能是 @tt{apply-cont} 的结果。而且，@tt{apply-procedure/k} 在尾部递归调用
@tt{value-of/k}，所以不论 @${\mathit{Bounce}} 是什么，它是 @tt{value-of/k}、
@tt{apply-cont}和 @tt{apply-procedure/k} 结果的集合。

过程 @tt{value-of/k} 和 @tt{apply-cont} 只是在尾部调用其他过程。真正把值放入
@${\mathit{Bounce}} 中的是 @tt{apply-procedure/k}。这些是什么样的值呢？我们来看
代码。

@eopl-code{
@racketblock[
(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
      (... (value-of/k body ...)))))
]
}

已知 @tt{apply-procedure/k} 返回无参数的过程，该过程在调用时返回一个
@${\mathit{ExpVal}}，或调用 @tt{value-of/k}、@tt{apply-cont} 或
@tt{apply-procedure/k} 之一的结果，也就是 @${\mathit{Bounce}}。所以，
@tt{apply-procedure/k} 可能的取值由如下集合描述：

@nested{
@$${\mathit{ExpVal} \cup (() \to \mathit{Bounce})}

这和 @tt{value-of/k} 的可能结果相同，所以我们得出结论：

@$${\mathit{Bounce} = \mathit{ExpVal} \cup (() \to \mathit{Bounce})}

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

过程 @tt{trampoline} 满足其合约：首先给它传入一个 @${\mathit{Bounce}}。如果其参
数是一个@${\mathit{ExpVal}}（也是 @${\mathit{FinalAnswer}}），那么返回；否则，参
数一定是一个返回值为 @${\mathit{Bounce}} 的过程。所以，它调用这个无参数过程，然
后调用自身处理其结果，返回值总是一个 @${\mathit{Bounce}}（在 @secref{s7.4}我们将
看到如何自动完成这个推理过程）。

@tt{apply-procedure/k} 返回的每个无参数过程都表示计算流程中的一个快照。我们可以
在计算中的不同位置返回这样的快照。在 @secref{s5.5}，我们将看到如何用这一思想模拟
多线程程序中的原子操作。

@eopl-figure{
@racketblock[
@#,elem{@${\mathit{Bounce}} = @${\mathit{ExpVal} \cup (() \to \mathit{Bounce})}}

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

@eopl-caption["fig-5.7"]{用过程表示跳床}
}
@eopl-index[#:range-mark 'end "Defunctionalization"]

@exercise[#:level 1 #:tag "ex5.17"]{

修改跳跃式解释器，把所有调用 @tt{apply-procedure/k} 的地方（只有一处）放入
@tt{(lambda () ...)} 中。这一修改需要更改合约吗？

}

@exercise[#:level 1 #:tag "ex5.18"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex5.18" "ex5.20"] "Data structure representation" @eopl-index-entry["of trampolining" "trampolining"]]
@figure-ref{fig-5.7} 中的跳床系统使用过程表示 @${\mathit{Bounce}}。改用数据结构
表示法。

}

@exercise[#:level 1 #:tag "ex5.19"]{

不要在 @tt{apply-procedure/k} 主体周围插入 @tt{(lambda () ...)}，改为在
@tt{apply-cont} 的主体周围插入。修改合约，使之符合这一更改。@${\mathit{Bounce}}
的定义需要修改吗？然后，用数据结构表示法替换过程表示法表示 @${\mathit{Bounce}}，
像@exercise-ref{ex5.18} 那样。

}

@exercise[#:level 1 #:tag "ex5.20"]{

在@exercise-ref{ex5.18} 中，@tt{trampoline} 返回 @${\mathit{FinalAnswer}} 之前的
最后一颗弹球形如 @tt{(apply-cont (end-cont) @${val})}，其中，@${val} 是
@${\mathit{ExpVal}}。利用这一点优化@exercise-ref{ex5.19} 中弹球的表示。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex5.18" "ex5.20"] "Data structure representation" @eopl-index-entry["of trampolining" "trampolining"]]

}

@exercise[#:level 2 #:tag "ex5.21"]{

用普通的过程式语言实现跳跃式解释器。用@exercise-ref{ex5.18} 中的数据结构表示快照，
把 @tt{trampoline} 中对自身的递归调用替换为普通的 @tt{while} 或其它循环结构。

}

@exercise[#:level 3 #:tag "ex5.22"]{

@eopl-index[#:suffix @exer-ref-range["ex5.22"] "Defunctionalization"]
有人可能想用普通的过程式语言转译@secref{expr}中传递环境的解释器。同样是因
为@elemref["imper-reason"]{上述}原因，除了最简单的情况，这种转换都会失败。跳跃技
术在这种情况下也有效吗？

}

@section[#:style section-title-style-numbered #:tag "s5.3"]{指令式解释器}

在@secref{state}中我们看到，给共享变量赋值有时可以替代绑定。
考虑@figure-ref{fig-5.8} 顶部的老例子 @tt{even} 和 @tt{odd}。

可以用@figure-ref{fig-5.8} 中间的程序替代它们。其中，共享变量 @tt{x} 供两个过程
交换信息。在顶部的例子中，过程主体在环境中查找相关数据；在另一个程序中，它们从存
储器中查找相关数据。

考虑@figure-ref{fig-5.8} 底部的计算跟踪日志。它可能是二者中任一计算跟踪日志。当
我们记录调用的过程和实参时，它是第一个计算的跟踪日志；当我们记录调用的过程和寄存
器 @tt{x} 的值时，它是第二个计算的跟踪日志。

而当我们记录程序计数器的位置和寄存器 @tt{x} 的内容时，这又可以解释为
@emph{goto}（名为@term["flowchart program"]{流程图程序}）的跟踪日志。

@eopl-figure{
@eopl-code{
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

@eopl-code{
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

@eopl-code{
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

@eopl-code{
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

@eopl-caption["fig-5.8"]{跟踪日志相同的三个程序}
}

@eopl-index["Control context"]
能如此，只是因为原代码中 @tt{even} 和 @tt{odd} 的调用不扩大控制上下文：它们是尾
调用。我们不能这样转换 @tt{fact}，因为 @tt{fact} 的跟踪日志无限增长：
@exact-elem{“}程序计数器@exact-elem{”}不是像这里一样出现在跟踪日志的最外层，而
是出现在控制上下文中。

任何不需要控制上下文的程序都可以这样转换。这给了我们一条重要原理：

@nested[#:style tip]{
 @centered{@bold{无参数的尾调用等同于跳转。}}
}

如果一组过程只通过尾调用互相调用，那么我们可以像像@figure-ref{fig-5.8} 那样，翻
译程序，用赋值代替绑定，然后把赋值程序转译为流程图程序。

本节，我们用这一原理翻译传递续文的解释器，将其转换为适合无高阶过程语言的形式。

我们首先从@figure-ref{fig-5.4} 和 @countref{fig-5.5} 中的解释器开始，用数据结构
表示续文。续文的数据结构表示如@figure-ref{fig-5.9} 和 @countref{fig-5.10} 所示。

@eopl-figure[#:position "!ht"]{
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

@eopl-caption["fig-5.9"]{用数据结构实现的续文（第1部分）
                         @eopl-index["Continuations" "data structure representation of"]
                         @eopl-index["Data structure representation" @eopl-index-entry["of continuations" "continuations"]]}

}

我们的第一个任务是列出需要通过共享寄存器通信的过程。这些过程及其形参为：

@nested{
@eopl-code{
@codeblock[#:indent 0]{
(value-of/k exp env cont)
(apply-cont cont val)
(apply-procedure/k proc1 val cont)
}
}
}

所以我们需要五个寄存器：@tt{exp}、@tt{env}、@tt{cont}、@tt{val} 和 @tt{proc1}。
上面的三个过程各改为一个无参数过程，每个实参的值存入对应的寄存器，调用无参数过程，
换掉上述过程的调用。所以，这段代码

@nested{
@eopl-code{
@racketblock[
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      ...)))
]
}

可以替换为：

@eopl-code{
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

}

@eopl-figure{
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

@eopl-caption["fig-5.10"]{用数据结构实现的续文（第2部分）
                          @eopl-index["Continuations" "data structure representation of"]
                          @eopl-index["Data structure representation" @eopl-index-entry["of continuations" "continuations"]]}

}

现在，我们依次转换四个过程。我们还要修改 @tt{value-of-program} 的主体，因为那是
最初调用 @tt{value-of/k}的地方。只有三点小麻烦：

@itemlist[#:style 'ordered

 @item{存储器在过程调用之间往往保持不变。这对应上例中的赋值 @tt{(set! cont
 cont)}。我们大可移除这样的赋值。}

 @item{我们必须确保 @tt{cases} 表达式中的字段不与寄存器重名。否则字段会遮蔽寄存
 器，寄存器就无法访问了。例如，在 @tt{value-of-program} 中，如果我们写：

 @eopl-code{
 @codeblock[#:indent 5]{
 (cases program pgn
   (a-program (exp)
     (value-of/k exp (init-env) (end-cont))))
 }
 }

 那么 @tt{exp} 绑定到局部变量，我们无法给全局寄存器 @tt{exp} 赋值。解决方法是重
 命名局部变量，避免冲突：

 @eopl-code{
 @codeblock[#:indent 5]{
 (cases program pgn
   (a-program (exp1)
     (value-of/k exp1 (init-env) (end-cont))))
 }
 }

 然后，可以写：

 @eopl-code{
 @codeblock[#:indent 5]{
 (cases program pgn
   (a-program (exp1)
     (set! cont (end-cont))
     (set! exp exp1)
     (set! env (init-env))
     (value-of/k)))
 }
 }

 我们已仔细挑选数据类型中的字段名，避免这种冲突。

 }

 @item{一次调用中如果两次使用同一寄存器，又会造成一点麻烦。考虑转换 @tt{(cons (f
 (car x)) (f (cdr x)))} 中的第一个调用，其中，@tt{x} 是 @tt{f} 的形参。不做过多
 考虑的话，这个调用可以转换为：

 @eopl-code{
 @racketblock[
 (begin
   (set! x (car x))
   (set! cont (arg1-cont x cont))
   (f))
 ]
 }

 但这是不对的，因为它给寄存器 @tt{x} 赋了新值，但 @tt{x} 原先的值还有用。解决方
 法是调整赋值顺序，把正确的值放入寄存器中，或者使用临时变量。大多情况下，要避免
 这种问题，可以先给续文变量赋值：

 @eopl-code{
 @racketblock[
 (begin
   (set! cont (arg1-cont x cont))
   (set! x (car x))
   (f))
 ]
 }

 有时临时变量无法避免；考虑 @tt{(f y x)}，其中 @tt{x} 和 @tt{y} 是 @tt{f} 的形参。
 我们的例子中还未遇到这种麻烦。}

]

翻译完的解释器如@figure-ref{fig-5.11}--@countref{fig-5.14} 所示。这个过程
叫做@term["registerization"]{寄存}。很容易用支持跳转的指令式语言翻译它。

@eopl-figure{
@racketblock[
(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define proc1 'uninitialized)

@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{FinalAnswer}}}
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (set! cont (end-cont))
        (set! exp exp1)
        (set! env (init-env))
        (value-of/k)))))

@#,elem{@bold{@tt{value-of/k}} : @${\mathit{()} \to \mathit{FinalAnswer}}}
@#,elem{@bold{用法} : 依赖寄存器}
@#,elem{@tt{exp} : @${\mathit{Exp}}}
@#,elem{@tt{env} : @${\mathit{Env}}}
@#,elem{@tt{cont} : @${\mathit{Cont}}}
(define value-of/k
  (lambda ()
    (cases expression exp
      (const-exp (num)
        (set! val (num-val num))
        (apply-cont))
      (var-exp (var)
        (set! val (apply-env env var))
        (apply-cont))
      (proc-exp (var body)
        (set! val (proc-val (procedure var body env)))
        (apply-cont))
      (letrec-exp (p-name b-var p-body letrec-body)
        (set! exp letrec-body)
        (set! env (extend-env-rec p-name b-var p-body env))
        (value-of/k))
@#,exact-elem{\begin{comment}}
)))
@#,exact-elem{\end{comment}
\smallskip}
]

@eopl-caption["fig-5.11"]{指令式解释器（第1部分）}

}

@eopl-figure[#:position "!ht"]{
@racketblock[
@#,exact-elem{\smallskip
\begin{comment}}
(((
@#,exact-elem{\end{comment}}
      (zero?-exp (exp1)
        (set! cont (zero1-cont cont))
        (set! exp exp1)
        (value-of/k))
      (let-exp (var exp1 body)
        (set! cont (let-exp-cont var body env cont))
        (set! exp exp1)
        (value-of/k))
      (if-exp (exp1 exp2 exp3)
        (set! cont (if-test-cont exp2 exp3 env cont))
        (set! exp exp1)
        (value-of/k))
      (diff-exp (exp1 exp2)
        (set! cont (diff1-cont exp2 env cont))
        (set! exp exp1)
        (value-of/k))
      (call-exp (rator rand)
        (set! cont (rator-cont rand env cont))
        (set! exp rator)
        (value-of/k)))))
]

@eopl-caption["fig-5.12"]{指令式解释器（第2部分）}

}

@eopl-figure[#:position "!ht"]{
@racketblock[
@#,elem{@bold{@tt{apply-cont}} : @${\mathit{()} \to \mathit{FinalAnswer}}}
@#,elem{@bold{用法} : 读取寄存器}
@#,elem{@tt{cont} : @${\mathit{Cont}}}
@#,elem{@tt{val} : @${\mathit{ExpVal}}}
(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont ()
        (eopl:printf "计算结束.~%")
        val)
      (zero1-cont (saved-cont)
        (set! cont saved-cont)
        (set! val (bool-val (zero? (expval->num val))))
        (apply-cont))
      (let-exp-cont (var body saved-env saved-cont)
        (set! cont saved-cont)
        (set! exp body)
        (set! env (extend-env var val saved-env))
        (value-of/k))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
        (set! cont saved-cont)
        (if (expval->bool val)
          (set! exp exp2)
          (set! exp exp3))
        (set! env saved-env)
        (value-of/k))
@#,exact-elem{\begin{comment}}
)))
@#,exact-elem{\end{comment}
\smallskip}
]

@eopl-caption["fig-5.13"]{指令式解释器（第3部分）}

}

@eopl-figure{
@racketblock[
@#,exact-elem{\smallskip
\begin{comment}}
(((
@#,exact-elem{\end{comment}}
      (diff1-cont (exp2 saved-env saved-cont)
        (set! cont (diff2-cont val saved-cont))
        (set! exp exp2)
        (set! env saved-env)
        (value-of/k))
      (diff2-cont (val1 saved-cont)
        (let ((num1 (expval->num val1))
               (num2 (expval->num val)))
          (set! cont saved-cont)
          (set! val (num-val (- num1 num2)))
          (apply-cont)))
      (rator-cont (rand saved-env saved-cont)
        (set! cont (rand-cont val saved-cont))
        (set! exp rand)
        (set! env saved-env)
        (value-of/k))
      (rand-cont (rator-val saved-cont)
        (let ((rator-proc (expval->proc rator-val)))
          (set! cont saved-cont)
          (set! proc1 rator-proc)
          (set! val val)
          (apply-procedure/k))))))

@#,elem{@bold{@tt{apply-procedure/k}} : @${\mathit{()} \to \mathit{FinalAnswer}}}
@#,elem{@bold{用法} : 依赖寄存器}
@#,elem{@tt{proc1} : @${\mathit{Proc}}}
@#,elem{@tt{val} : @${\mathit{ExpVal}}}
@#,elem{@tt{cont} : @${\mathit{Cont}}}
(define apply-procedure/k
  (lambda ()
    (cases proc proc1
      (procedure (var body saved-env)
        (set! exp body)
        (set! env (extend-env var val saved-env))
        (value-of/k)))))
]

@eopl-caption["fig-5.14"]{指令式解释器（第4部分）}

}

@exercise[#:level 1 #:tag "ex5.23"]{

如果删去解释器某一分支中的@exact-elem{“}goto@exact-elem{”}会怎样？解释器会出什
么错？

}

@exercise[#:level 1 #:tag "ex5.24"]{

设计一些例子，解释上文提到的每个麻烦。

}

@exercise[#:level 2 #:tag "ex5.25"]{

寄存支持多参数过程的解释器（@exercise-ref{ex3.21}）。

}

@exercise[#:level 1 #:tag "ex5.26"]{

用跳床转换这个解释器，用 @tt{(set! pc apply-procedure/k)} 替换
@tt{apply-procedure/k} 的调用，并使用下面这样的驱动器：

@eopl-code{
@racketblock[
(define trampoline
  (lambda (pc)
    (if pc (trampoline (pc)) val)))
]
}

}

@exercise[#:level 1 #:tag "ex5.27"]{

设计一个语言特性，导致最后给 @tt{cont} 赋值时，必须用临时变量。

}

@exercise[#:level 1 #:tag "ex5.28"]{

给本节的解释器添加@exercise-ref{ex5.12} 中的辅助组件。由于续文表示方式相同，可以
复用那里的代码。验证本节的指令式解释器生成的跟踪日志与@exercise-ref{ex5.12} 中的
解释器@emph{完全}相同。

}

@exercise[#:level 1 #:tag "ex5.29"]{

转换本节的 @tt{fact-iter}（@pageref{fact-iter}）。

}

@exercise[#:level 2 #:tag "ex5.30"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex5.30"] "Binding" "extent of"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex5.30"] "Dynamic binding (dynamic scope)"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex5.30"] "Dynamic extent"]
修改本节的解释器，让过程使用@exercise-ref{ex3.28} 中的动态绑定。提示：像本章这样
转换@exercise-ref{ex3.28} 中的解释器；二者不同的部分转换后才会不同。
像@exercise-ref{ex5.28} 那样给解释器添加辅助组件。观察可知，就像当前状态中只有一
个续文，当前状态只会压入或弹出一个环境，而且环境与续文同时压入或弹出。由此我们得
出结论，动态绑定具有@term["dynamic extent"]{动态期限}：即，形参的绑定保留
到过程返回为止。词法绑定则与之不同，绑定包裹在闭包内时可以无限期地保留。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex5.30"] "Binding" "extent of"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex5.30"] "Dynamic binding (dynamic scope)"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex5.30"] "Dynamic extent"]

}

@exercise[#:level 1 #:tag "ex5.31"]{

添加全局寄存器，排除本节代码中剩余的 @tt{let} 表达式。

}

@exercise[#:level 2 #:tag "ex5.32"]{

改进前一题的解答，尽可能减少全局寄存器的数量。不到 5 个就可以。除了本节解释器中
已经用到的，不要使用其他数据结构。

}

@exercise[#:level 2 #:tag "ex5.33"]{

@eopl-index[#:suffix @exer-ref-range["ex5.33" "ex5.34"] "Defunctionalization"]
把本节的解释器翻译为指令式语言。做两次，一次使用宿主语言中的无参数过程调用，一次
使用 @tt{goto}。计算量增加时，这二者性能如何？

}

@exercise[#:level 2 #:tag "ex5.34"]{

如@pageref{imperative-lang}所述，用大多数指令式语言都难以完成这种翻译，因为它们
在所有过程调用中使用堆栈，即使是尾调用。而且，对大型解释器，由 @tt{goto} 链接的
代码可能太过庞大，以致某些编译器无法处理。把本节的解释器翻译为指令式语言，
用@exercise-ref{ex5.26} 中的跳跃技术规避这一难题。

}

@section[#:style section-title-style-numbered #:tag "s5.4"]{异常}

迄今为止，我们只用续文管理语言中的普通控制流。但是续文还能让我们修改控制上下文。让
我们来给我们的语言添加@term["exception handling"]{异常处理}。我们给语言新
增两个生成式：

@nested[#:style small]{
@envalign*{
        \mathit{Expression} &::= @tt{try @m{\mathit{Expression}} catch (@m{\mathit{Identifier}}) @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{try-exp (exp1 var handler-exp)}} \\[5pt]
        \mathit{Expression} &::= @tt{raise @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{raise-exp (exp)}}}
}

@tt{try} 表达式在 @tt{catch} 从句描述的异常处理上下文中求第一个参数的值。如果该
表达式正常返回，它的值就是整个 @tt{try} 表达式的值，异常处理器（即
@tt{handler-exp}）则被移除。

@tt{raise} 表达式求出参数的值，以该值抛出一个异常。这个值会传给最接近的异常处理
器，并绑定到这个处理器的变量。然后，处理器主体将被求值。处理器主体可以返回一个值，
这个值称为对应 @tt{try} 表达式的值；或者，它可以抛出另一个异常，
将异常@term["propagate"]{传播} 出去；这时，该异常会传给第二接近的异常处理器。

这里是一个例子（暂时假设我们给语言添加了字符串）。

@nested{
@eopl-code{
@verbatim|{
let list-index =
     proc (str)
      letrec inner (lst)
       = if null?(lst)
         then raise("ListIndexFailed")
         else if string-equal?(car(lst), str)
              then 0
              else -((inner cdr(lst)), -1)
}|
}

过程 @tt{list-index} 是个咖喱式过程，它取一个字符串，一个字符串列表，返回字符串
在列表中的位置。如果找不到期望的列表元素，@tt{inner} 抛出一个异常，跳过所有待做
的减法，把 @tt{"ListIndexFailed"} 传给最接近的异常处理器。

}

这个异常处理器可以利用调用处的信息对异常做适当处理。

@nested{
@eopl-code{
@verbatim|{
let find-member-number =
     proc (member-name)
      ... try ((list-index member-name) member-list)
            catch (exn)
             raise("CantFindMemberNumber")
}|
}

过程 @tt{find-member-number} 取一字符串，用 @tt{list-index} 找出字符串在列表
@tt{member-name} 中的位置。@tt{find-member-number} 的调用者没办法知道
@tt{list-index}，所以 @tt{find-member-number} 把错误消息翻译成调用者能够理解的异
常。

}

根据程序的用途，还有一种可能是，元素名不在列表中时，@tt{find-member-number} 返回
一个默认值。

@nested{
@eopl-code{
@verbatim|{
let find-member-number =
     proc (member-name)
      ... try ((list-index member-name) member-list)
           catch (exn)
            the-default-member-number
}|
}

在这些程序中，我们忽略了异常的值。在其他情况下，@tt{raise} 传出的值可能包含一部
分可供调用者利用的信息。

}

用传递续文的解释器实现这种异常处理机制直截了当。我们从 @tt{try} 表达式开始。在续
文的数据结构表示中，我们添加两个构造器：

@nested{
@eopl-code{
@codeblock[#:indent racket-block-offset]{
(try-cont
  (var identifier?)
  (handler-exp expression?)
  (env environment?)
  (cont continuation?))
(raise1-cont
  (saved-cont continuation?))
}
}

在 @tt{value-of/k} 中，我们给 @tt{try} 添加下面的从句：

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(try-exp (exp1 var handler-exp)
  (value-of/k exp1 env
    (try-cont var handler-exp env cont)))
}
}

}

求 @tt{try} 表达式主体的值时会发生什么呢？如果主体正常返回，那么这个值应该传给
@tt{try} 表达式的续文，也就是此处的 @tt{cont}：

@eopl-equation{
@verbatim|{
(apply-cont (try-cont |@${var} |@${handler\mbox{-}exp} |@${env} |@${cont}) |@${val})
= (apply-cont |@${cont} |@${val})
}|
}

如果一个异常抛出了呢？首先，我们当然得求出 @tt{raise} 参数的值。

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(raise-exp (exp1)
  (value-of/k exp1 env
    (raise1-cont cont)))
}
}

当 @tt{exp1} 的值返回给 @tt{raise1-cont} 时，我们要查找续文中最接近的异常处理器，
即最上层的 @tt{try-cont} 续文。所以，我们把续文规范写成：

@nested{
@eopl-equation{
@verbatim|{
(apply-cont (raise1-cont |@${cont}) |@${val})
= (apply-handler |@${val} |@${cont})
}|
}

其中，@tt{apply-handler} 是一过程，它找出最接近的异常处理器，然后调用它
（@figure-ref{fig-5.15}）。

}

@eopl-figure[#:position "!t"]{
@racketblock[
@#,elem{@bold{@tt{apply-handler}} : @${\mathit{ExpVal} \times \mathit{Cont} \to \mathit{FinalAnswer}}}
(define apply-handler
  (lambda (val cont)
    (cases continuation cont
      (try-cont (var handler-exp saved-env saved-cont)
        (value-of/k handler-exp
          (extend-env var val saved-env)
          saved-cont))
      (end-cont ()
        (report-uncaught-exception))
      (diff1-cont (exp2 saved-env saved-cont)
        (apply-handler val saved-cont))
      (diff2-cont (val1 saved-cont)
        (apply-handler val saved-cont))
      ...)))
]

@eopl-caption["fig-5.15"]{过程 @tt{apply-handler}}

}

要明白怎样将这些结合到一起，我们考虑用被定语言实现的 @tt{index}。令 @${exp_0} 指
代表达式：

@eopl-code{
@verbatim|{
let index
     = proc (n)
        letrec inner (lst)
          = if null? (lst)
            then raise 99
            else if zero?(-(car(lst), n))
                 then 0
                 else -((inner cdr(lst)), -1)
          in proc (lst)
              try (inner lst)
               catch (x) -1
in ((index 5) list(2, 3))
}|
}

我们从任意环境 @${\rho_0} 和续文 @${cont_0} 开始求 @${exp_0} 的值，只展示计算的
关键部分，并插入注释。

@nested{
@eopl-computation{
@verbatim|{
(value-of/k
  <<let index = ... in ((index 5) list(2, 3))>>
  |@${\rho_0}
  |@${cont_0})
= |@smaller{@emph{执行}@tt{let}@emph{主体}}
(value-of/k
  <<((index 5) list(2, 3))>>
  ((index               |@smaller{@emph{称之为}@${\rho_1}}
     #(struct:proc-val
        #(struct:procedure n <<letrec ...>> |@${\rho_0})))
   (i #(struct:num-val 1))
   (v #(struct:num-val 5))
   (x #(struct:num-val 10)))
  #(struct:end-cont))
= |@smaller{@emph{最后求}@tt{try}@emph{的值}}
(value-of/k
  <<try (inner lst) catch (x) -1>>
  ((lst                 |@smaller{@emph{称之为}@${\rho_{lst=(2 \  3)}}}
     #(struct:list-val
        (#(struct:num-val 2) #(struct:num-val 3))))
   (inner ...)
   (n #(struct:num-val 5))
   |@${\rho_0})
  #(struct:end-cont))
= |@smaller{@emph{在}@tt{try-cont}@emph{续文中求}@tt{try}@emph{主体的值}}
(value-of/k
  <<(inner lst)>>
  |@${\rho_{lst=(2 \  3)}}
  #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
     #(struct:end-cont)))
= |@smaller{@tt{lst}@emph{绑定到}@${(2 \  3)}，@emph{求}@tt{inner}@emph{主体的值}}
(value-of/k
  <<if null?(lst) ...>>
  |@${\rho_{lst=(2 \  3)}}
  #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
     #(struct:end-cont)))
= |@smaller{@emph{求条件的值，进入递归所在的行}}
(value-of/k
  <<-((inner cdr(lst)), -1)>>
  |@${\rho_{lst=(2 \  3)}}
  #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
     #(struct:end-cont)))
= |@smaller{@emph{求}@tt{diff-exp}@emph{第一个参数的值}}
(value-of/k
  <<(inner cdr(lst))>>
  |@${\rho_{lst=(2 \  3)}}
  #(struct:diff1-cont <<-1>> |@${\rho_{lst=(2 \  3)}}
     #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
        #(struct:end-cont))))
= |@smaller{@tt{lst}@emph{绑定到}@${(3)}，@emph{求}@tt{inner}@emph{主体的值}}
(value-of/k
  <<if null?(lst) ...>>
  ((lst #(struct:list-val (#(struct:num-val 3)))) |@smaller{@emph{称之为}@${\rho_{lst=(3)}}}
   (inner ...)
   |@${\rho_0})
  #(struct:diff1-cont <<-1>> |@${\rho_{lst=(2 \  3)}}
     #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
        #(struct:end-cont))))
= |@smaller{@emph{求条件的值，进入递归所在的行}}
(value-of/k
  <<-((inner cdr(lst)), -1)>>
  |@${\rho_{lst=(3)}}
  #(struct:diff1-cont <<-1>> |@${\rho_{lst=(2 \  3)}}
     #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
        #(struct:end-cont))))
= |@smaller{@emph{求}@tt{diff-exp}@emph{第一个参数的值}}
(value-of/k
  <<(inner cdr(lst))>>
  |@${\rho_{lst=(3)}}
  #(struct:diff1-cont <<-1>> |@${\rho_{lst=(2 \  3)}}
     #(struct:diff1-cont <<-1>> |@${\rho_{lst=(2 \  3)}}
        #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
           #(struct:end-cont)))))
= |@smaller{@tt{lst}@emph{绑定到}@${()}，@emph{求}@tt{inner}@emph{主体的值}}
(value-of/k
  <<if null?(lst) ... >>
  ((lst #(struct:list-val ()))     |@smaller{@emph{称之为}@${\rho_{lst=()}}}
   (inner ...)
   (n #(struct:num-val 5))
   ...)
  #(struct:diff1-cont <<-1>> |@${\rho_{lst=(3)}}
     #(struct:diff1-cont <<-1>> |@${\rho_{lst=(2 \  3)}}
        #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
           #(struct:end-cont)))))
= |@smaller{@emph{求}@tt{raise}@emph{表达式参数的值}}
(value-of/k
  <<99>>
  |@${\rho_{lst=()}}
  #(struct:raise1-cont
     #(struct:diff1-cont <<-1>> |@${\rho_{lst=(3)}}
        #(struct:diff1-cont <<-1>> |@${\rho_{lst=(2 \  3)}}
           #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
              #(struct:end-cont))))))

= |@smaller{@emph{用}@tt{apply-handler}@emph{展开续文，直到找出一个异常处理器}}
(apply-handler
  #(struct:num-val 99)
     #(struct:diff1-cont <<-1>> |@${\rho_{lst=(3)}}
        #(struct:diff1-cont <<-1>> |@${\rho_{lst=(2 \  3)}}
           #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
              #(struct:end-cont)))))
=
(apply-handler
  #(struct:num-val 99)
  #(struct:diff1-cont <<-1>> |@${\rho_{lst=(2 \  3)}}
     #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
        #(struct:end-cont))))
=
(apply-handler
  #(struct:num-val 99)
  #(struct:try-cont x <<-1>> |@${\rho_{lst=(2 \  3)}}
     #(struct:end-cont)))
= |@smaller{@emph{找到异常处理器；把异常值绑定到}@tt{x}}
(value-of/k
  #(struct:const-exp -1)
  ((x #(struct:num-val 99))
  |@${\rho_{lst=(2 \  3)}} ...)
  #(struct:end-cont))
=
(apply-cont #(struct:end-cont) #(struct:const-exp -1))
=
#(struct:const-exp -1)
}|
}
}

如果列表包含期望值，那么我们不需调用 @tt{apply-handler}，而是调用
@tt{apply-cont}，并执行续文中所有待完成的 @tt{diff}。


@exercise[#:level 2 #:tag "ex5.35"]{

这种实现很低效，因为异常抛出时，@tt{apply-handler} 必须在续文中线性查找处理器。
让所有续文都能直接使用 @tt{try-cont} 续文，从而避免这种查找。

}

@exercise[#:level 1 #:tag "ex5.36"]{

另一种避免 @tt{apply-handler} 线性查找的设计是使用两个续文，一个正常续文，一个异
常续文。修改本节的解释器，改用两个续文，实现这一目标。

}

@exercise[#:level 1 #:tag "ex5.37"]{

修改被定语言，在过程调用的实参数目错误时抛出异常。

}

@exercise[#:level 1 #:tag "ex5.38"]{

修改被定语言，添加除法表达式，并在被零除时抛出异常。

}

@exercise[#:level 2 #:tag "ex5.39"]{

目前，异常处理器可以重新抛出异常，把它传播出去；或者返回一个值，作为 @tt{try} 表
达式的值。还可以这样设计语言：允许计算从异常抛出的位置继续。修改本节的解释器，在
@tt{raise} 调用处的续文中运行异常处理器的主体，完成这种设计。

}

@exercise[#:level 3 #:tag "ex5.40"]{

把 @tt{raise} 异常处的续文作为第二个参数传递，使被定语言中的异常处理器既能返回也
能继续。这可能需要把续文作为一种新的表达值。为用值调用续文设计恰当的语法。

}

@exercise[#:level 3 #:tag "ex5.41"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex5.41"] "Continuations" "procedural representation of"]
我们展示了如何用数据结构表示的续文实现异常。我们没办法马上用@secref{s2.2.3}中的
步骤得到过程表示法，因为我们现在有两个观测器：@tt{apply-handler} 和
@tt{apply-cont}。用一对过程实现本节的续文：一个单参数过程，表示 @tt{apply-cont}
中续文的动作；一个无参数过程，表示 @tt{apply-handler} 中续文的动作。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex5.41"] "Continuations" "procedural representation of"]

}

@exercise[#:level 2 #:tag "ex5.42"]{

@eopl-index[#:range-mark 'start  #:suffix @exer-ref-range["ex5.42" "ex5.44"]
            (eopl-index-entry @tt{call-with-current-continuation} "Callwithcurrentcontinuation")]
前一道练习只在抛出异常时捕获续文。添加形式 @tt{letcc @${\mathit{Identifier}} in
@${\mathit{Expression}}}，允许在语言中的任意位置捕获续文，其规范为：

@eopl-equation{
@verbatim|{
(value-of/k (letcc |@${var} |@${body}) |@${\rho} |@${cont})
= (value-of/k |@${body} (extend-env |@${var} |@${cont} |@${\rho}) |@${cont})
}|
}

这样捕获的续文可用 @tt{throw} 调用：表达式 @tt{throw @${\mathit{Expression}} to
@${\mathit{Expression}}} 求出两个子表达式的值。第二个表达式应返回一续文，应用于
第一个表达式的值。@tt{throw} 表达式当前的续文则被忽略。

}

@exercise[#:level 2 #:tag "ex5.43"]{

修改前一道练习被定语言中的 @tt{letcc}，把捕获的续文作为一种新的过程，这样就能写
@tt{(@${exp_1} @${exp_2})}，而不必写 @tt{throw @${\mathit{Expression}} to
@${\mathit{Expression}}}。

}

@exercise[#:level 2 #:tag "ex5.44"]{

前面练习里的 @tt{letcc} 和 @tt{throw} 还有一种设计方式，只需给语言添加一个过程。
这个过程在 Scheme 中叫做 @tt{call-with-current-continuation}，它取一个单参数过程
@tt{p}，并给 @tt{p} 传递一个单参数过程，这个过程在调用时，将其参数传递给当前的续
文@tt{cont}。@tt{call-with-current-continuation} 可用 @tt{letcc} 和 @tt{throw}
定义如下：

@eopl-code{
@verbatim|{
let call-with-current-continuation
      = proc (p)
          letcc cont
          in (p proc (v) throw v to cont)
in ...
}|
}

给语言添加 @tt{call-with-current-continuation}。然后写一个翻译器，用只有
@tt{call-with-current-continuation} 的语言翻译具有 @tt{letcc} 和 @tt{throw} 的语
言。
@eopl-index[#:range-mark 'end  #:suffix @exer-ref-range["ex5.42" "ex5.44"]
            (eopl-index-entry @tt{call-with-current-continuation} "Callwithcurrentcontinuation")]
}

@section[#:style section-title-style-numbered #:tag "s5.5"]{线程}

许多编程任务中，可能需要一次进行多项计算。当这些计算作为同一进程的一部分，运行在
同一地址空间，通常称它们为@term["thread"]{线程}。本节，我们将看到如何修改解释器，
模拟多线程程序的执行。

我们的多线程解释器不是做单线程计算，而且维护多个线程。就像本章之前展示的那样，每
个线程包含一项正在进行的计算。线程使用@secref{state}中的赋值，通过共享内存通信。

在我们的系统中，整个计算包含一个线程@term["pool"]{池}。每个
线程@term["running"]{在运行}、@@term["runnable"]{可运行} 或者@term["blocked"]{受
阻塞}。在我们的系统中，一次只能有一个线程在运行。在多 CPU 系统中，可以有若干线程
同时运行。可运行的线程记录在名为@term["ready queue"]{就绪队列}的队列中。还有些线
程因为种种原因未能就绪，我们说这些线程@emph{受阻塞}。本节稍后介绍受阻塞线程。

线程调度由@term["scheduler"]{调度器} 执行，它将就绪队列保存为自身状态的一部分。
此外，它保存一个计时器，当一个线程完成若干步（即线程的@term["time slice"]{时间片}
或@term["quantum"]{量子}）时，它中断线程，将其放入就绪队列中，并从就绪队列中选出
一个新的线程来运行。这叫做@term["pre-emptive scheduling"]{抢占式调度}。

我们的新语言基于 IMPLICIT-REFS，名叫 THREADS。在 THREADS 中，新线程由名为
@tt{spawn} 的结构创建。@tt{spawn} 取一参数，该参数的值应为一个过程。新创建的线程
运行时，给那个过程传递一个未定义的参数。该线程不是立刻运行，而是放入就绪队列中，
轮到它时才运行。@tt{spawn} 的执行只求效果；在我们的系统中，我们为它任选一个返回
值 73。

我们来看这种语言的两个示例程序。@figure-ref{fig-5.16} 定义了一个过程 @tt{noisy}，
它取一个列表，打印出列表的第一个元素，然后递归处理列表的剩余部分。这里，主体中的
表达式创建两个线程，分别打印列表 @tt{[1,2,3,4,5]} 和 @tt{[6,7,8,9,10]}。两个列表
究竟如何穿插取决于调度器；在本例中，在被调度器打断之前，每个线程打印出列表中的两
个元素，

@eopl-figure[#:position "!ht"]{
@eopl-code{
@verbatim|{
test: two-non-cooperating-threads

letrec
  noisy (l) = if null?(l)
              then 0
              else begin print(car(l)); (noisy cdr(l)) end
in
   begin
    spawn(proc (d) (noisy [1,2,3,4,5]));
    spawn(proc (d) (noisy [6,7,8,9,10]));
    print(100);
    33
   end

100
1
2
6
7
3
4
8
9
5
10
正确结果: 33
实际结果: #(struct:num-val 33)
正确
}|
}

@eopl-caption["fig-5.16"]{两个交错运行的线程}

}

@figure-ref{fig-5.17} 展示了一个生产者和一个消费者，由初始值为 0 的缓存相联系。
生产者取一参数 @tt{n}，进入 @tt{wait}，循环 5 次，然后把 @tt{n} 放入缓存。每次进
入 @tt{wait} 循环，它打印一个倒数计时器（以 200s 为单位）的值。消费者取一参数
（但忽略它），进入一循环，等待缓存变成非零值。每次进入循环时，它打印一个计数器
（以 100s 为单位）的值，以展示它等结果等了多久。主线程将生产者放入就绪队列，打印
出 300，并在自身中启动消费者。所以，前两项，300 和 205，分别由主线程和生产者线程
打印。@note{原文为So the first two items, 300 and 205, are printed by the main
thread. 实则205是生产者所在线程打印。}就像前一个例子那样，在被打断之前，消费者线
程和生产者线程各自循环大约两次。

@eopl-figure{
@eopl-code{
@verbatim|{
let buffer = 0
in let producer = proc (n)
        letrec
         wait(k) = if zero?(k)
                   then set buffer = n
                   else begin
                         print(-(k,-200));
                         (wait -(k,1))
                        end
         in (wait 5)
   in let consumer = proc (d)
           letrec busywait (k) = if zero?(buffer)
                                 then begin
                                       print(-(k,-100));
                                       (busywait -(k,-1))
                                      end
                                 else buffer
           in (busywait 0)
      in begin
          spawn(proc (d) (producer 44));
          print(300);
          (consumer 86)
         end

300
205
100
101
204
203
102
103
202
201
104
105
正确结果: 44
实际结果: #(struct:num-val 44)
正确
}|
}

@eopl-caption["fig-5.17"]{由缓存连接的生产者和消费者}

}

实现从 IMPLICIT-REFS 语言传递续文的解释器开始。这与@secref{s5.1}中的类似，只是多
了 IMPLICIT-REFS 中的存储器（当然！），以及@exercise-ref{ex5.9} 中的续文构造器
@tt{set-rhs-cont}。

我们给这个解释器添加一个调度器。调度器状态由四个值组成，接口提供六个过程来操作这
些值，如@figure-ref{fig-5.18} 所示。

@figure-ref{fig-5.19} 展示了本接口的实现。这里 @tt{(enqueue @${q} @${val})} 返回
一队列，除了把 @${val} 放在末尾之外，与 @${q} 相同。@tt{(dequeue @${q} @${f})}
取出队头及剩余部分，将它们作为参数传递给 @${f}。

我们用无参数且返回表达值的 Scheme 过程表示线程：

@nested[#:style small]{
@nested[#:style 'inset]{
@elem{@${\mathit{Thread} = () \to \mathit{ExpVal}}}
}
}

如果就绪队列非空，那么过程 @tt{run-next-thread} 从就绪队列取出第一个线程并运行，
赋予它一个大小为 @tt{the-max-time-slice} 的新时间片。如果还有就绪线程，它还把
@tt{the-ready-queue} 设置为剩余线程队列。如果就绪队列为空，@tt{run-next-thread}
返回 @tt{the-final-answer}，计算至此全部终止。

然后我们来看解释器。@tt{spawn} 表达式在某个续文中求参数的值，这个续文执行时，将
一个新线程放入就绪队列，并将 73 返回给 @tt{spawn} 的调用者。新的线程执行时，将一
个任意值（这里选 28）传给 @tt{spawn} 参数求值得到的过程。要完成这些，我们给
@tt{value-of/k} 新增从句：

@nested{
@eopl-code{
@codeblock[#:indent racket-block-offset]{
(spawn-exp (exp)
  (value-of/k exp env
    (spawn-cont cont)))
}
}

给 @tt{apply-cont} 新增从句：

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(spawn-cont (saved-cont)
  (let ((proc1 (expval->proc val)))
    (place-on-ready-queue!
      (lambda ()
        (apply-procedure/k proc1
          (num-val 28)
          (end-subthread-cont))))
    (apply-cont saved-cont (num-val 73))))
}
}

@; TODO: format for interface in figure 5.18
@eopl-figure[#:position "!ht"]{

@big-bracket[#:title "调度器的内部状态"]{
@tabular[#:row-properties (list (make-table-row-skip "2pt")) #:sep @hspace[1]
(list (list @tt{the-ready-queue} "就绪队列")
      (list @tt{the-final-answer} "主线程结束时的值")
      (list @tt{the-max-time-slice} "每个线程运行的步数")
      (list @tt{the-time-remaining} "当前运行线程剩余的步数"))]
}

@big-bracket[#:title "调度器的接口"]{
@tabular[#:row-properties (list (make-table-row-skip "2pt")) #:sep @hspace[1]
(list (list @tt{@bold{initialize-scheduler!}} @${: \mathit{Int} \to \mathit{Unspecified}})
      (list "" "初始化调度器状态")
      (list @tt{@bold{place-on-ready-queue!}} @${: \mathit{Thread} \to \mathit{Unspecified}})
      (list "" "把线程放入就绪队列")
      (list @tt{@bold{run-next-thread}} @${: \mathit{()} \to \mathit{FinalAnswer}})
      (list "" "运行下一个线程。如果没有就绪线程，返回最终答案。")
      (list @tt{@bold{set-final-answer!}} @${: \mathit{ExpVal} \to \mathit{Unspecified}})
      (list "" "设置最终答案")
      (list @tt{@bold{time-expired?}} @${: \mathit{()} \to \mathit{Bool}})
      (list "" "判断计时器是否为0")
      (list @tt{@bold{decrement-timer!}} @${: \mathit{()} \to \mathit{Unspecified}})
      (list "" @elem{递减@tt{the-time-remaining}}))]
}

@eopl-caption["fig-5.18"]{调度器的状态和接口}

}

@eopl-figure{
@racketblock[
@#,elem{@bold{@tt{initialize-scheduler!}} : @${\mathit{Int} \to \mathit{Unspecified}}}
(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)))

@#,elem{@bold{@tt{place-on-ready-queue!}} : @${\mathit{Thread} \to \mathit{Unspecified}}}
(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue
      (enqueue the-ready-queue th))))

@#,elem{@bold{@tt{run-next-thread}} : @${\mathit{()} \to \mathit{FinalAnswer}}}
(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
        (begin
          (when (debug-mode?)
            (eopl:printf "计算结束.~%"))
          the-final-answer)
        (begin
          (when (debug-mode?)
            (eopl:printf "切换到另一线程.~%"))
          (dequeue the-ready-queue
            (lambda (first-ready-thread other-ready-thread)
              (set! the-ready-queue other-ready-thread)
              (set! the-time-remaining the-max-time-slice)
              (first-ready-thread)))))))

@#,elem{@bold{@tt{set-final-answer!}} : @${\mathit{ExpVal} \to \mathit{Unspecified}}}
(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)))

@#,elem{@bold{@tt{time-expired?}} : @${\mathit{ExpVal} \to \mathit{Bool}}}
(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))

@#,elem{@bold{@tt{decrease-timer!}} : @${\mathit{()} \to \mathit{Unspecified}}}
(define decrease-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))))
]

@eopl-caption["fig-5.19"]{调度器}

}

@eopl-figure{
@eopl-code{
@verbatim|{
let x = 0
in let mut = mutex()
   in let incr_x = proc (id)
                    proc (dummy)
                     set x = -(x,-1)
      in begin
          spawn((incr_x 100));
          spawn((incr_x 200));
          spawn((incr_x 300))
         end
}|
}

@eopl-caption["fig-5.20"]{不安全的计数器}

}

跳跃式解释器生成快照时也要做这样：它将计算打包（这里的 @tt{(lambda ()
(apply-procedure/k ...))}），然后把它传给另一个过程处理。在跳床的例子中，我们把
线程传给跳床，后者直接执行前者。这里，我们把新线程放入就绪队列，继续我们的现有计
算。

}

这带来一个关键问题：每个线程应在什么续文中运行？

@itemlist[

 @item{运行主线程的续文应记录主线程的值，作为最终答案，然后运行剩余线程。}

 @item{子线程结束时无法报告它的值，所以运行子线程的续文应忽略其值，然后直接运行
 剩余线程。}

]

由此我们得出两种新续文，其行为由 @tt{apply-cont} 中的以下几行实现：

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(end-main-thread-cont ()
  (set-final-answer! val)
  (run-next-thread))

(end-subthread-cont ()
  (run-next-thread))
}
}

我们从 @tt{value-of-program} 入手整个系统：

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Int} \times \mathit{Program} \to \mathit{FinalAnswer}}}
(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp1)
        (value-of/k
          exp1
          (init-env)
          (end-main-thread-cont))))))
]
}

最后，我们修改 @tt{apply-cont}，让它在每次调用时递减计时器。如果计时器到期，那就
中止当前计算。在实现时，我们先把一个线程放入就绪队列，它用调用
@tt{run-next-thread} 时恢复的计时器再次调用 @tt{apply-cont}。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{apply-cont}} : @${\mathit{Cont} \times \mathit{ExpVal} \to \mathit{FinalAnswer}}}
(define apply-cont
  (lambda (cont val)
    (if (time-expired?)
      (begin
        (place-on-ready-queue!
          (lambda () (apply-cont cont val)))
        (run-next-thread))
      (begin
        (decrement-timer!)
        (cases continuation cont
          ...)))))
]
}

共享变量不是可靠的通信方式，因为多个线程可能试图写同一变量。例如，
考虑@figure-ref{fig-5.20} 中的程序。这里，我们创建了三个线程，试图累加同一个计数
器 @tt{x}。如果一个线程读取了计数器，但在更新计数器之前被打断，那么两个线程将把
计数器设置成同样的值。因此，计数器可能变成 2，甚至是 1，而不是 3。

我们想要确保不会发生这种混乱。同样地，我们想要组织我们的程序，
避免@figure-ref{fig-5.17} 中的程序空转。恰恰相反，它应该能够进入休眠状态，并在生
产者向共享缓存插入值时唤醒。

@eopl-index[#:range-mark 'start "Binary semaphore"]
有许多方式设计这类同步组件。一种简单的方式是使用@term['("mutex exclusion"
"mutex")]{互斥锁} 或@term["binary semaphore"]{二元信号量}。

互斥锁可能@term["open"]{打开} 或@term["closed"]{关闭}。它还包含一个等待互斥锁打
开的线程队列。互斥锁有三种操作：

@itemlist[

 @item{@tt{mutex}，无参数操作，创建一个初始状态为打开的互斥锁。}

 @item{@tt{wait}，单参数操作，线程用它表明想要访问某一互斥锁。它的参数必须是一把
 互斥锁。它的行为取决于互斥锁的状态。

 @itemlist[

  @item{若互斥锁关闭，当前线程放入互斥锁的等待队列中，并中止。我们说当前线
  程@emph{受阻塞}，等待这把互斥锁。}

  @item{若互斥锁打开，则将其关闭，并让当前线程继续执行。}

 ]

 @tt{wait}的执行只求效果；它的返回值未定义。
 }

 @item{@tt{signal}，单参数操作，线程用它表明准备释放某一互斥锁。它的参数必须是一
 把互斥锁。

 @itemlist[

  @item{若互斥锁关闭，且等待队列中没有线程，则将其打开，并让当前线程继续执行。}

  @item{若互斥锁关闭，且等待队列中仍有线程，则从中选出一个线程，放入调度器的等待
  队列，互斥锁则保持关闭。执行 @tt{signal} 的线程继续计算。}

  @item{若互斥锁打开，则保持打开，线程继续执行。}

 ]

 @tt{signal}的执行只求效果；它的返回值未定义。@tt{signal} 操作总是成功：执行它的
 线程仍然是运行线程。

 }

]

@eopl-index[#:range-mark 'start "Critical region"]
这些属性保证在一对连续的 @tt{wait} 和 @tt{signal} 之间，只有一个线程可以执行。这
部分程序叫做@term["critical region"]{关键区域}。在关键区域内，两个线程不可能同时
执行。例如，@figure-ref{fig-5.21} 展示了我们之前的例子，只是在关键行周围插入了一
把互斥锁。在这个程序中，一次只有一个线程可以执行 @tt{set x = -(x,-1)}；所以计数
器一定能够到达终值 3。
@eopl-index[#:range-mark 'end "Critical region"]

@eopl-figure{
@eopl-code{
@verbatim|{
let x = 0
in let mut = mutex()
   in let incr_x = proc (id)
                    proc (dummy)
                      begin
                        wait(mut);
                        set x = -(x,-1);
                        signal(mut)
                      end
      in begin
          spawn((incr_x 100));
          spawn((incr_x 200));
          spawn((incr_x 300))
         end
}|
}

@eopl-caption["fig-5.21"]{使用互斥锁的安全计数器}

}

我们用两个引用模拟互斥锁：一个指向其状态（开启或关闭），一个指向等待这把锁的线程
列表。我们还把互斥锁作为一种表达值。

@eopl-code{
@racketblock[
(define-datatype mutex mutex?
  (a-mutex
    (ref-to-closed? reference?)
    (ref-to-wait-queue reference?)))
]
}

我们给 @tt{value-of/k} 添加适当的行：

@nested{
@eopl-code{
@codeblock[#:indent racket-block-offset]{
(mutex-exp ()
  (apply-cont cont (mutex-val (new-mutex))))
}
}

其中：

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{new-mutex}} : @${\mathit{()} \to \mathit{Mutex}}}
(define new-mutex
  (lambda ()
    (a-mutex
     (newref #f)
     (newref '()))))
]
}
}

@tt{wait} 和 @tt{signal} 作为新的单参数操作，只是调用过程 @tt{wait-for-mutex} 和
@tt{signal-mutex}。@tt{wait} 和 @tt{signal} 都求出它们唯一参数的值，所以，在
@tt{apply-cont} 中我们写：

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(wait-cont
  (saved-cont)
  (wait-for-mutex
    (expval->mutex val)
    (lambda () (apply-cont saved-cont (num-val 52)))))

(signal-cont
  (saved-cont)
  (signal-mutex
    (expval->mutex val)
    (lambda () (apply-cont saved-cont (num-val 53)))))
}
}

现在，我们可以写出 @tt{wait-for-mutex} 和 @tt{signal-mutex}。这些过程取两个参数：
一个互斥锁，一个线程，其工作方式如上所述（@figure-ref{fig-5.22}）。

@eopl-figure{
@racketblock[
@#,elem{@bold{@tt{wait-for-mutex}} : @${\mathit{Mutex} \times \mathit{Thread} \to \mathit{FinalAnswer}}}
@#,elem{@bold{用法} : @tt{等待互斥锁开启，然后关闭它}}
(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
        (cond
          ((deref ref-to-closed?)
            (setref! ref-to-wait-queue
              (enqueue (deref ref-to-wait-queue) th))
            (run-next-thread))
          (else
            (setref! ref-to-closed? #t)
            (th)))))))

@#,elem{@bold{@tt{signal-mutex}} : @${\mathit{Mutex} \times \mathit{Thread} \to \mathit{FinalAnswer}}}
(define signal-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
        (let ((closed? (deref ref-to-closed?))
              (wait-queue (deref ref-to-wait-queue)))
          (if closed?
            (if (empty? wait-queue)
              (begin
                (setref! ref-to-closed? #f)
                (th))
              (begin
                (dequeue
                  wait-queue
                  (lambda (first-waiting-th other-waiting-ths)
                    (place-on-ready-queue!
                      first-waiting-th)
                    (setref!
                      ref-to-wait-queue
                      other-waiting-ths)))
                (th)))
            (th)))))))
]

@eopl-caption["fig-5.22"]{@tt{wait-for-mutex} 和 @tt{signal-mutex}
                          @eopl-index["Binary semaphore"]}
}

@eopl-index[#:range-mark 'end "Binary semaphore"]

@exercise[#:level 1 #:tag "ex5.45"]{

给本节的语言添加形式 @tt{yield}。线程不论何时执行 @tt{yield}，都将自身放入就绪队
列之中，就绪队列头部的线程接着执行。当线程继续时，就好像调用 @tt{yield} 返回了
99。

}

@exercise[#:level 2 #:tag "ex5.46"]{

在@exercise-ref{ex5.45} 的系统中，线程放入就绪队列，既可能是因为耗尽时间片，也可
能是因为它选择让步。在后一种情况下，线程会以一个完整的时间片重启。修改系统，让就
绪队列记录每个线程的剩余时间片（如果有的话），在线程重启时仍使用剩余的时间片。

}

@exercise[#:level 1 #:tag "ex5.47"]{

如果剩余两个子线程，二者都在等待另一个子线程持有的互斥锁会怎样？

}

@exercise[#:level 1 #:tag "ex5.48"]{

@eopl-index[#:suffix @exer-ref-range["ex5.48"] "Data structure representation" @eopl-index-entry["of threads" "threads"]]
我们用过程表示线程。将其改为数据结构表示法。

}

@exercise[#:level 1 #:tag "ex5.49"]{

@eopl-index[#:suffix @elem{"ex."@countref{ex5.49}} "Activation record"]
为 THREADS 完成@exercise-ref{ex5.15}（用堆栈上的帧表示续文）。

}

@exercise[#:level 2 #:tag "ex5.50"]{

寄存本节的解释器。必须寄存的互递归尾调用过程有哪些？

}

@exercise[#:level 3 #:tag "ex5.51"]{

我们想要组织我们的程序，避免@figure-ref{fig-5.17} 中的程序空转。恰恰相反，它应该
能够进入休眠状态，并在生产者向共享缓存插入值时唤醒。用具有互斥锁的程序完成这些，
或者实现一种同步操作符完成这些。

}

@exercise[#:level 3 #:tag "ex5.52"]{

写出使用互斥锁的程序，如@figure-ref{fig-5.21}，但主线程等待所有三个子线程结束，
然后返回 @tt{x} 的值。

}

@exercise[#:level 3 #:tag "ex5.53"]{

修改线程的表示，添加@term["thread identifier"]{线程描述符}。每个新线程都有一个新
的线程描述符。当子线程创建时，传给它的不是本节中的任意值 28，而是它的线程描述符。
子线程的描述符也作为 @tt{spawn} 表达式的值返回给父线程。给解释器添加辅助组件，跟
踪线程描述符的创建。验证就绪队列中一个线程描述符至多出现一次。子线程如何获知父线
程的描述符？原程序的线程描述符应如何处理？

}

@exercise[#:level 2 #:tag "ex5.54"]{

给@exercise-ref{ex5.53} 的解释器添加组件 @tt{kill}。@tt{kill} 结构取一线程号，在
就绪队列或所有等待队列中找出对应的线程，然后删除它。此外，当它找到目标线程时，返
回真；在所有队列中都找不到指定线程号时，返回假。

}

@exercise[#:level 2 #:tag "ex5.55"]{

给@exercise-ref{ex5.53} 的解释器添加线程通信组件，一个线程可以用另一线程的描述符
给它发送一个值。线程可以选择接收消息，没有线程给它发消息时可以阻塞。

}

@exercise[#:level 2 #:tag "ex5.56"]{

修改@exercise-ref{ex5.55} 的解释器，不要使用共享存储器，而是让每个线程具有自己的
存储器。在这种语言中，几乎可以排除互斥锁。重写本节语言的示例程序，但不用互斥锁。

}

@exercise[#:level 2 #:tag "ex5.57"]{

在你最爱的操作系统教材中，有各种各样的同步机制。挑出三种，在本节的框架下实现它们。

}

@exercise[#:level 1 #:tag "ex5.58"]{

（绝对@${@exercise-level-mark}）和朋友吃些披萨吧，但是一人一次一定只拿一块！

}
