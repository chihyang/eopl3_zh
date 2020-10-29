#lang scribble/book
@(require "style.rkt"
          scribble/core)

@title[#:style part-title-style-unnumbered #:tag "glo"]{译名表}

下表列出本书专有名词及其翻译，并对有疑义者略加说明。

@tabular[#:cell-properties '((baseline baseline))
(list
@list["Abstraction boundary" "抽象边界"]
@list["Binary semaphore" "二元信号量"]
@list["Bounce" "弹球，或不译"]
@list["Call by name" "按名调用"]
@list["Call by need" "按需调用"]
@list["Call by reference" "按指调用"]
@list["Call by value and result" "按值和结果调用"]
@list["Call by value" "按值调用"]
@list["Class" "类"]
@list["Clause" "从句，指某个表达式的子表达式"]
@list["Construct"
      @compound-paragraph[
       (make-style #f '())
       (list @para{结构，表示用语言书写表达式的某种特定语法})]]
@list["Contact" "合约"]
@list["Continuation" "续文"]
@list["Continuation-passing style" "续文传递风格，简称 CPS"]
@list["Contravariant" "逆变"]
@list["Control context" "控制上下文"]
@list["Critical region" "关键区域"]
@list["Defined language" "被定语言"]
@list["Defining language" "定义语言"]
@list["Dereference" "解引用"]
@list["Derivation" "推导"]
@list["Dynamic binding" "动态绑定"]
@list["Dynamic dispatch" "动态分发"]
@list["Dynamic extent" "动态期限"]
@list["Effect" "效果、计算效果"]
@list["Exception" "异常"]
@list["Explicit reference" "显式引用"]
@list["Expanded type" "展开类型"]
@list["Flowchart program" "流程图程序"]
@list["Form"
      @compound-paragraph[
       (make-style #f '())
       (list @para{形式，在本书中，这一术语和 construct 意思相近})]]
@list["Formal parameter" "形参"]
@list["Frozen" "冻结"]
@list["Ill typed" "异常类型"]
@list["Implementation language" "定义语言"]
@list["Implementation" "实现"]
@list["Implicit reference" "隐式引用"]
@list["Inheritance" "继承"]
@list["Inlining" "内联"]
@list["Invariant" "不变式"]
@list["Iterative control behavior" "迭代性控制行为"]
@list["Lazy evaluation" "懒求值"]
@list["List" "列表"]
@list["Memoization" "助记法"]
@list["Instance" "实例"]
@list["Member" "成员"]
@list["Message-passing" "消息传递"]
@list["Method" "方法"]
@list["Module" "模块"]
@list["Mutex, mutex exclusion" "互斥锁"]
@list["Occurrence check" "验存"]
@list["Object" "对象"]
@list[@compound-paragraph[
       (make-style #f '())
       (list @para{Object-oriented programming})]
      "面向对象编程"]
@list["Pair" "序对"]
@list["Polymorphic" "多态"]
@list["Pool" "池，特指线程池"]
@list["Pre-emptive scheduling" "抢占式调度"]
@list["Procedure" "过程"]
@list["Quantum" "量子，即时间片"]
@list["Ready queue" "就绪队列"]
@list["Recursive control behavior" "递归性控制行为"]
@list["Register" "寄存器"]
@list["Registerization" "寄存"]
@list["Registerize" "寄存"]
@list["Rule of inference" "推理规则，简称规则"]
@list["Scope" "作用域"]
@list["Scoping" "定界"]
@list["Source language" "源语言"]
@list["Specification" "规范"]
@list["Specifying" "定义"]
@list["Store" "存储器"]
@list["Substitution"
      @compound-paragraph[
       (make-style #f '())
       (list
        @para{代换，代换式（组），视上下文，这一术语有时表示类型推导的动作，有时
              表示动作的结果；表示结果时，有时为单数，有时为复数})]]
@list["Subtyping" "子类型判定"]
@list["Tail call" "尾调用"]
@list["Tail form" "尾式"]
@list["Thawed" "解冻"]
@list["Thread" "线程"]
@list["Thunk" "值箱"]
@list["Time slice" "时间片"]
@list["Trampoline" "跳床"]
@list["Trampolining" "跳跃"]
@list["Type checking" "类型检查"]
@list["Type error" "类型错误"]
@list["Type inference" "类型推导"]
@list["Type structure" "类型结构"]
@list["Typing" "判类"]
@list["Unification" "合一"]
@list["Value restriction" "值约束"]
@list["Well typed" "正常类型"]
@list["car" "首项，或不译"]
@list["cdr" "余项，或不译"]
)]
