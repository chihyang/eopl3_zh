#lang scribble/book
@(require "style.rkt"
          scribble-math)

@index-prefix

@title[#:style part-title-style-unnumbered #:tag "index"]{索引}

@; collect all the see and seealso here to make sure they appear after all the
@; page numbers
@eopl-index[#:decorator 'seealso #:prefix "Procedural representation" "Action under application"]
@eopl-index[#:decorator 'seealso #:prefix "Abstract data types" "Recursive data types"]
@eopl-index[#:decorator 'seealso #:prefix "Or" "Alternation"]
@eopl-index[#:decorator 'seealso #:prefix "Mutation" "Assignment"]
@eopl-index[#:decorator 'seealso #:prefix "References" "L-values"]
@eopl-index[#:decorator 'seealso #:prefix "Assignment" "Mutation"]
@eopl-index[#:decorator 'seealso #:prefix "Recursive data types" "Abstract data types (ADTs)"]

@eopl-index[#:decorator 'see #:prefix "Method of object" "Member function"]
@eopl-index[#:decorator 'see #:prefix "EXPLICIT-REFS; IMPLICITREFS" "State"]
@eopl-index[#:decorator 'see #:prefix "Binding" "Variable(s)" "binding of"]
@eopl-index[#:decorator 'see #:prefix "Scope of variable declaration" "Variable(s)" "scope of"]

@eopl-translation-block{
@eopl-index-entry-translate["Abstract data types (ADTs)" "抽象数据类型 (ADTs)"]
@eopl-index-entry-translate["Abstraction boundary" "抽象边界"]
@eopl-index-entry-translate["Abstract syntax" "抽象语法"]
@eopl-index-entry-translate["Abstract syntax tree" "抽象语法树"]
@eopl-index-entry-translate["Abstract type" "抽象类型"]
@eopl-index-entry-translate["Accumulator" "累加器"]
@eopl-index-entry-translate["Action under application" "调用时执行的动作"]
@eopl-index-entry-translate["Activation record" "活跃记录表"]
@eopl-index-entry-translate["Actual parameter" "实际参数"]
@eopl-index-entry-translate["Algorithm W" "W 算法"]
@eopl-index-entry-translate["Aliases" "别名"]
@eopl-index-entry-translate["Allocation" "分配"]
@eopl-index-entry-translate[@eopl-index-entry["of objects" "objects"] "对象"]
@eopl-index-entry-translate[@eopl-index-entry["in store" "store"] "存储器"]
@eopl-index-entry-translate["Alternation" "并联"]
@eopl-index-entry-translate["Ancestor class" "祖先类"]
@eopl-index-entry-translate["Antecedent" "前件"]
@eopl-index-entry-translate[@eopl-index-entry[@elem{@tt{a(n)-@${\mathit{type\mbox{-}name}}} constructor} "antypename"]
                            @eopl-index-entry[@elem{@tt{a(n)-@${\mathit{type\mbox{-}name}}} 构造器} "antypename"]]
@eopl-index-entry-translate[@eopl-index-entry[@elem{@tt{apply-} procedures} "applyprocedures"]
                            @eopl-index-entry[@elem{@tt{apply-} 过程} "apply过程"]]
@eopl-index-entry-translate["Argument" "实参"]
@eopl-index-entry-translate["Arrays" "数组"]
@eopl-index-entry-translate["Assignment" "赋值"]
@eopl-index-entry-translate["Association list (a-list)" "关联列表 (a-list)"]
@eopl-index-entry-translate["Auxiliary procedures" "辅助过程"]
@eopl-index-entry-translate["Axiom" "公理"]
}

@print-index

@index-suffix
