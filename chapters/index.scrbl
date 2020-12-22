#lang scribble/book
@(require "style.rkt")

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

@print-index

@index-suffix
