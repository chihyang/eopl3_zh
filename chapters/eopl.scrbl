#lang scribble/book
@(require "style.rkt"
          "bibliography.rkt")

@title[#:style '(toc)]{编程语言要素}
@author{Daniel P. Friedman}
@author{Mitchell Wand}

@table-of-contents[]

@include-section["foreword.scrbl"]
@include-section["preface.scrbl"]
@include-section["acknowledgments.scrbl"]

@include-section["chapter1.scrbl"]
@include-section["chapter2.scrbl"]
@include-section["chapter3.scrbl"]
@include-section["chapter4.scrbl"]
@include-section["chapter5.scrbl"]
@include-section["chapter6.scrbl"]
@include-section["chapter7.scrbl"]
@include-section["chapter8.scrbl"]
@include-section["chapter9.scrbl"]

@include-section["appendix_a.scrbl"]
@include-section["appendix_b.scrbl"]

@generate-bibliography[#:sec-title "参考书目"]
