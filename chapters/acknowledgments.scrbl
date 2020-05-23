#lang scribble/book
@(require "style.rkt"
          scribble/manual)

@title[#:style part-title-style-unnumbered #:tag "awk"]{致谢}

感谢无数同事同学，他们使用和评论了本书的前两版，又为第三版的漫长筹备提供了无价帮
助。特别感激以下诸君的贡献，对他们我们深表谢忱。Oliver Danvy鼓励我们思考一阶组合
式续文传递算法，并提出了一些饶有趣味的练习。Matthias Felleisen切中肯綮的分析改善
了一些章节的设计。Amr Sabry提出了许多有用的建议，在@secref{oac}的草稿中发现了至
少一个极难察觉的问题。Benjamin Pierce自使用本书第一版授课以来提出了一系列深入见
解，其中大半已为我们采用。Gary Leavens对第二版的初稿提出了极为细致而珍贵的评论，
包括许多详细的修改建议。Stephanie Weirich在本书第二版@secref{types}的类型推导代
码中发现了一个不易察觉的问题。Ryan Newton除了阅读@secref{da}草稿之外，又承担了一
个艰巨任务：为该版的每道练习题推荐难度等级。单中杰从第三版初稿开始授课，提供了大
量有益的评论。

Kevin Millikin、Arthur Lee、Roger Kirchner、Max Hailperin和Erik Hilsdale使用了第
二版初稿。Will Clinger、Will Byrd、Joe Near和Kyle Blocher使用了本版手稿。他们
的评论弥足珍贵。Ron Garcia、Matthew Flatt、Shriram Krishnamurthi、Steve Ganz、
Gregor Kiczales、Marlene Miller、Galen Williamson、Dipanwita Sarkar、Steven
Bogaerts、Albert Rossi、Craig Citro、Christopher Dutchyn、Jeremy Siek和Neil
Ching也认真阅读并做了评论。

尤其感谢这几位对本书的帮助。感谢Neil Ching编制了索引。在我们尝试设计
@racket[define-datatype]和@racket[cases]语法扩展时，Jonathan Sobel和Erik
Hilsdale完成了一些原型实现，贡献了很多想法。程序语言小组，尤其是Matthias
Felleisen、Matthew Flatt、Robby Findler和Shriram Krishnamurthi，热心帮忙兼容他们
的DrScheme系统。Kent Dybvig开发了极为高效和健壮的Chez Scheme实现，本书作者已使用
多年。Will Byrd在整个过程中都提供了无价帮助。Matthias Felleisen强烈推荐我们兼容
DrScheme的模块系统，这在位于 @url{http://www.eopl3.com} 的实现中显而易见。

特别值得一提的是体贴和关心我们状态的人。George Springer和Larry Finkelstein提供了
无价支持。特别感谢Bob Prior，我们MIT出版社的优秀编辑，是他鼓励我们攻下本版的写作。
同样感谢Ada Brunstein，Bob的继任，使我们流畅过渡到新的编辑。印第安纳大学信息学院
和东北大学计算机信息科学学院为我们进行这一工程创造了环境。Mary Friedman在数周的
写作谈话中热情招待，大大加快了我们的进程。

我们感谢Christopher T. Haynes在前两版中的合作。很不幸，他已志不在此，没有继续同
我们参与本版的编写。

最后，感谢我们的家人，包容我们写作本书时的热情。谢谢你，Rob、Shannon、Rachel、
Sara，还有Mary；谢谢你，Rebecca和Joshua，Jennifer和Stephen，Joshua和Georgia，还
有Barbara。

本版筹备良久，我们可能忽视了一路帮助过我们的人。我们为任何忽视道歉。您总在书中看
到这样的话，可能会奇怪为什么有人会这样写。当然，你会为任何忽视道歉。但是，当你有
了一众帮手（得装满一村），你确实有种责任感：任何人都不容忽视。所以，如果您被忽视
了，我们深表歉意。@linebreak{}

@elem[#:style right]{—— D.P.F，M.W.}
