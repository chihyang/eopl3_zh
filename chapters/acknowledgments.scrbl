#lang scribble/book
@(require scribble/manual)
@(require "../style/style.rkt")

@title[#:style 'unnumbered #:tag "awk"]{鸣谢}

感谢无数同事同学，他们使用和评论了本书的前两版，又为第三版的漫长筹备提供了无价帮
助。特别感激以下诸君的贡献，对他们我们深表谢忱。Oliver Danvy鼓励我们思考一阶组合
式续文传递算法，并提出了一些饶有趣味的练习。Matthias Felleisen切中肯綮的分析改善
了一些章节的设计。Amr Sabry提出了许多有用的建议，在第九章的草稿中发现了至少一个
极难察觉的问题。Benjamin Pierce自使用本书第一版授课以来提出了一系列卓有见地的观
察，其中大半已为我们采用。Gary Leavens对第二版的初稿提出了极为深入而珍贵的评论，
包括大量详细的修改建议。Stephanie Weirich在本书第二版第七章的类型推导代码中发现
了一个不易察觉的问题。Ryan Newton除了阅读第二章草稿之外，又承担了一个艰巨任务：
为该版的每道练习题推荐难度等级。单中杰从第三版初稿开始授课，提供了大量有益的评论。

Kevin Millikin、Arthur Lee、Roger Kirchner、Max Hailperin和Erik Hilsdale都用了第
二版的初稿。Will Clinger、Will Byrd、Joe Near和Kyle Blocher都使用了本版草稿。他
们的评论弥足珍贵。Ron Garcia、Matthew Flatt、Shriram Krishnamurthi、Steve Ganz、
Gregor Kiczales、Marlene Miller、Galen Williamson、Dipanwita Sarkar、Steven
Bogaerts、Albert Rossi、Craig Citro、Christopher Dutchyn、Jeremy Siek和Neil
Ching也认真阅读并进行了评论。

一些人为本书帮助我们，值得特别感谢。我们感谢Neil Ching编制了索引。在我们尝试设计
@racket[define-datatype] 和 @racket[cases] 语法扩展时，Jonathan Sobel和Erik
Hilsdale完成了一些原型实现，贡献了很多想法。编程语言小组，尤其是Matthias
Felleisen、Matthew Flatt、Robby Findler和Shriram Krishnamurthi，热心帮助提供和
DrScheme系统的兼容性。Kent Dybvig开发了极为高效和健壮的Chez Scheme实现，本书的作
者已使用多年。Will Byrd在整个过程中都提供了无价帮助。Matthias Felleisen强烈推荐
我们兼容DrScheme的模块系统，这在位于 @url{http://www.eopl3.com} 的实现中显而易见。

一些人因他们的深思和对我们健康的关心值得特别提及。George Springer和Larry
Finkelstein提供了无价的支持。Bob Prior，我们MIT出版社的优秀编辑，因他鼓励我们攻
下本版的写作而值得特别感谢。Ada Brunstein，Bob的继任，使我们流畅过渡到新的编辑，
也值得我们特别感谢。印第安纳大学信息学院和东北大学计算机信息科学学院为我们进行这
一工程创造了环境。Mary Friedman在数周的写作谈话中热情招待，大大加快了我们的进程。

我们感谢Christopher T. Haynes在前两版中的合作。很不幸地，他已志不在此，没有继续
同我们参与本版的编写。

最后，感谢我们的家人，包容我们完成写作本书时的热情。谢谢你，Rob、Shannon、Rachel、
Sara，还有Mary；谢谢你，Rebecca和Joshua，Jennifer和Stephen，Joshua和Georgia，还
有，Barbara。

本版筹备良久，我们可能忽视了一路帮助过我们的人。我们为任何忽视道歉。读者总在书中
看到这样的话，可能会奇怪为什么有人会这样写。当然，你得为任何忽视道歉。但是，当你
有了一众帮手（得一个村子才装得下），你确实能感受到不忽视任何人的责任感。所以，如
果您被忽视了，我们深表歉意。@linebreak{}

@elem[#:style right]{—— D.P.F，M.W.}
