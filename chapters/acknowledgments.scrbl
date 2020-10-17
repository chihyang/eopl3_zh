#lang scribble/book
@(require "style.rkt"
          scribble/manual)

@title[#:style part-title-style-unnumbered #:tag "awk"]{致谢}

感谢无数同事同学，他们使用和评论了本书的前两版，又为第三版的漫长构思提供了无价帮
助。特别感激以下诸君的贡献，对他们我们深表谢忱。Oliver Danvy 鼓励我们思考一阶组
合式续文传递算法，并提出了一些饶有趣味的练习。Matthias Felleisen 切中肯綮的分析
改善了一些章节的设计。Amr Sabry 提出了许多有用的建议，在@secref{oac}的草稿中发现
了至少一个极难察觉的问题。Benjamin Pierce 自使用本书第一版授课以来提出了一系列深
入见解，其中大半已为我们采用。Gary Leavens 对第二版的初稿提出了极为细致而珍贵的
评论，包括许多详细的修改建议。Stephanie Weirich 在本书第二版@secref{types}的类型
推导代码中发现了一个不易察觉的问题。Ryan Newton 除了阅读@secref{da}草稿之外，又
承担了一个艰巨任务：为该版的每道练习题推荐难度等级。Chung-chieh Shan 从第三版初
稿开始授课，提供了大量有益的评论。

Kevin Millikin、Arthur Lee、Roger Kirchner、Max Hailperin 和 Erik Hilsdale 使用
了第二版初稿。Will Clinger、Will Byrd、Joe Near 和 Kyle Blocher 使用了本版手稿。
他们的评论弥足珍贵。Ron Garcia、Matthew Flatt、Shriram Krishnamurthi、Steve Ganz、
Gregor Kiczales、Marlene Miller、Galen Williamson、Dipanwita Sarkar、Steven
Bogaerts、Albert Rossi、Craig Citro、Christopher Dutchyn、Jeremy Siek 和 Neil
Ching 也认真阅读并做了评论。

尤其感谢这几位对本书的帮助。感谢 Neil Ching 编制了索引。在我们尝试设计
@tt{define-datatype} 和 @tt{cases} 语法扩展时，Jonathan Sobel 和 Erik Hilsdale
完成了一些原型实现，贡献了很多想法。程序语言小组，尤其是 Matthias Felleisen、
Matthew Flatt、Robby Findler 和 Shriram Krishnamurthi，热心帮忙兼容他们的
DrScheme 系统。Kent Dybvig 开发了极为高效和健壮的 Chez Scheme 实现，本书作者已使
用多年。Will Byrd 在整个过程中都提供了无价帮助。Matthias Felleisen 强烈推荐我们
兼容 DrScheme 的模块系统，这在位于 @url{http://www.eopl3.com} 的实现中显而易见。

特别值得一提的是体贴和关心我们进度的人。George Springer 和 Larry Finkelstein 提
供了无价支持。特别感谢 Bob Prior，我们 MIT 出版社的优秀编辑，是他鼓励我们攻下本
版的写作。同样感谢 Bob 的继任 Ada Brunstein，帮我们流畅过渡到新的编辑。印第安纳
大学信息学院和东北大学计算机信息科学学院为我们进行这一工程创造了环境。Mary
Friedman 在数周的写作谈话中热情招待，大大加快了我们的进程。

我们感谢 Christopher T. Haynes 在前两版中的合作。很遗憾，他已志不在此，没有继续
同我们参与本版的编写。

最后，感谢我们的家人，包容我们写作本书时的热情。谢谢你，Rob、Shannon、Rachel、
Sara，还有 Mary；谢谢你，Rebecca 和 Joshua，Jennifer 和 Stephen，Joshua 和
Georgia，还有 Barbara。

本版筹备良久，我们可能忽视了一路帮助过我们的人。我们为任何忽视道歉。您总在书中看
到这样的话，可能会奇怪为什么有人会这样写。忽视了别人，你当然感到抱歉。但是，当你
有了一众帮手（得一个村子才装得下），你确实有种责任感：任何人都不容忽视。所以，如
果您被忽视了，我们深表歉意。@linebreak{}

@elem[#:style right]{—— D.P.F，M.W.}
