import flowchart;
texpreamble("\usepackage{xeCJK}
\setCJKmainfont[BoldFont={WenQuanYi Micro Hei}, ItalicFont={AR PL UKai CN}]{Adobe Song Std}
\setCJKsansfont{Adobe Song Std}
\setCJKmonofont{Adobe Song Std}
\xeCJKsetup{CJKmath=true, PlainEquation=true}
\usepackage[T1]{fontenc}
");
defaultpen(fontsize(8pt));
unitsize(12pt);
pair pos = (0, 0);
real x_off=5, y_off=4;
real offset = 8;
real ah=4;

block block1=rectangle("前端", pos);
block block2=rectangle("解释器", shift(x_off, 0)*pos);
block block3=rectangle("现实世界", shift(0, y_off)*shift(x_off, 0)*pos,drawpen=invisible);
block block4=rectangle("{\scriptsize\emph{程序文本}}", shift(-x_off+2, y_off*3/5)*pos,drawpen=invisible);
block block5=rectangle("{\scriptsize\emph{语法树}}", shift(x_off/3, y_off*3/5)*pos,drawpen=invisible);
block block6=rectangle("{\scriptsize\emph{答案}}", shift(x_off-1, y_off*2/5)*shift(x_off, 0)*pos,drawpen=invisible);
block block7=rectangle("{\scriptsize\emph{输入{-}输出}}", shift(x_off-2, y_off*4/5)*shift(x_off, 0)*pos,drawpen=invisible);

draw(block1);
draw(block2);
draw(block3);
draw(block4);
draw(block5);
draw(block6);
draw(block7);

add(new void(picture pic, transform t) {
    pair start = (block1.left(t).x - (block2.left(t).x - block1.right(t).x) , block2.left(t).y);
    pair end = (block2.right(t).x + block2.left(t).x - block1.right(t).x, block2.left(t).y);
    pair mid1 = midpoint(start--block1.left(t));
    pair mid2 = midpoint(block1.right(t)--block2.left(t));
    pair mid3 = midpoint(block2.right(t)--end);
    pair mid4 = midpoint(block3.bottom(t)--block2.top(t));
    pair mid5 = midpoint(block4.bottom(t)--mid1);
    pair mid6 = midpoint(block5.bottom(t)--mid2);
    pair mid7 = midpoint(block6.bottom(t)--mid3);
    pair mid8 = midpoint(block7.left(t)--mid4);
    draw(pic, start--block1.left(t), arrow=Arrow(size=ah));
    draw(pic, block1.right(t)--block2.left(t), arrow=Arrow(size=ah));
    draw(pic, block2.right(t)--end, arrow=Arrow(size=ah));
    draw(pic, block3.bottom(t)--block2.top(t), arrow=Arrows(size=ah));

    draw(pic, block4.bottom(t){down}..{right}mid5{right}..{down}mid1, arrow=Arrow(size=ah));
    draw(pic, block5.bottom(t){down}..{right}mid6{right}..{down}mid2, arrow=Arrow(size=ah));
    draw(pic, block6.bottom(t){down}..{left}mid7{left}..{(-1,-1)}mid3, arrow=Arrow(size=ah));
    draw(pic, block7.left(t){left}..{down}mid8{down}..{(-1,-1)}mid4, arrow=Arrow(size=ah));
  });

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
