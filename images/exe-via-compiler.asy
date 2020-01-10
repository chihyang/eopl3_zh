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

block block0=rectangle("前端", pos);
block block1=rectangle("编译器", shift(x_off, 0)*pos);
block block2=rectangle("解释器", shift(x_off, 0)*shift(x_off, 0)*pos);
block block3=rectangle("现实世界", shift(0, y_off)*shift(x_off, 0)*shift(x_off, 0)*pos,drawpen=invisible);
block block4=rectangle("{\scriptsize\emph{程序文本}}", shift(-x_off+2, y_off*3/5)*pos,drawpen=invisible);
block block5=rectangle("{\scriptsize\emph{语法树}}", shift(x_off/3, y_off*3/5)*pos,drawpen=invisible);
block block6=rectangle("{\scriptsize\emph{译得程序}}", shift(x_off/3, y_off*3/5)*shift(x_off, 0)*pos,drawpen=invisible);
block block7=rectangle("{\scriptsize\emph{答案}}", shift(x_off-1, y_off*2/5)*shift(x_off, 0)*shift(x_off, 0)*pos,drawpen=invisible);
block block8=rectangle("{\scriptsize\emph{输入{-}输出}}", shift(x_off-2, y_off*4/5)*shift(x_off, 0)*shift(x_off, 0)*pos,drawpen=invisible);
block block9=rectangle("或机器", shift(0, -y_off)*shift(x_off, 0)*shift(x_off, 0)*pos,drawpen=invisible);

draw(block0);
draw(block1);
draw(block2);
draw(block3);
draw(block4);
draw(block5);
draw(block6);
draw(block7);
draw(block8);
draw(block9);

add(new void(picture pic, transform t) {
    pair start = (block0.left(t).x - (block1.left(t).x - block0.right(t).x) , block1.left(t).y);
    pair end = (block2.right(t).x + block2.left(t).x - block1.right(t).x, block2.left(t).y);
    pair mid0 = midpoint(start--block0.left(t));
    pair mid1 = midpoint(block0.right(t)--block1.left(t));
    pair mid2 = midpoint(block1.right(t)--block2.left(t));
    pair mid3 = midpoint(block2.right(t)--end);
    pair mid4 = midpoint(block3.bottom(t)--block2.top(t));
    pair mid5 = midpoint(block4.bottom(t)--mid0);
    pair mid6 = midpoint(block5.bottom(t)--mid1);
    pair mid7 = midpoint(block6.bottom(t)--mid2);
    pair mid8 = midpoint(block7.bottom(t)--mid3);
    pair mid9 = midpoint(block8.left(t)--mid4);
    pair mid10 = midpoint(block9.top(t)--block2.bottom(t));
    real len = (block2.bottom(t).y - block9.top(t).y) / 2;
    draw(pic, start--block0.left(t), arrow=Arrow(size=ah));
    draw(pic, block0.right(t)--block1.left(t), arrow=Arrow(size=ah));
    draw(pic, block1.right(t)--block2.left(t), arrow=Arrow(size=ah));
    draw(pic, block2.right(t)--end, arrow=Arrow(size=ah));
    draw(pic, block3.bottom(t)--block2.top(t), arrow=Arrows(size=ah));
    draw(pic, block9.top(t){(-2,len)}..{up}(mid10.x-3, mid10.y){up}{(2,len)}..{up}block2.bottom(t), arrow=Arrow(size=ah));

    draw(pic, block4.bottom(t){down}..{right}mid5{right}..{down}mid0, arrow=Arrow(size=ah));
    draw(pic, block5.bottom(t){down}..{right}mid6{right}..{down}mid1, arrow=Arrow(size=ah));
    draw(pic, block6.bottom(t){down}..{right}mid7{right}..{down}mid2, arrow=Arrow(size=ah));
    draw(pic, block7.bottom(t){down}..{left}mid8{left}..{(-1,-1)}mid3, arrow=Arrow(size=ah));
    draw(pic, block8.left(t){left}..{down}mid9{down}..{(-1,-1)}mid4, arrow=Arrow(size=ah));
  });

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
