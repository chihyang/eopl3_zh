import flowchart;
texpreamble("\usepackage{xeCJK}
\setCJKmainfont[BoldFont={WenQuanYi Micro Hei}, ItalicFont={AR PL UKai CN}]{Adobe Song Std}
\setCJKsansfont{Adobe Song Std}
\setCJKmonofont{Adobe Song Std}
\xeCJKsetup{CJKmath=true, PlainEquation=true}
\usepackage[T1]{fontenc}
");
real fsz=8pt;
real dsz=12pt;
defaultpen(fontsize(fsz));
unitsize(dsz);
pair pos = (0, 0);
real x_off=5, y_off=5;
real ah=4;

block block0=rectangle("\tt{\phantom{d}c1\phantom{p}}", pos, drawpen=invisible);
block block1=rectangle("\tt{\phantom{d}d1\phantom{p}}", shift(x_off, 0) * block0.center, drawpen=invisible);
block block2=rectangle("\tt{\phantom{d}c1\phantom{p}}", shift(0, -y_off) * block0.center, drawpen=invisible);
block block3=rectangle("\tt{\phantom{d}d1\phantom{p}}", shift(0, -y_off) * block1.center, drawpen=invisible);
block block4=rectangle("\tt{\phantom{d}c2\phantom{p}}", shift(-x_off, 0) * block2.center, drawpen=invisible);
block block5=rectangle("\tt{\phantom{d}d2\phantom{p}}", shift(x_off, 0) * block3.center, drawpen=invisible);

draw(block0);
draw(block1);
draw(block2);
draw(block3);
draw(block4);
draw(block5);


add(new void(picture pic, transform t) {
    draw(pic, "$f$", align=2N, block0.right(t)--block1.left(t), arrow=Arrow(size=ah));
    draw(pic, "$f$", align=2N, block2.right(t)--block3.left(t), arrow=Arrow(size=ah));
    draw(pic, "$<$", align=2N, block4.right(t)--block2.left(t), arrow=Arrow(size=ah));
    draw(pic, "$<$", align=2N, block3.right(t)--block5.left(t), arrow=Arrow(size=ah));
    block f=rectangle("$f$", pos);
    real off=f.top(t).y-f.bottom(t).y;
    pair start = shift(0,-y_off-2)*midpoint(block0.right(t)--block1.left(t));
    pair end = shift(0,2+off)*midpoint(block2.right(t)--block3.left(t));
    draw(pic, start{(2,-3)}..{(2,-3)}end, arrow=Arrow(size=ah));
  });

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
