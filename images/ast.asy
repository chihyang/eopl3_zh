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
real w = 3;
real l = 18;
real x_off=5, y_off=3;
real ah=4;

block block1=rectangle("\texttt{lambda-exp}", pos);
block block2=rectangle("\texttt{x}", shift(-x_off, -y_off)*pos);
block block3=rectangle("\texttt{app-exp}", shift(x_off, -y_off)*pos);
block block4=rectangle("\texttt{var-exp}", shift(-x_off, -y_off)*shift(x_off, -y_off)*pos);
block block5=rectangle("\texttt{f}", shift(0,-1.5*y_off)*shift(-x_off, -y_off)*shift(x_off, -y_off)*pos);
block block6=rectangle("\texttt{app-exp}", shift(x_off, -y_off)*shift(x_off, -y_off)*pos);
block block7=rectangle("\texttt{var-exp}", shift(-x_off, -y_off)*shift(x_off, -y_off)*shift(x_off, -y_off)*pos);
block block8=rectangle("\texttt{f}", shift(0,-y_off)*shift(-x_off, -y_off)*shift(x_off, -y_off)*shift(x_off, -y_off)*pos);
block block9=rectangle("\texttt{var-exp}", shift(x_off, -y_off)*shift(x_off, -y_off)*shift(x_off, -y_off)*pos);
block block10=rectangle("\texttt{f}", shift(0,-y_off)*shift(x_off, -y_off)*shift(x_off, -y_off)*shift(x_off, -y_off)*pos);

draw(block1);
draw(block2);
draw(block3);
draw(block4);
draw(block5);
draw(block6);
draw(block7);
draw(block8);
draw(block9);
draw(block10);

Label Lbvar = Label("\texttt{bound-var}", align=(0,0), position=MidPoint, filltype=Fill(white));
Label Lbd = Label("\texttt{body}", align=(0,0), position=MidPoint, filltype=Fill(white));
Label Lrtor = Label("\texttt{rator}", align=(0,0), position=MidPoint, filltype=Fill(white));
Label Lrand = Label("\texttt{rand}", align=(0,0), position=MidPoint, filltype=Fill(white));
Label Lvar = Label("\texttt{var}", align=(0,0), position=MidPoint, filltype=Fill(white));

add(new void(picture pic, transform t) {
    draw(pic, Lbvar, block1.bottom(t)--block2.top(t), arrow=Arrow(size=3));
    draw(pic, Lbd, block1.bottom(t)--block3.top(t), arrow=Arrow(size=ah));
    draw(pic, Lrtor, block3.bottom(t)--block4.top(t), arrow=Arrow(size=ah));
    draw(pic, Lvar, block4.bottom(t)--block5.top(t), arrow=Arrow(size=ah));
    draw(pic, Lrand, block3.bottom(t)--block6.top(t), arrow=Arrow(size=ah));
    draw(pic, Lrtor, block6.bottom(t)--block7.top(t), arrow=Arrow(size=ah));
    draw(pic, Lvar, block7.bottom(t)--block8.top(t), arrow=Arrow(size=ah));
    draw(pic, Lrand, block6.bottom(t)--block9.top(t), arrow=Arrow(size=ah));
    draw(pic, Lvar, block9.bottom(t)--block10.top(t), arrow=Arrow(size=ah));
  });

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
