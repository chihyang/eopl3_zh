texpreamble("\usepackage{xeCJK}
\setCJKmainfont[BoldFont={WenQuanYi Micro Hei}, ItalicFont={AR PL UKai CN}]{Adobe Song Std}
\setCJKsansfont{Adobe Song Std}
\setCJKmonofont{Adobe Song Std}
\xeCJKsetup{CJKmath=true, PlainEquation=true}
\usepackage[T1]{fontenc}
\usepackage{listings}
");
defaultpen(fontsize(10pt));
unitsize(12pt);
real w = 1.2;
real l = 1.2;
real offset = 2*w+l/2;
path a_box = box((0, 0), (l, w));
path a_arrow = (l/2,w/2)--(l/2,-w*1);
path b_arrow = (l*1.5,w/2)--(l*4,w/2);
real ah=4;

draw(shift(0)*a_box);
draw(shift(1*l)*a_box);
draw(shift(0, -2*w)*a_box);
draw(shift(1*l, -2*w)*a_box);

draw(shift(4*l)*shift(0)*a_box);
draw(shift(4*l)*shift(1*l)*a_box);
draw(shift(4*l)*shift(0, -2*w)*a_box);
draw(shift(4*l)*shift(1*l, -2*w)*a_box);

draw(a_arrow,arrow=Arrow(size=ah));
draw((l*0.5,-w*1.5)--(l*0.5,-w*3), arrow=Arrow(size=ah));
draw((l*1.5,-w*1.5)--(l*1.5,-w*3), arrow=Arrow(size=ah));

draw(shift(4*l)*a_arrow,arrow=Arrow(size=ah));
draw(shift(4*l)*b_arrow,arrow=Arrow(size=ah));
draw((l*1.5,w/2)--shift(4*l)*(0,w/2),arrow=Arrow(size=ah));
draw(shift(4*l)*(l*0.5,-w*1.5)--shift(4*l)*(l*0.5,-w*3), arrow=Arrow(size=ah));
draw(shift(4*l)*(l*1.5,-w*1.5)--shift(4*l)*(l*1.5,-w*3), arrow=Arrow(size=ah));

draw(shift(0,2*l)*a_arrow,arrow=Arrow(size=ah));

label("$\mathit{saved\mbox{-}env}$",shift(4*l)*(l*4,w/2),align=E);
label("$\phantom{\mathit{l}}$\tt{y}$\phantom{\mathit{l}}$",(l*0.5,-w*3),align=S);
label("$\mathit{val_2}$",(l*1.5,-w*3),align=S);
label("$\phantom{\mathit{l}}$\tt{x}$\phantom{\mathit{l}}$",shift(4*l)*(l*0.5,-w*3),align=S);
label("$\mathit{val_1}$",shift(4*l)*(l*1.5,-w*3),align=S);

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
