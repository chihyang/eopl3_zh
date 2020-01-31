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
path b_arrow = (l*1.5,w/2)--(l*3.5,w/2);
real ah=4;

draw(shift(0)*a_box);
draw(shift(1*l)*a_box);

draw(shift(3.5*l)*a_box);
draw(shift(3.5*l)*shift(1*l)*a_box);

draw(shift(3.5*l)*shift(3.5*l)*shift(0)*a_box);
draw(shift(3.5*l)*shift(3.5*l)*shift(1*l)*a_box);

draw(a_arrow,arrow=Arrow(size=ah));
draw(shift(3.5*l)*a_arrow,arrow=Arrow(size=ah));
draw(shift(3.5*l)*shift(3.5*l)*a_arrow,arrow=Arrow(size=ah));

draw(b_arrow,arrow=Arrow(size=ah));
draw(shift(3.5*l)*b_arrow,arrow=Arrow(size=ah));
draw(shift(3.5*l)*shift(3.5*l)*b_arrow,arrow=Arrow(size=ah));

label("$\mathit{saved\mbox{-}env}$",shift(3.5*l)*shift(3.5*l)*(l*3.5,w/2),align=E);
label("{\tt{z}}{\emph{的值}}",(l/2,-w*1),align=S);
label("{\tt{y}}{\emph{的值}}",shift(3.5*l)*(l/2,-w*1),align=S);
label("{\tt{x}}{\emph{的值}}",shift(3.5*l)*shift(3.5*l)*(l/2,-w*1),align=S);

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
