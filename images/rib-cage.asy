settings.tex="xelatex";
texpreamble("\usepackage{xeCJK}
\setCJKmainfont[BoldFont={WenQuanYi Micro Hei}, ItalicFont={AR PL UKai CN}]{Adobe Song Std}
\setCJKsansfont{Adobe Song Std}
\setCJKmonofont{Adobe Song Std}
\xeCJKsetup{CJKmath=true, PlainEquation=true}
\usepackage[T1]{fontenc}
");
defaultpen(fontsize(8pt));
unitsize(12pt);
real w = 1;
real l = 1;
real offset = 8;
path a_box = box((0, 0), (l, w));
path a_arrow = (l/2,w/2)--(l/2,-w*1);
path b_arrow = (l*1.5,w/2)--(l*offset,w/2);
path c_arrow = (0,w/2)--(l*(offset-1.5),w/2);

// first part
draw(shift(0)*a_box);
draw(shift(1)*a_box);
draw(shift(0,-2*w)*a_box);
draw(shift(1,-2*w)*a_box);

draw(a_arrow,arrow=Arrow());
draw(b_arrow,arrow=Arrow());
draw((l*(1.5+offset)/2,w/2+3.5){(0.5,-1.5)}..{down}(l*(1.5+offset)/2+0.5,w/2+2){down}..{(-0.5,-1.5)}(l*(1.5+offset)/2,w/2+0.5), arrow=Arrow());
draw(((l*0.5,-w*1.5)--(-l,-w*3)), arrow=Arrow());
draw((l*1.5,-w*1.5)--(l*3,-w*3), arrow=Arrow());

label("\emph{脊柱}",(l*(1.5+offset)/2,w/2+3.5),align=N);
label("\texttt{(a b c)}",(-l,-w*3.5));
label("\texttt{(11 12 13)}",(l*3,-w*3.5));

// second part
draw(shift(offset)*a_box);
draw(shift(offset+1)*a_box);
draw(shift(offset,-2*w)*a_box);
draw(shift(offset+1,-2*w)*a_box);

draw(shift(offset)*a_arrow,arrow=Arrow());
draw(shift(offset)*b_arrow,arrow=Arrow());
draw(shift(offset)*((l*0.5,-w*1.5)--(-l,-w*3)), arrow=Arrow());
draw(shift(offset)*((l*1.5,-w*1.5)--(l*3,-w*3)), arrow=Arrow());

label("\texttt{(x z)}",shift(offset)*(-l,-w*3.5));
label("\texttt{(66 77)}",shift(offset)*(l*3,-w*3.5));

// third part
draw(shift(offset*2)*a_box);
draw(shift(offset*2+1)*a_box);
draw(shift(offset*2,-2*w)*a_box);
draw(shift(offset*2+1,-2*w)*a_box);

draw(shift(offset*2)*a_arrow,arrow=Arrow());
draw(shift(offset*2+l*1.5)*(xscale(0.8)*c_arrow),arrow=Arrow());
draw(shift(offset*2)*((l*0.5,-w*1.5)--(-l,-w*3)), arrow=Arrow());
draw(shift(offset*2)*((l*1.5,-w*1.5)--(l*3,-w*3)), arrow=Arrow());

label("\textit{环境其余部分}",shift(offset*2+0.8*(offset-1.5))*(l*1.5,w/2),align=E);
label("\texttt{(x y)}",shift(offset*2)*(-l,-w*3.5));
label("\texttt{(88 99)}",shift(offset*2)*(l*3,-w*3.5));

shipout(currentpicture.fit());
