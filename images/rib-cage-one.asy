defaultpen(fontsize(8pt));
unitsize(12pt);
real w = 1;
real l = 1;
real offset = 2*w+l/2;
path a_box = box((0, 0), (l, w));
path a_arrow = (l/2,w/2)--(l/2,-w*1);
path b_arrow = (l*1.5,w/2)--(l*3.5,w/2);

draw(shift(0)*a_box);
draw(shift(1*l)*a_box);
draw(shift(0, -2*w)*a_box);
draw(shift(1*l, -2*w)*a_box);

draw(a_arrow,arrow=Arrow());
draw(b_arrow,arrow=Arrow());
draw((l*0.5,-w*1.5)--(-l,-w*3), arrow=Arrow());
draw((l*1.5,-w*1.5)--(l*3,-w*3), arrow=Arrow());

label("$\mathit{saved\mbox{-}env}$",(l*3.5,w/2),align=E);
label("$\mathit{saved\mbox{-}vars}$",(-l,-w*3.5));
label("$\mathit{saved\mbox{-}vals}$",(l*3,-w*3.5));
shipout(currentpicture.fit());
