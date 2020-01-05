defaultpen(fontsize(10pt));
unitsize(1cm);
real w = 1;
real l = 1.6;
real offset = 2*w+l/2;
path a_box = box((0, 0), (l, w));
path a_arrow = (l,w/2)--(l+2-l,w/2);
path a_label = (l/2, w/2);

draw(shift(0)*a_box);
draw(shift(2)*a_box);
draw(shift(4)*a_box);
draw(shift(6)*a_box);
draw(shift(8.2,1.2*w)*a_box);
draw(shift(10.2,1.2*w)*a_box);
draw(shift(8.2,-w*1.8)*a_box);
draw(shift(10.2,-w*1.2)*a_box);
draw(shift(10.2,-w*2.4)*a_box);

draw(shift(0)*a_arrow, arrow=Arrow());
draw(shift(2)*a_arrow, arrow=Arrow());
draw(shift(4)*a_arrow, arrow=Arrow());
draw((6+l, w/2) -- (8.2, 1.7*w), arrow=Arrow());
draw((8.2+l, 1.7*w) -- (10.2, 1.7*w), arrow=Arrow());
draw((6+l, w/2) -- (8.2, -1.3*w), arrow=Arrow());
draw((8.2+l, -1.3*w) -- (10.2, -0.7*w), arrow=Arrow());
draw((8.2+l, -1.3*w) -- (10.2, -1.9*w), arrow=Arrow());

label("1", shift(0)*a_label);
label("2", shift(2)*a_label);
label("3", shift(4)*a_label);
label("4", shift(6)*a_label);
label("5", shift(8.2, w*1.2)*a_label);
label("6", shift(2)*(shift(8.2, w*1.2)*a_label));
label("7", shift(8.2,-1.8*w)*a_label);
label("8", shift(2,0.6*w)*(shift(8.2,-1.8*w)*a_label));
label("9", shift(2,-0.6*w)*(shift(8.2,-1.8*w)*a_label));
shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
