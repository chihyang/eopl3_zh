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
pair pos=(0,0);
block _block0=rectangle("\tt{procedure}", pos);
real _l=_block0.right().x - _block0.left().x;
block block0=rectangle("\tt{procedure}", pos, minwidth=_l*1.2);
real w=block0.top().y - block0.bottom().y;
real l=block0.right().x - block0.left().x;
real ah=4;

draw(block0);

add(new void(picture pic, transform t) {
    // second row
    block _block1=rectangle("\tt{dummy}", pos, minheight=w);
    real l1=_block1.right(t).x - _block1.left(t).x;
    block block1=rectangle("\tt{dummy}", shift(l/2+l1/2)*pos, minheight=w, minwidth=w*1.5);

    block _block2=rectangle("\tt{<<begin ...>>}",
                            shift(l/2+w*0.75)*pos, minheight=w);
    real l2=_block2.right(t).x - _block2.left(t).x;
    block block2=rectangle("\tt{<<begin ...>>}",
                           shift(l/2+l1+l2/2)*pos, minheight=w);

    block block3=rectangle(body=Label("\tt{x}", p=invisible),
                           shift(l2/2+w*0.75)*shift(l/2+l1+l2/2)*pos, minheight=w, minwidth=w*1.5);
    real l3=block3.right(t).x - block3.left(t).x;

    block _block4=rectangle("\tt{proc-val}", pos);
    real l4=_block4.right(t).x - _block4.left(t).x;
    block block4=rectangle("\tt{proc-val}", drawpen=invisible,
                           shift(-l/2-l4/2)*pos, minheight=w);

    // first row
    block _block5=rectangle("\tt{extend-env}");
    real l5=_block5.right(t).x - _block5.left(t).x;
    block block5=rectangle("\tt{extend-env}",
                           shift(-l/2-l5/2-w,w*5)*pos, minheight=w);

    block block6=rectangle("", shift(-l/2-w*0.5,w*5)*pos, minheight=w, minwidth=w);
    block block7=rectangle("", shift(-l/2+w*0.5,w*5)*pos, minheight=w, minwidth=w);
    block block8=rectangle("", shift(-l/2+w*1.5,w*5)*pos, minheight=w, minwidth=w);

    // bound as vector, second row
    real b_length=(block3.right().x+2-block4.left().x);
    real b_x=(block3.right().x+2+block4.left().x)/2;
    pair bound_center=(b_x, 0);
    block block9=rectangle("", bound_center, minheight=w+4, minwidth=b_length);

    // third row
    block block10=rectangle("\tt{extend-env}",
                            shift(-w*1.2,-w*4)*pos, minheight=w);
    block block11=rectangle("", shift(-w*1.2+l5/2+w*0.5,-w*4)*pos, minheight=w, minwidth=w);
    block block12=rectangle("", shift(-w*1.2+l5/2+w*1.5,-w*4)*pos, minheight=w, minwidth=w);
    block block13=rectangle("", shift(-w*1.2+l5/2+w*2.5,-w*4)*pos, minheight=w, minwidth=w);

    // fourth row
    block block14=rectangle("", shift(w*0.75, -w*4.5)*block13.center, minheight=w*2.5, minwidth=w*2.5);
    block block15=rectangle("", block14.center, minheight=w*2.5+4, minwidth=w*2.5+4);

    draw(pic, block1);
    draw(pic, block2);
    draw(pic, block3);
    draw(pic, block4);
    draw(pic, block5);
    draw(pic, block6);
    draw(pic, block7);
    draw(pic, block8);
    draw(pic, block9);
    draw(pic, block10);
    draw(pic, block11);
    draw(pic, block12);
    draw(pic, block13);
    draw(pic, block14);
    draw(pic, block15);

    // arrows
    pair arr1=shift(-w, w)*block5.topleft();
    draw(pic, arr1--block5.topleft(), arrow=Arrow(size=ah));

    pair arr2=shift(-w/6)*(block6.bottomleft().x, block9.topleft().y + w * 2.5);
    draw(pic, block6.center--arr2, arrow=Arrow(size=ah));
    label(pic, "\tt{g}", arr2, S);

    pair arr3=shift(0,-w)*arr2;
    draw(pic, block7.center{down}..{(-4,-1)}arr3{(-4,-1)}..{down}block9.topleft(), arrow=Arrow(size=ah));

    pair arr4=shift(w*2)*block8.center;
    draw(pic, block8.center--arr4, arrow=Arrow(size=ah));
    label(pic, "\tt{saved-env}", arr4, E);

    pair arr5=shift(0, -w)*block0.bottom();
    pair arr6=shift(0, -w*1.5)*block2.bottom();
    draw(pic, block3.center{down}..{left}arr6{left}..{left}arr5{left}..{down}block10.topleft(), arrow=Arrow(size=ah));

    pair arr7=shift(w*-2.5)*block15.topleft();
    draw(pic, block11.center--arr7, arrow=Arrow(size=ah));
    label(pic, "\tt{counter}", arr7, S);

    draw(pic, block12.center--block15.topleft(), arrow=Arrow(size=ah));
    label(pic, "\emph{位置的引用}", point(block12.center--block15.topleft(), 0.5), E);

    pair arr8=shift(w*2)*block13.center;
    draw(pic, block13.center--arr8, arrow=Arrow(size=ah));
    label(pic, "\tt{saved-env}", arr8, E);

    label(pic, "\emph{计数器的位置}", block15.right(), E);
});

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
