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
    block block1=rectangle("\tt{x}", shift(l/2+w*0.75)*pos, minheight=w, minwidth=w*1.5);
    real l1=block1.right(t).x - block1.left(t).x;

    block _block2=rectangle("\tt{<<if zero?(x) then 0 else -((double -(x,1)), -2)>>}",
                            shift(l/2+w*0.75)*pos, minheight=w);
    real l2=_block2.right(t).x - _block2.left(t).x;
    block block2=rectangle("\tt{<<if zero?(x) then 0 else -((double -(x,1)), -2)>>}",
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
                           shift(-l/2-l5/2-w*4.2,w*3)*pos, minheight=w);

    block block6=rectangle("", shift(-l/2-w*3.7,w*3)*pos, minheight=w, minwidth=w);
    block block7=rectangle("", shift(-l/2-w*2.7,w*3)*pos, minheight=w, minwidth=w);
    block block8=rectangle("", shift(-l/2-w*1.7,w*3)*pos, minheight=w, minwidth=w);

    // bound as vector, second row
    real b_length=(block3.right().x+2-block4.left().x);
    real b_x=(block3.right().x+2+block4.left().x)/2;
    pair bound_center=(b_x, 0);
    block block9=rectangle("", bound_center, minheight=w+4, minwidth=b_length);
    block block10=rectangle("", bound_center, minheight=w+8, minwidth=b_length+4);

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

    // arrows
    pair arr1=shift(-w, w)*block5.topleft();
    draw(pic, arr1--block5.topleft(), arrow=Arrow(size=ah));

    pair arr2=shift(-w/6)*(block6.bottomleft().x, block10.topleft().y);
    draw(pic, block6.center--arr2, arrow=Arrow(size=ah));
    label(pic, "\tt{x}", arr2, S);

    draw(pic, block7.center--block10.topleft(), arrow=Arrow(size=ah));

    pair arr3=shift(w*2)*block8.center;
    draw(pic, block8.center--arr3, arrow=Arrow(size=ah));
    label(pic, "\tt{saved-env}", arr3, E);

    label(pic, "\emph{包含$expval$的向量}", block10.top(), N);

    pair arr4=shift(-6*w, -2.5*w)*block3.topright();
    pair arr5=(block6.right().x, arr4.y);
    draw(pic, block3.center{down}..{left}arr4{left}..{left}arr5{left}..{up}block5.bottom(), arrow=Arrow(size=ah));
  });

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
