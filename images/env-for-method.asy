import flowchart;
texpreamble("\usepackage{xeCJK}
\setCJKmainfont[BoldFont={WenQuanYi Micro Hei}, ItalicFont={AR PL UKai CN}]{Adobe Song Std}
\setCJKsansfont{Adobe Song Std}
\setCJKmonofont{Adobe Song Std}
\xeCJKsetup{CJKmath=true, PlainEquation=true}
\usepackage[T1]{fontenc}
");
defaultpen(fontsize(7pt));
unitsize(9pt);
pair pos=(0,0);
block _block0=rectangle("\tt{\phantom{p}an-object\phantom{d}}", pos);
real _l=_block0.right().x - _block0.left().x;
real _w=_block0.top().y - _block0.bottom().y;
block block0=rectangle("\tt{\phantom{p}an-object\phantom{d}}", pos, minwidth=_l*0.8, minheight=_w*0.8);
real w=block0.top().y - block0.bottom().y;
real l=block0.right().x - block0.left().x;
real ah=6;

draw(block0);

add(new void(picture pic, transform t) {
    // first row
    block _block1=rectangle("\tt{\phantom{p}c3\phantom{d}}");
    real l1=_block1.right(t).x - _block1.left(t).x;
    block block1=rectangle("\tt{\phantom{p}c3\phantom{d}}",
                           shift(l/2+l1/2)*pos, minwidth=l1, minheight=w);

    real bw=w*1.2;              // block width
    pair next_pos=shift(l/2+l1+bw/2)*pos;
    block block2=rectangle("", next_pos, minwidth=bw, minheight=w);

    // row 5
    next_pos=shift(bw*4.8, -w*5)*next_pos;
    block block3=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block4=rectangle("", next_pos, minwidth=bw, minheight=w);

    next_pos=shift(bw*1.6)*next_pos;
    block block5=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block6=rectangle("", next_pos, minwidth=bw, minheight=w);

    next_pos=shift(bw*1.6)*next_pos;
    block block7=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block8=rectangle("", next_pos, minwidth=bw, minheight=w);

    next_pos=shift(bw*1.6)*next_pos;
    block block9=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block10=rectangle("", next_pos, minwidth=bw, minheight=w);

    next_pos=shift(bw*1.6)*next_pos;
    block block11=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block12=rectangle("", next_pos, minwidth=bw, minheight=w);

    // row 6
    next_pos=shift(0,-2.5*w)*block3.center;
    block block13=rectangle("$\mathit{11}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    next_pos=shift(0,-2.5*w)*block5.center;
    block block14=rectangle("$\mathit{12}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    next_pos=shift(0,-2.5*w)*block7.center;
    block block15=rectangle("$\mathit{22}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    next_pos=shift(0,-2.5*w)*block9.center;
    block block16=rectangle("$\mathit{31}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    next_pos=shift(0,-2.5*w)*block11.center;
    block block17=rectangle("$\mathit{32}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    // row 4
    next_pos=shift(-bw,w*4.5)*block3.center;
    block block18=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block19=rectangle("", next_pos, minwidth=bw, minheight=w);

    // row 3, middle
    next_pos=shift(-bw*1.5-l1/2,w*3.7)*block1.center;
    block block20=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block21=rectangle("", next_pos, minwidth=bw, minheight=w);

    // row 1, middle
    next_pos=shift(0,w*6)*block20.center;
    block block22=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block23=rectangle("", next_pos, minwidth=bw, minheight=w);

    // row 1, right
    next_pos=(block18.center.x, block23.center.y);
    block block24=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block25=rectangle("", next_pos, minwidth=bw, minheight=w);

    // row 1, left
    next_pos=shift(-bw-0.68*(block24.center.x-block23.center.x))*block22.center;
    block block26=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block27=rectangle("", next_pos, minwidth=bw, minheight=w);

    // row 2, left
    next_pos=shift(0,-3.3*w)*block26.center;
    block block28=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block29=rectangle("", next_pos, minwidth=bw, minheight=w);

    // row 3
    next_pos=shift(0,-0.5*w)*(block26.center.x, block20.center.y);
    block block30=rectangle("$\mathit{7}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);
    next_pos=shift(bw*1.6)*next_pos;
    block block31=rectangle("$\mathit{8}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    // label above row 3
    next_pos=shift(0,w)*block30.center;
    block block32=rectangle("\tt{(u v)}", next_pos, minwidth=bw, minheight=w, drawpen=invisible);

    // label in row 3
    next_pos=shift(-bw*0.8)*block30.center;
    block block33=rectangle("\tt{(}", next_pos, minwidth=bw, minheight=w, drawpen=invisible);
    next_pos=shift(bw)*block31.center;
    block block34=rectangle("\tt{)}", next_pos, minwidth=bw, minheight=w, drawpen=invisible);

    // label in row 4
    next_pos=shift(-bw*0.5)*block0.left().x;
    block block35=rectangle("\tt{(}", next_pos, minwidth=bw, minheight=w, drawpen=invisible);

    // label above row 4, left
    next_pos=shift(0,w)*block35.center;
    block block36=rectangle("\tt{(}", next_pos, minwidth=bw, minheight=w, drawpen=invisible);

    // label above row 4, right
    block _block37=rectangle("\tt{\phantom{(d}\%super)}");
    real l_lable=_block37.right().x-_block37.left().x;
    next_pos=shift(bw/2+l_lable/3,w)*block2.center;
    block block37=rectangle("\tt{\phantom{d}\%super)}", next_pos, minheight=w, drawpen=invisible);

    // label in row 4, middle
    next_pos=shift(0,-w)*block37.center;
    block block38=rectangle("\tt{\phantom{dsu}c1\phantom{xp})}", next_pos, minwidth=l_lable, minheight=w, drawpen=invisible);

    // label above row 4, middle
    next_pos=(block20.right().x, block37.center.y);
    block block39=rectangle("\tt{\phantom{d}\%self\phantom{p}}", next_pos, minheight=w, drawpen=invisible);

    // label above row 5, left
    next_pos=shift(-w/10,w*1.5)*block3.center;
    block block40=rectangle("\tt{\phantom{d}(x\phantom{p}}", next_pos, minheight=w, drawpen=invisible);

    // label above row 5, middle
    next_pos=shift(0,w*1.5)*block5.center;
    block block41=rectangle("\tt{\phantom{d}y\%1\phantom{p}}", next_pos, minheight=w, drawpen=invisible);

    // label above row 5, right
    next_pos=shift(0,w*1.5)*block7.center;
    block block42=rectangle("\tt{\phantom{d}y)\phantom{p}}", next_pos, minheight=w, drawpen=invisible);

    // row 4
    draw(pic, block1);
    draw(pic, block2);
    dot(pic, block2.center);

    // row 5
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

    dot(pic, block3.center);
    dot(pic, block4.center);
    dot(pic, block5.center);
    dot(pic, block6.center);
    dot(pic, block7.center);
    dot(pic, block8.center);
    dot(pic, block9.center);
    dot(pic, block10.center);
    dot(pic, block11.center);

    // row 6
    draw(pic, block13);
    draw(pic, block14);
    draw(pic, block15);
    draw(pic, block16);
    draw(pic, block17);

    // row 4, right
    draw(pic, block18);
    draw(pic, block19);

    // row 3, middle
    draw(pic, block20);
    draw(pic, block21);

    // row 1, middle, right, left
    draw(pic, block22);
    draw(pic, block23);
    draw(pic, block24);
    draw(pic, block25);
    draw(pic, block26);
    draw(pic, block27);

    // row 2
    draw(pic, block28);
    draw(pic, block29);

    // row 3
    draw(pic, block30);
    draw(pic, block31);

    // label above row 3
    draw(pic, block32);

    // label in row 3
    draw(pic, block33);
    draw(pic, block34);

    // label in row 4, left
    draw(pic, block35);

    // label above row 4, left
    draw(pic, block36);
    // label above row 4, right
    draw(pic, block37);
    // label in row 4, right
    draw(pic, block38);
    // label above row 4, middle
    draw(pic, block39);

    // label above row 5, left, middle, right
    draw(pic, block40);
    draw(pic, block41);
    draw(pic, block42);

    dot(pic, block18.center);
    dot(pic, block19.center);

    dot(pic, block20.center);
    dot(pic, block21.center);

    dot(pic, block22.center);
    dot(pic, block23.center);

    dot(pic, block24.center);

    dot(pic, block26.center);
    dot(pic, block27.center);

    dot(pic, block28.center);
    dot(pic, block29.center);

    // arrows
    pair pos1=shift(bw*1.2)*block6.center;
    arrowbar a=Arrow(size=ah/2, angle=30);

    // arrow from row 1 to row 1
    draw(pic, block23.center--block24.left(), arrow=a);
    draw(pic, block27.center--block22.left(), arrow=a);

    // arrow from row 1 to row 2/3
    draw(pic, block26.center--block28.top(), arrow=a);
    draw(pic, block22.center--block20.top(), arrow=a);
    draw(pic, block24.center--block18.top(), arrow=a);

    // arrow from row 2
    draw(pic, block28.center--block32.top(), arrow=a);
    draw(pic, block29.center--shift(0,w/5)*(block29.center.x, block32.bottomright().y), arrow=a);
    // TODO: better curve
    draw(pic, block20.center{down}..{right}block35.left(), arrow=a);
    draw(pic, block21.center{down}..{right}block36.left(), arrow=a);

    // arrow from row 4
    // TODO: better curve
    draw(pic, block2.center{down}..{right}block3.left(), arrow=a);
    draw(pic, block18.center{down}..{right}block40.left(), arrow=a);
    draw(pic, block19.center{down}..{right}block3.left(), arrow=a);

    // arrow from row 5
    draw(pic, block4.center--block5.left(), arrow=a);
    draw(pic, block6.center--block7.left(), arrow=a);
    draw(pic, block8.center--block9.left(), arrow=a);
    draw(pic, block10.center--block11.left(), arrow=a);

    // arrow from row 5 to row 6
    draw(pic, block3.center--block13.top(), arrow=a);
    draw(pic, block5.center--block14.top(), arrow=a);
    draw(pic, block7.center--block15.top(), arrow=a);
    draw(pic, block9.center--block16.top(), arrow=a);
    draw(pic, block11.center--block17.top(), arrow=a);

  });

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
