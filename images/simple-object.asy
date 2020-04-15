import flowchart;
texpreamble("\usepackage{xeCJK}
\setCJKmainfont[BoldFont={WenQuanYi Micro Hei}, ItalicFont={AR PL UKai CN}]{Adobe Song Std}
\setCJKsansfont{Adobe Song Std}
\setCJKmonofont{Adobe Song Std}
\xeCJKsetup{CJKmath=true, PlainEquation=true}
\usepackage[T1]{fontenc}
");
defaultpen(fontsize(10pt));
unitsize(16pt);
pair pos=(0,0);
block _block0=rectangle("\tt{an-object}", pos);
real _l=_block0.right().x - _block0.left().x;
block block0=rectangle("\tt{an-object}", pos, minwidth=_l*1.2);
real w=block0.top().y - block0.bottom().y;
real l=block0.right().x - block0.left().x;
real ah=6;

draw(block0);

add(new void(picture pic, transform t) {
    // first row
    block _block5=rectangle("\tt{\phantom{p}c3\phantom{d}}");
    real l5=_block5.right(t).x - _block5.left(t).x;
    block block5=rectangle("\tt{\phantom{p}c3\phantom{d}}",
                           shift(l/2+l5*1.2/2)*pos, minwidth=l5*1.2, minheight=w);

    real bw=w*1.2;              // block width
    pair next_pos=shift(l/2+l5*1.2+bw/2)*pos;
    block block6=rectangle("", next_pos, minwidth=bw, minheight=w);

    next_pos=shift(bw*2.2)*next_pos;
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

    next_pos=shift(bw*1.6)*next_pos;
    block block13=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block14=rectangle("", next_pos, minwidth=bw, minheight=w);

    next_pos=shift(bw*1.6)*next_pos;
    block block15=rectangle("", next_pos, minwidth=bw, minheight=w);
    next_pos=shift(bw)*next_pos;
    block block16=rectangle("", next_pos, minwidth=bw, minheight=w);

    // second row
    next_pos=shift(0,-2.5*w)*block7.center;
    block block1=rectangle("$\mathit{11}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    next_pos=shift(0,-2.5*w)*block9.center;
    block block2=rectangle("$\mathit{12}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    next_pos=shift(0,-2.5*w)*block11.center;
    block block3=rectangle("$\mathit{22}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    next_pos=shift(0,-2.5*w)*block13.center;
    block block4=rectangle("$\mathit{31}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    next_pos=shift(0,-2.5*w)*block15.center;
    block block17=rectangle("$\mathit{32}$", next_pos, minwidth=bw, minheight=w, drawpen=scale(2)*currentpen);

    // third row
    next_pos=shift(0,-2*w)*block1.center;
    block block18=rectangle("\tt{\phantom{d}x1\phantom{p}}", next_pos, minwidth=bw, minheight=w, drawpen=invisible);

    next_pos=shift(0,-2*w)*block2.center;
    block block19=rectangle("\tt{\phantom{d}y1\phantom{p}}", next_pos, minwidth=bw, minheight=w, drawpen=invisible);

    next_pos=shift(0,-2*w)*block3.center;
    block block20=rectangle("\tt{\phantom{d}x2\phantom{p}}", next_pos, minwidth=bw, minheight=w, drawpen=invisible);

    next_pos=shift(0,-2*w)*block4.center;
    block block21=rectangle("\tt{\phantom{d}y2\phantom{p}}", next_pos, minwidth=bw, minheight=w, drawpen=invisible);

    next_pos=shift(0,-2*w)*block17.center;
    block block22=rectangle("\tt{\phantom{d}z\phantom{p}}", next_pos, minwidth=bw, minheight=w, drawpen=invisible);

    draw(pic, block5);
    draw(pic, block6);
    dot(pic, block6.center);
    draw(pic, block7);
    dot(pic, block7.center);
    draw(pic, block8);
    dot(pic, block8.center);
    draw(pic, block9);
    dot(pic, block9.center);
    draw(pic, block10);
    dot(pic, block10.center);
    draw(pic, block11);
    dot(pic, block11.center);
    draw(pic, block12);
    dot(pic, block12.center);
    draw(pic, block13);
    dot(pic, block13.center);
    draw(pic, block14);
    dot(pic, block14.center);
    draw(pic, block15);
    dot(pic, block15.center);
    draw(pic, block16);

    draw(pic, block1);
    draw(pic, block2);
    draw(pic, block3);
    draw(pic, block4);
    draw(pic, block17);

    draw(pic, block18);
    draw(pic, block19);
    draw(pic, block20);
    draw(pic, block21);
    draw(pic, block22);

    // arrows
    pair pos1=shift(bw*1.2)*block6.center;
    arrowbar a=Arrow(size=ah/2, angle=30);
    draw(pic, block6.center--pos1, arrow=a);
    draw(pic, block8.center--block9.left(), arrow=a);
    draw(pic, block10.center--block11.left(), arrow=a);
    draw(pic, block12.center--block13.left(), arrow=a);
    draw(pic, block14.center--block15.left(), arrow=a);

    draw(pic, block7.center--block1.top(), arrow=a);
    draw(pic, block9.center--block2.top(), arrow=a);
    draw(pic, block11.center--block3.top(), arrow=a);
    draw(pic, block13.center--block4.top(), arrow=a);
    draw(pic, block15.center--block17.top(), arrow=a);

    // lables
    block block_l1=rectangle(pack("\small{\emph{存储器中}}","\small{\emph{的位置}}"), shift(-l*1.2)*block1.bottomleft(), drawpen=invisible);
    draw(pic, block_l1);
    draw(pic, block_l1.right(){(1,1)}..{right}shift(-w*0.8,-w/10)*block1.left(), arrow=Arrow(arrowhead=HookHead, size=ah/3, angle=30));

    block block_l2=rectangle("\small{\emph{对应的字段}}", shift(0,-w*1.7)*block_l1.center, drawpen=invisible);
    draw(pic, block_l2);
    draw(pic, block_l2.right(){(3,1)}..{right}shift(-w)*block18.center, arrow=Arrow(arrowhead=HookHead, size=ah/3, angle=30));
  });

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
