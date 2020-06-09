import roundedpath;
texpreamble("\usepackage{xeCJK}
\setCJKmainfont[BoldFont={WenQuanYi Micro Hei}, ItalicFont={AR PL UKai CN}]{Adobe Song Std}
\setCJKsansfont{Adobe Song Std}
\setCJKmonofont{Adobe Song Std}
\xeCJKsetup{CJKmath=true, PlainEquation=true}
\usepackage[T1]{fontenc}
\usepackage{listings}
");
real fsz=12pt;
real dsz=8pt;
real ah=4;

defaultpen(fontsize(fsz));
unitsize(dsz);

pair pos=(0,0);

Label[] lines={"{\tt{\phantom{}(lambda (x y)\phantom{xxxxxxxxxxxxxxxxxxxxxxxxxxxx}; \small{x1, y1}}}",
               "{\tt{\phantom{xx}(let ((z (+ x y)))\phantom{xxxxxxxxxxxxxxxxxxxxx}; \small{z1}}}",
               "{\tt{\phantom{xxxx}(lambda (x z)\phantom{xxxxxxxxxxxxxxxxxxxxxxxx}; \small{x2, z2}}}",
               "{\tt{\phantom{xxxxxx}(let ((x (let ((x\phantom{xxxxxxxxxxxxxxxxxx}; \small{x3, x4}}}",
               "{\tt{\phantom{xxxxxxxxxxxxxxxxxxxxxxx}(+ x y z))\phantom{xxxxxxxx}; }}\small{\emph{第5行}}",
               "{\tt{\phantom{xxxxxxxxxxxxxxxxxxxxx}(y 11))\phantom{xxxxxxxxxxxxx}; \small{y2}}}",
               "{\tt{\phantom{xxxxxxxxxxxxxxxxx}(+ x y z)\phantom{x})))\phantom{xxxxxxxxxxx}; }}\small{\emph{第7行}}",
               "{\tt{\phantom{xxxxxxxx}(+ x y z)\phantom{x})\phantom{xxxxxxxxxxxxxxx} ) ) )\phantom{x}; }}\small{\emph{第8行}}"};

Label[] notes={"\footnotesize{{\texttt{x1, y1}}\emph{的作用域}}",
               "\footnotesize{{\texttt{z}}\emph{的作用域}}",
               "\footnotesize{{\texttt{x2, z2}}\emph{的作用域}}",
               "\footnotesize{{\texttt{x4, y2}}\emph{的作用域}}",
               "\footnotesize{{\texttt{x3}}\emph{的作用域}}"};

real code_word_width=0;
real code_line_height=0;

real box0_min_x=0;
real box0_min_y=0;
real box0_max_x=0;
real box0_max_y=0;

real box1_min_x=0;
real box1_min_y=0;
real box1_max_x=0;
real box1_max_y=0;

real box2_min_x=0;
real box2_min_y=0;
real box2_max_x=0;
real box2_max_y=0;

real box3_min_x=0;
real box3_min_y=0;
real box3_max_x=0;
real box3_max_y=0;

real box4_min_x=0;
real box4_min_y=0;
real box4_max_x=0;
real box4_max_y=0;

real get_x(Label l) {
  picture pic;
  unitsize(pic, dsz);
  frame f=pack(align=S, l);
  path p=box(f, p=invisible);
  add(pic, f, (0, 0), Align);
  pair p_max=max(pic, user=true);
  real x = p_max.x;
  return x;
}

real get_y(Label l) {
  picture pic;
  unitsize(pic, dsz);
  frame f=pack(align=S, l);
  path p=box(f, p=invisible);
  add(pic, f, (0, 0), Align);
  pair p_max=max(pic, user=true);
  real y = p_max.y;
  return y;
}

// get width and height of a line
code_word_width=get_x("{\tt{x}}");
code_line_height=get_y("{\tt{(procedure)}}\small{\emph{第7行}}");

// get x positions of round boxes
box0_min_x=get_x("{\tt{\phantom{xx}}}");
box0_max_x=get_x("{\tt{\phantom{xxxxxxxx}(+ x y z)\phantom{x})\phantom{xxxxxxxxxxxxxxx} ) )}}");

box1_min_x=get_x("{\tt{\phantom{xxxx}}}");
box1_max_x=get_x("{\tt{\phantom{xxxxxxxx}(+ x y z)\phantom{x})\phantom{xxxxxxxxxxxxxxx} )}}");

box2_min_x=get_x("{\tt{\phantom{xxxxxx}}}");
box2_max_x=get_x("{\tt{\phantom{xxxxxxxx}(+ x y z)\phantom{x})\phantom{xxxxxxxxxxxxxxx}}}");

box3_min_x=get_x("{\tt{\phantom{xxxxxxxxxxxxxxxxx}}}");
box3_max_x=get_x("{\tt{\phantom{xxxxxxxxxxxxxxxxx}(+ x y z)}}");

box4_min_x=get_x("{\tt{\phantom{xxxxxxxx}}}");
box4_max_x=get_x("{\tt{\phantom{xxxxxxxx}(+ x y z)}}");

// get y positions of round boxes
box0_min_y=0*code_line_height;
box0_max_y=-7*code_line_height;

box1_min_y=-1*code_line_height;
box1_max_y=-7*code_line_height;

box2_min_y=-2*code_line_height;
box2_max_y=-7*code_line_height;

box3_min_y=-5*code_line_height;
box3_max_y=-6*code_line_height;

box4_min_y=-6*code_line_height;
box4_max_y=-7*code_line_height;

// draw codes
for (Label l : lines) {
  frame f=pack(align=S, l);
  add(f, pos, Align);
  pos=(pos.x, pos.y-code_line_height);
}

// draw contour
path box0=roundedpath(box((box0_min_x - code_word_width / 2, box0_min_y),
                          (box0_max_x, box0_max_y - code_line_height)),
                      0.5);
draw(box0);

path box1=roundedpath(box((box1_min_x - code_word_width / 2, box1_min_y),
                          (box1_max_x, box1_max_y - code_line_height / 3 * 2)),
                      0.5);
draw(box1);

path box2=roundedpath(box((box2_min_x - code_word_width / 2, box2_min_y),
                          (box2_max_x, box2_max_y - code_line_height / 3)),
                      0.5);
draw(box2);

path box3=roundedpath(box((box3_min_x - code_word_width / 3, box3_min_y - code_line_height / 20),
                          (box3_max_x, box3_max_y)),
                      0.5);
draw(box3);

path box4=roundedpath(box((box4_min_x - code_word_width / 3, box4_min_y - code_line_height / 20),
                          (box4_max_x, box4_max_y)),
                      0.5);
draw(box4);

pair[] arrow_tails = { point(box0, 7),
                       point(box1, 7),
                       point(box2, 7),
                       point(box3, 7),
                       point(box4, 7) };

// draw notes, and arrows
real note_line_height=6.5*code_line_height/notes.length;
for (int i = 0; i < notes.length; ++i) {
  real note_width=get_x(notes[i]);
  real note_height=get_y(notes[i]);
  pair pos=(-2*code_word_width, -(i+1)*note_line_height);
  frame f=pack(align=S, notes[i]);
  add(f, shift(-note_width)*pos, Align);
  pair note_arrow=shift(0, note_height / 2)*pos;
  if (arrow_tails[i].y > note_arrow.y) {
    draw(note_arrow{NE}..{E}arrow_tails[i], arrow=Arrow(size=ah));
  } else {
    draw(note_arrow{(3,1)}..{(3,-1)}arrow_tails[i], arrow=Arrow(size=ah));
  }
}

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
