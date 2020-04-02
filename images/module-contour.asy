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

Label[] lines={"\Large{\tt{module m1}}",
               "\Large{\tt{\phantom{x}interface}}",
               "\Large{\tt{\phantom{xx}[ a :\phantom{x}int}}",
               "\Large{\tt{\phantom{xxxx}b :\phantom{x}int}}",
               "\Large{\tt{\phantom{xxxx}c :\phantom{x}int ]}}",
               "\Large{\tt{\phantom{x}body}}",
               "\Large{\tt{\phantom{xx}[ a = 33}}",
               "\Large{\tt{\phantom{xxxx}x = -(a,1)}}",
               "\Large{\tt{\phantom{xxxx}b = -(a,x)}}",
               "\Large{\tt{\phantom{xxxx}c = -(x,b) ]}}",
               "\Large{\tt{\phantom{x}}}",
               "\Large{\tt{let a = 10}}",
               "\Large{\tt{in -(from m1 take a,}}",
               "\Large{\tt{\phantom{xxxxx}a)}}",
               };

Label[] notes={"{\Large{{\texttt{a = 33}}}}\emph{的作用范围}",
               "\Large{\texttt{from m1 take a}}",
               "\Large{\texttt{from m1 take b}}",
               "\Large{\texttt{from m1 take c}}",
               "\emph{的作用范围}",
};

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
code_word_width=get_x("\Large{\tt{x}}");
code_line_height=get_y("\Large{\tt{(procedure)}}");

// get x positions of round boxes
box0_min_x=get_x("\Large{\tt{\phantom{xxx}}}");
box0_max_x=get_x("\Large{\tt{\phantom{xxx}c = -(x,b)}}");

box1_min_x=0;
box1_max_x=get_x("\Large{\tt{in -(from m1 take a,}}");

// get y positions of round boxes
box0_min_y=-6*code_line_height;
box0_max_y=-9*code_line_height;

box1_min_y=-10*code_line_height;
box1_max_y=-13*code_line_height;

// draw codes
for (Label l : lines) {
  frame f=pack(align=S, l);
  add(f, pos, Align);
  pos=(pos.x, pos.y-code_line_height);
}

// draw contour
path box0=shift(-code_word_width / 8)*roundedpath(box((box0_min_x - code_word_width / 20, box0_min_y), (box0_max_x + code_word_width, box0_max_y)), 2.5);
draw(box0);

path box1=shift(-code_word_width / 8, code_line_height / 20)*roundedpath(box((box1_min_x, box1_min_y), (box1_max_x, box1_max_y)), 2.5);
draw(box1);

// draw notes, and arrows
real note0_width=get_x(notes[0]);
real note0_height=get_y(notes[0]);
pair pos=(-2*code_word_width, -8*code_line_height);
frame f=pack(align=S, notes[0]);
add(f, shift(-note0_width)*pos, Align);

pair note0_arrow=shift(0, note0_height / 2)*pos;
pair end0=(min(box0).x, (min(box0).y+max(box0).y)/2);
draw(note0_arrow{(2,1)}..{(1,-1)}end0, arrow=Arrow(size=ah));

real note1_width=get_x(notes[1]);
real note1_height=get_y(notes[1]);

pair pos1=(-2*code_word_width, -10*code_line_height);
frame f=pack(align=S, notes[1]);
add(f, shift(-note1_width)*pos1, Align);

pair pos2=(-2*code_word_width, -11*code_line_height);
frame f=pack(align=S, notes[2]);
add(f, shift(-note1_width)*pos2, Align);

pair pos3=(-2*code_word_width, -12*code_line_height);
frame f=pack(align=S, notes[3]);
add(f, shift(-note1_width)*pos3, Align);

pair pos4=(-code_word_width, -13*code_line_height);
frame f=pack(align=S, notes[4]);
add(f, shift(-note1_width)*pos4, Align);

pair note1_arrow=shift(0, note1_height / 2)*pos2;
draw(note1_arrow{(2,1)}..{(1,-1)}point(box1, 7), arrow=Arrow(size=ah));

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
