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

Label[] lines={"{\tt{module m1}}",
               "{\tt{\phantom{x}interface}}",
               "{\tt{\phantom{xx}[opaque t}}",
               "{\tt{\phantom{xxx}z : t}}",
               "{\tt{\phantom{xxx}s : (t -> t)}}",
               "{\tt{\phantom{xxx}is-z? : (t -> bool) ]}}",
               "{\tt{\phantom{x}body}}",
               "{\tt{\phantom{xx}[type t = int}}",
               "{\tt{\phantom{xxx}z = 33}}",
               "{\tt{\phantom{xxx}s = proc (x : t) -(x, -1)}}",
               "{\tt{\phantom{xxx}is-z? = proc (x : t) zero?(-(x,z)) ]}}",
               "{\tt{\phantom{x}}}",
               "{\tt{let f = proc (x : from m1 take t) ...}}",
               "{\tt{in ...}}",
               };

Label[] notes={"{\scriptsize{\texttt{opaque t}}\emph{的作用范围}}",
               "{\scriptsize{\texttt{type t = int}}\emph{的作用范围}}",
               "{\scriptsize{\texttt{from m1 take t}}\emph{的作用范围}}",
               "{\scriptsize\emph{（模糊类型）}}",
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

real box2_min_x=0;
real box2_max_x=0;
real box2_min_y=0;
real box2_max_y=0;

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
code_line_height=get_y("{\tt{(procedure)}}");

// get x positions of round boxes
box0_min_x=get_x("{\tt{\phantom{xx}}}");
box0_max_x=get_x("{\tt{\phantom{xx}is-z? : (t -> bool) }}");

box1_min_x=get_x("{\tt{\phantom{xx}}}");
box1_max_x=get_x("{\tt{\phantom{xx}is-z? = proc (x : t) zero?(-(x,z)) }}");

box2_min_x=get_x("{\tt{\phantom{}}}");
box2_max_x=get_x("{\tt{let f = proc (x : from m1 take t) ...\phantom{xxxx}}}");

// get y positions of round boxes
box0_min_y=-2*code_line_height;
box0_max_y=-5*code_line_height;

box1_min_y=-7*code_line_height;
box1_max_y=-10*code_line_height;

box2_min_y=-10.5*code_line_height;
box2_max_y=-16*code_line_height;

// draw codes
for (Label l : lines) {
  frame f=pack(align=S, l);
  add(f, pos, Align);
  pos=(pos.x, pos.y-code_line_height);
}

// draw contour
path box0=roundedpath(box((box0_min_x, box0_min_y), (box0_max_x, box0_max_y)), 1.2);
draw(box0);

path box1=roundedpath(box((box1_min_x, box1_min_y), (box1_max_x, box1_max_y)), 1.2);
draw(box1);

path box2=roundedpath(box((box2_min_x-code_word_width / 2, box2_min_y), (box2_max_x, box2_max_y)), 1.2);
draw(box2);

// draw notes, and arrows
real note0_width=get_x(notes[0]);
real note0_height=get_y(notes[0]);
pair pos=(-2*code_word_width, -4*code_line_height);
frame f=pack(align=S, notes[0]);
add(f, shift(-note0_width)*pos, Align);

pair note0_arrow=shift(0, note0_height / 2)*pos;
pair end0=(min(box0).x, (min(box0).y+max(box0).y)/2);
draw(note0_arrow{(2,1)}..{(1,-1)}end0, arrow=Arrow(size=ah));

real note1_width=get_x(notes[1]);
real note1_height=get_y(notes[1]);
pair pos1=(-2*code_word_width, -8.5*code_line_height);
frame f=pack(align=S, notes[1]);
add(f, shift(-note1_width)*pos1, Align);

pair note1_arrow=shift(0, note0_height / 2)*pos1;
pair end1=(min(box1).x, (min(box1).y+max(box1).y)/2);
draw(note1_arrow{(2,1)}..{(1,-1)}end1, arrow=Arrow(size=ah));

real note2_width=get_x(notes[2]);
real note2_height=get_y(notes[2]);
pair pos2=(-2*code_word_width, -13.8*code_line_height);
frame f=pack(align=S, notes[2], notes[3]);
add(f, shift(-note2_width)*pos2, Align);

pair note2_arrow=shift(0, note2_height)*pos2;
pair end2=(min(box2).x, (min(box2).y+max(box2).y)/2);
draw(note2_arrow{(2,1)}..{(1,-1)}end2, arrow=Arrow(size=ah));

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
