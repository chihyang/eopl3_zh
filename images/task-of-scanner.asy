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
real ah=8;

defaultpen(fontsize(fsz));
unitsize(dsz);

pair pos=(0,0);

Label l1="\Huge{\tt{foo}\phantom{)p}}";
Label l2="\Huge{\tt{bar}\phantom{)p}}";
Label l3="\Huge{\tt{\%}\phantom{(dp)}}";
Label l4="\huge{\tt{此处为注释}\phantom{)dp}}";
Label l5="\Huge{\tt{)\phantom{pd}}}";
Label l6="\Huge{\tt{begin}}";
Label l7="\Huge{\tt{baz}\phantom{p}}";

Label l1_space="\Huge{\tt{foo}}";
Label l2_space="\Huge{\tt{bar}}";
Label l3_space="\Huge{\tt{\%}}";
Label l4_space="\huge{\emph{此处为注释}}";
Label l5_space="\Huge{\tt{)}}";
Label l6="\Huge{\tt{begin}}";
Label l7_space="\Huge{\tt{baz}}";

Label s1="\small{\textsl{ident}\phantom{pd)}}";
Label s2="\small{\textsl{ident}\phantom{pd)}}";
Label s3="\small{\textsl{\")\"\phantom{pd}}}";
Label s4="\small{\textsl{\"begin\"}\phantom{)}}";
Label s5="\small{\textsl{ident}\phantom{p)}}";

Label n1="\emph{忽略空格}";
Label n2="\emph{忽略注释}";
Label n3="\emph{区分标点}、\emph{关键字和标识符}";

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

Label a_space="\Huge{\tt{)}}";
real code_word_width=get_x("\Huge{\tt{x}}");
real space_width=get_x(a_space);
real space_height=get_x(a_space);

// first row and its comments
frame f=pack(align=E, l1);
add(f, pos, Align);

path box1=box(pos, (get_x(l1_space), get_y(l6)*1.1));
draw(box1);

f=pack(align=E, s1);
add(f, shift(0, get_y(l1))*pos, Align);

pos=shift(code_word_width/2+get_x(l1_space))*pos;
f=pack(align=E, l2);
add(f, pos, Align);

path box2=shift(code_word_width/2+get_x(l1_space))*box(0, (get_x(l2_space), get_y(l6)*1.1));
draw(box2);

f=pack(align=E, s2);
add(f, shift(0, get_y(l2))*pos, Align);

pos=shift(code_word_width+get_x(l2_space))*pos;
f=pack(align=E, l3);
add(f, pos, Align);

pos=shift(get_x(l3_space))*pos;
f=pack(align=E, l4);
add(f, pos, Align);

// notes for first line
pair pn1=shift(-space_width, get_y(l6))*max(box2);
f=pack(align=E, n1);
add(f, pn1, Align);

pair a1_start=shift(0,space_height/3)*pn1;
pair a1_mid=shift(-space_width*2.5,space_height/10)*a1_start;
pair a1_end=shift(0,space_height/5)*max(box1);
draw(a1_start..arc(shift(space_width/2,-space_width/2)*a1_mid, space_width/2, 100, 180)..a1_end, arrow=Arrow(size=ah));

pair pn2=shift(space_width*3, get_y(l6)*1.5)*pos;
f=pack(align=E, n2);
add(f, pn2, Align);

pair a2_start=shift(0,space_height/2)*pn2;
pair a2_end=shift(space_width*4.5)*a1_end;
draw(a2_start{left}..{(-1,-3)}a2_end, arrow=Arrow(size=ah));

// second row and its comments
pos=shift(0,-get_y(l5)*2-get_y(s1))*(0,0);
f=pack(align=E, l5);
add(f, pos, Align);

path box3=box(pos, (pos.x+get_x(l5_space), pos.y+get_y(l6)*1.1));
draw(box3);

pair pos1=shift(0,get_y(l5))*pos;
f=pack(align=E, s3);
add(f, pos1, Align);

pos=shift(get_x(l5_space)*1.8)*pos;
f=pack(align=E, l6);
add(f, pos, Align);

path box4=box(pos, (pos.x+get_x(l6), pos.y+get_y(l6)*1.1));
draw(box4);

pair pos2=shift(0,get_y(l5))*pos;
f=pack(align=E, s4);
add(f, pos2, Align);

pos=shift(get_x(l5_space)*0.8+get_x(l6))*pos;
f=pack(align=E, l7);
add(f, pos, Align);

Label l7_space="\Huge{\tt{baz}}";
path box5=box(pos, (pos.x+get_x(l7_space), pos.y+get_y(l6)*1.1));
draw(box5);

pair pos3=shift(0,get_y(l5))*pos;
f=pack(align=E, s5);
add(f, pos3, Align);

pair pn3=shift(space_width*2, -get_y(l6))*min(box4);
f=pack(align=E, n3);
add(f, pn3, Align);

pair a2_mid=shift(-space_width,-space_height*2.5)*min(box4);
draw(pn3{SW}..a2_mid..{(-1,5)}shift(0,-space_height/10)*min(box3), arrow=Arrow(size=ah));

pair a3_mid=shift(space_width/5,-space_height*1.8)*min(box4);
draw(pn3{(-2,-1)}..a3_mid..{up}shift(0,-space_height/10)*min(box4), arrow=Arrow(size=ah));

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
