import flowchart;
texpreamble("\usepackage{xeCJK}
\setCJKmainfont[BoldFont={WenQuanYi Micro Hei}, ItalicFont={AR PL UKai CN}]{Adobe Song Std}
\setCJKsansfont{Adobe Song Std}
\setCJKmonofont{Adobe Song Std}
\xeCJKsetup{CJKmath=true, PlainEquation=true}
\usepackage[T1]{fontenc}
");
real fsz=12pt;
real dsz=8pt;
real ah=4;
real border=1;                  // border width around lable
defaultpen(fontsize(fsz));
unitsize(dsz);

Label module_1="\large{爱丽丝的模块}";
Label[] title_1={"\large{爱丽丝的接口}",
                 "\small\texttt{\phantom{p}}",
                 "\small\texttt{foo : (int -> int)}",
                 "\small\texttt{bar : (int -> bool)}",
                 "\small\texttt{\phantom{p}}"};

Label[] content_1={"\texttt{\phantom{p}}",
                   "\large{爱丽丝的实现}",
                   "\small\texttt{\phantom{p}}",
                   "\small\texttt{foo-helper = ...}",
                   "\small\texttt{foo = proc (x : int) ...}",
                   "\small\texttt{bar = proc (x : int) ...}",
                   "\small\texttt{\phantom{p}}",
                   "\small\texttt{\phantom{p}}"};

Label module_2="鲍伯的模块";
Label[] title_2={"\small{鲍伯的接口}",
                 "\tiny{\tiny{\tiny{\texttt{}}}}",
                 "\scriptsize{\texttt{foo : (int -> int)}}",
                 "\scriptsize{\texttt{bar : (bool -> int)}}",
                 "\tiny\texttt{\phantom{bar = proc (x : int) ...}}"};

Label[] content_2={"\tiny\texttt{\phantom{p}}",
                   "\small{鲍伯的实现}",
                   "\scriptsize{\texttt{\phantom{fp}}}",
                   "\scriptsize{\texttt{\phantom{fp}}}",
                   "\scriptsize{\texttt{\phantom{fp}}}",
                   "\scriptsize{\texttt{\phantom{fp}}}",
                   "\scriptsize{\texttt{\phantom{fp}}}",
                   "\scriptsize{\texttt{\phantom{fp}}}"};

Label module_3="查理的模块";
Label[] title_3={"\small{查理的接口}",
                 "\tiny{\tiny{\tiny{\texttt{}}}}",
                 "\scriptsize{\texttt{quux : (int -> int)}}",
                 "\scriptsize{\texttt{baz : (bool -> int)}}",
                 "\tiny{\texttt{\phantom{bar = proc (x : int) ...}}}"};

Label[] content_3={"\tiny{\texttt{\phantom{p}}}",
                   "\small{查理的实现}",
                   "\scriptsize{\texttt{\phantom{fp}}}",
                   "\scriptsize{\texttt{\phantom{fp}}}",
                   "\scriptsize{\texttt{\phantom{fp}}}",
                   "\scriptsize{\texttt{\phantom{fp}}}",
                   "\scriptsize{\texttt{\phantom{fp}}}",
                   "\scriptsize{\texttt{\phantom{fp}}}"};

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


// For the third part
real width=0;
pair pos=(0,0);
pair corner=pos;

// compute width of third block
for (Label l : title_3) {
  real x=get_x(l);
  if (x > width) {
    width=x;
  }
}

for (Label l : content_3) {
  real x=get_x(l);
  if (x > width) {
    width=x;
  }
}

// draw title part
frame f=pack(align=S, module_3);
add(f, (pos.x, pos.y+border), Align);

for (Label l : title_3) {
  real y=get_y(l);
  pos=(pos.x, pos.y-y);
  frame f=pack(align=S, l);
  add(f, pos, Align);
};

real h=get_y(title_3[0]);
path box4=box((corner.x-border, corner.y+get_x(title_3[1])/2), (corner.x+width+border, pos.y));
corner=pos;

// draw content part
for (Label l : content_3) {
  real y=get_y(l);
  pos=(pos.x, pos.y-y);
  frame f=pack(align=S, l);
  add(f, pos, Align);
}

h=get_y(title_3[0]);
path box5=box((corner.x-border,corner.y), (corner.x+width+border, pos.y));

// draw boxes in the following order
filldraw(box4,white,black);
filldraw(box5,rgb(209/255, 209/255, 211/255),black);
layer();

// For the second part
pos=(pos.x-width+2.1*get_x(content_3[0]), max(box4).y-8*get_x(content_3[0]));
corner=pos;
width=0;

// compute width of second block
for (Label l : title_2) {
  real x=get_x(l);
  if (x > width) {
    width=x;
  }
}

for (Label l : content_2) {
  real x=get_x(l);
  if (x > width) {
    width=x;
  }
}

// draw title part
f=pack(align=S, module_2);
add(f, (pos.x, pos.y+border), Align);

for (Label l : title_2) {
  real y=get_y(l);
  pos=(pos.x, pos.y-y);
  frame f=pack(align=S, l);
  add(f, pos, Align);
};

h=get_y(title_2[0]);
path box2=box((corner.x-border, corner.y+get_x(title_2[1])/2), (corner.x+width+border, pos.y));
corner=pos;

// draw content part
for (Label l : content_2) {
  real y=get_y(l);
  pos=(pos.x, pos.y-y);
  frame f=pack(align=S, l);
  add(f, pos, Align);
}

h=get_y(title_2[0]);
path box3=box((corner.x-border,corner.y), (corner.x+width+border, pos.y));

filldraw(box2,white,black);
filldraw(box3,rgb(209/255, 209/255, 211/255),black);
layer();

// For the first block
pos=(min(box2).x-width-7*get_x(content_3[0]), max(box2).y-15*get_x(content_3[0]));
corner=pos;
width=0;

// compute width of first block
for (Label l : title_1) {
  real x=get_x(l);
  if (x > width) {
    width=x;
  }
}

for (Label l : content_1) {
  real x=get_x(l);
  if (x > width) {
    width=x;
  }
}

// draw title part
f=pack(align=S, module_1);
add(f, (pos.x, pos.y+get_x(title_1[1])+border), Align);

for (Label l : title_1) {
  real y=get_y(l);
  pos=(pos.x, pos.y-y);
  frame f=pack(align=S, l);
  add(f, pos, Align);
};

h=get_y(title_1[0]);
path box0=box((corner.x-border, corner.y+get_x(title_1[1])), (corner.x+width+border, pos.y));
corner=pos;

// draw content part
for (Label l : content_1) {
  real y=get_y(l);
  pos=(pos.x, pos.y-y);
  frame f=pack(align=S, l);
  add(f, pos, Align);
}

h=get_y(title_1[0]);
path box1=box((corner.x-border,corner.y), (corner.x+width+border, pos.y));

filldraw(box0,white,black);
filldraw(box1,white,black);

shipout(bbox(currentpicture, 2, 2, filltype=Draw(2, 2), p=invisible));
