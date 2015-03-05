import graph;
import plain;
import markers;
import trembling;
usepackage("amsmath");
usepackage("amssymb");
tremble trlite = tremble(angle=15,frequency=.25,random=1,fuzz=2);
defaultpen(fontsize(11pt));
size(240,180);

pair orig=((2.5,2.5));
pair xfirst=((5,-1.7));
pair xsecond=((3,1.5));
pair resid=((0,4.5));
pair regres=(0.6*xfirst+0.8*xsecond);
pair yvec=(regres+resid);

fill(orig+regres+(0, 0.4)--orig+regres+(-0.4, 0.4)--orig+regres+(-0.4, 0)--orig+regres--cycle, blue+opacity(0.4)); // Right angle
fill(orig+0.6*xfirst+0.12*xsecond--orig+0.52*xfirst+0.12*xsecond--orig+0.52*xfirst--orig+0.6*xfirst--cycle,blue+opacity(0.4)); // Slanted right angle
fill(orig+0.6*xfirst--orig+0.68*xfirst--orig+0.68*xfirst+0.08*(yvec-0.6*xfirst)--orig+0.6*xfirst+0.08*(yvec-0.6*xfirst)--cycle, green+opacity(0.5)); // Slanted right angle


draw(trlite.deform((0,0)--(2+.8,4.8)--(11-.8,4.8)--(9,0)--cycle),black+1pt+opacity(0.5));
draw(orig--orig+1.1*xfirst,black+1.5pt,Arrow(8));
draw(orig--orig+1.1*xsecond,black+1.5pt,Arrow(8));
draw(orig--orig+yvec,black+1.5pt,Arrow(8));
draw(orig--orig+regres,black+1.5pt,Arrow(8));
draw(orig+regres--orig+regres+resid,black+0.75pt+dashed,Arrow(SimpleHead,8));

fill(orig+regres--orig+regres-0.11*xsecond--orig+regres-0.11*xsecond+(0, 0.5)--orig+regres+(0, 0.5)--cycle, green+opacity(0.4));
fill(orig+regres--orig+regres-0.11*xsecond--orig+regres-0.11*xsecond+(0, 0.5)--orig+regres+(0, 0.5)--cycle, green+opacity(0.4));

draw(orig--orig+0.6*xfirst,black+1pt,Arrow(SimpleHead,8)); 
draw(orig+0.6*xfirst--orig+regres,black+0.75pt+dashed,Arrow(SimpleHead,8)); 
draw(orig+0.6*xfirst--orig+regres+resid,black+0.75pt+dashed,Arrow(SimpleHead,8)); 

dot((orig+regres),black);
dot((orig+0.6*xfirst),black);

markangle("$\varphi$", orig+regres,orig+0.6*xfirst,orig+regres+resid, n=2, radius=16);
// perpendicular(orig+0.6*xfirst,NE,F-B,size=10mm,1mm+red)

label("$\vec 1$",orig+1.1*xfirst,0.5S+E,black);
label("$\boldsymbol{x}$",orig+1.1*xsecond,NW,black);
label("$\hat {\boldsymbol y}$",orig+regres,E,black);
label("$\hat{\boldsymbol \varepsilon} = \boldsymbol y - \hat {\boldsymbol y}$",orig+regres+0.6*resid,E);
label("$\boldsymbol y$",orig+0.8yvec,0.6N+0.4W,black);
label("$\cos^2 \varphi = R^2$",orig+0.8*resid,black);

label("$\bar {\vphantom{1} y} \cdot \vec 1$",orig+0.6*xfirst,SW,black);
label("$\hat {\boldsymbol y} - \bar {\vphantom{1} y} \vec 1$",orig+0.6*xfirst+0.4*xsecond,SE,black);
label("$\boldsymbol y - \bar {\vphantom{1} y} \vec 1$",orig+0.6*xfirst+0.4*xsecond+0.5*resid,S+0.2E,black,UnFill);

