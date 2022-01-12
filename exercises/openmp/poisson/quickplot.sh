#!/bin/bash
Data="./res/Tfield.dat"
if [[ $1=="png" ]]
then
    gnuplot-x11 -e "set palette rgb 33,13,10; splot '$Data' u (\$1<=0.1 && \$1>=-0.1?\$1:NaN):(\$2<=0.9 && \$2>=-0.9?\$2:NaN):3:(\$4<19?\$4:NaN) w p pt 7 ps 2 palette notitle; set term png; set out 'Tfield.png'; repl"
    okular Tfield.png
else
    gnuplot-x11 -e "set palette rgb 33,13,10; splot '$Data' u (\$1<=0.9 && \$1>=-0.9?\$1:NaN):(\$2<=0.9 && \$2>=-0.9?\$2:NaN):3:(\$4<19?\$4:NaN) w p pt 7 ps 2 palette notitle; set term post eps color solid; set out 'Tfield.eps'; repl"
    okular Tfield.eps
fi