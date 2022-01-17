#!/bin/bash
Data="./res/Tfield.dat"
fontsize=18
if [[ $1 == "png" ]]
then
    gnuplot-x11 -e "set palette rgb 33,13,10; splot '$Data' u (\$1==-0.5 ?\$1:NaN):2:3:4 w p pt 7 ps 2 palette notitle; set xrange [-1: 1]; set yrange [-1: 1]; set zrange[-1: 1]; set term png; set out 'Tfield.png'; repl"
    okular Tfield.png
else
    gnuplot-x11 -e "set palette rgb 33,13,10; splot '$Data' u (\$1<=1.0 && \$1>=-1.0?\$1:NaN):(\$2<=1.0 && \$2>=-1.0?\$2:NaN):(\$3>=-0.1 && \$3<=0.1?\$3:NaN):(\$4>0.0 && \$4<20.0?\$4:NaN) w p pt 7 ps 2 palette notitle; set grid; set cblabel 'T' font ',$fontsize'; set xlabel 'x' font ',$fontsize'; set ylabel 'y' font ',$fontsize'; set zlabel 'z' font ',$fontsize'; set term post eps color solid; set xtics font ', $fontsize'; set ytics font ', $fontsize'; set ztics font ', $fontsize'; set cbtics font ', $fontsize'; set out 'Tfield.eps'; repl"
    okular Tfield.eps
fi