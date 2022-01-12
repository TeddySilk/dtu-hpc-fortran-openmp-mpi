#!/bin/bash
Data="./res/Tfield.dat"
gnuplot-x11 -e "set palette rgb 33,13,10; splot '$Data' u 1:2:3:4 w p pt 7 ps 2 palette notitle; set term post eps color solid; set out 'Tfield.eps'; repl"