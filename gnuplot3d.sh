#!/bin/bash
# ----------------------------------------------------- #
# A simple bash-script to automate the gnuplot plotting #
# ----------------------------------------------------- #

# check if command line is empty
if [[ -z "$1" ]]
then
    echo -n "Input field data name >> "
    read field_data
else
    field_data=$1
fi

# check if input contains file extension 
IFS='.'                             # dot (.) is set as delimiter
read -ra INPUT <<< ${field_data}   # str is read into an array as tokens separated by IFS
IFS=' '

# if input does not contain a file extension, assume it is .dat
if [[ ${#INPUT[@]} == 1 ]]
then
    gnuplot-x11 -e "sp'${INPUT[0]}.dat' u 1:2:3 w l; set term post eps color solid; set out '${INPUT[0]}.eps'; repl"
    okular "${INPUT[0]}.eps"
elif [[ ${#INPUT[@]} == 2 ]]
then
    gnuplot-x11 -e "sp'${INPUT[0]}.${INPUT[1]}' u 1:2:3 w l; set term post eps color solid; set out '${INPUT[0]}.eps'; repl"
    okular "${INPUT[0]}.eps"
else
    echo "Bad input. Too many file extensions."
fi