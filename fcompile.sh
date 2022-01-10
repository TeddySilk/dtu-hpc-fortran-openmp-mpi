#!/bin/bash
# ---------------------------------------------- #
# A simple bash-script to automate compiling and #
# running a fortran script                       #
# ---------------------------------------------- #

# check if command line is empty
if [[ -z "$1" ]]
then
    echo -n "Input FORTRAN script name >> "
    read script_name
else
    script_name=$1
fi

# check if input contains file extension 
IFS='.'                             # dot (.) is set as delimiter
read -ra INPUT <<< ${script_name}   # str is read into an array as tokens separated by IFS
IFS=' '

# if input does not contain a file extension, check if it is .f90
if [ ${#INPUT[@]} == 1 ] && [ "${INPUT[0]}.f90" == "${INPUT[0]}.*"]
then
    f95 -free "${INPUT[0]}.f90" -o "${INPUT[0]}.out"
    ${INPUT[0]}.out
elif [[ ${#INPUT[@]} == 2 ]]
then
    f95 -free "${INPUT[0]}.${INPUT[-1]}" -o "${INPUT[0]}.out"
    ${INPUT[0]}.out
else
    echo "Bad input. Too many file extensions."
fi