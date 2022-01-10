
# INSTALLATION AND RUNNING
To get the program running, do as follows:

1. COMPILE
Open a terminal in "../heat_diffusion" and type in the command "make"

2. RUN
Run the executable "heat_diffusion_2d.exe". It will generate an input file on first run.

Check the input file if you want to change anything. Check m_global.f90 in the source folder (/src)
to see descriptions of the different inputs.


# RUNNING FROM PREVIOUSLY SAVED FIELD
To run from a previously saved field, when running the .exe file in the command line, 
argument 1 will be the field path, and argument 2 the step count for the field:

> heat_diffusion_2d.exe [arg1] [arg2]

For example,

> heat_diffusion_2d.exe res/Tfield@000200.bin 200

The program does not support extracting the step count, and must be input manually.


# CLEAN-UP
The makefile supports three different clean-up commands