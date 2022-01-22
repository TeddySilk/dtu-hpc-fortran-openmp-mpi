#----------------------------------------------------------------------
#  Makefile : diff2d
#  Version  : 0.5                                                         
#  Author   : Kristian Ebstrup Jacobsen                                                         
#  Created  : January 8, 2022
#----------------------------------------------------------------------
SHELL       = /bin/sh
TARGET		= diff2d_mpi.e
np			= 1

#----------------------------------------------------------------------
#  Compiler settings (Linux)
#----------------------------------------------------------------------
F77         = mpif90
CC          = cc
DEBUG       = -C
DEBUG       = 
OPT         = -O3
FFLAGS      = $(OPT) -free $(DEBUG) -J ./${MOD} -I ./${MOD}
CFLAGS      = -O
LD          = $(F77)
LDFLAGS     = 
CPP         = /lib/cpp
DEFINE      = 
LIBS        = 

#----------------------------------------------------------------------
#  Directories
#----------------------------------------------------------------------
RES			= res
MOD			= .mod
OBJ			= obj
SRC			= src

ifneq ($(RES),)
  $(shell test -d $(RES) || mkdir -p $(RES))
endif

ifneq ($(MOD),)
  $(shell test -d $(MOD) || mkdir -p $(MOD))
endif

ifneq ($(OBJ),)
  $(shell test -d $(OBJ) || mkdir -p $(OBJ))
endif

#----------------------------------------------------------------------
#  Default target
#----------------------------------------------------------------------
all: $(TARGET)

#----------------------------------------------------------------------
#  Object files:                                                       
#  NOTE: you HAVE to sort the objects files such that no file will 
#  depend on files below it ! 
#----------------------------------------------------------------------
OBJS =\
	${OBJ}/m_precision.o\
	${OBJ}/m_arrays.o\
	${OBJ}/m_global.o\
	${OBJ}/m_timer.o\
	${OBJ}/m_init.o\
	${OBJ}/m_io.o\
	${OBJ}/m_diffuse.o\
	${OBJ}/main.o

#----------------------------------------------------------------------
#  Dependencies:                                                       
#  NOTE: add the dependencies here explicitly ! 
#----------------------------------------------------------------------
${OBJ}/main.o: ${SRC}/main.f90 ${SRC}/m_global.f90 ${SRC}/m_precision.f90 ${SRC}/m_timer.f90 ${SRC}/m_io.f90 ${SRC}/m_init.f90 ${SRC}/m_diffuse.f90
	${F77} ${FFLAGS} -c $< -o $@
	
${OBJ}/m_diffuse.o: ${SRC}/m_diffuse.f90 ${SRC}/m_global.f90 ${SRC}/m_arrays.f90 ${SRC}/m_precision.f90 ${SRC}/m_io.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_init.o: ${SRC}/m_init.f90 ${SRC}/m_global.f90 ${SRC}/m_precision.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_io.o: ${SRC}/m_io.f90 ${SRC}/m_arrays.f90 ${SRC}/m_global.f90 ${SRC}/m_precision.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_arrays.o: ${SRC}/m_arrays.f90 ${SRC}/m_precision.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_global.o: ${SRC}/m_global.f90 ${SRC}/m_precision.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_precision.o: ${SRC}/m_precision.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_timer.o: ${SRC}/m_timer.f90 ${SRC}/m_precision.f90 ${SRC}/m_global.f90
	${F77} ${FFLAGS} -c $< -o $@

#----------------------------------------------------------------------
#  Link                                                                
#----------------------------------------------------------------------
${TARGET}: ${OBJS}
	${F77} -o $@ ${FFLAGS} $+

#----------------------------------------------------------------------
#  Clean                                                               
#----------------------------------------------------------------------

.PHONY: clean
clean :
	rm -f ${OBJ}/* ${MOD}/* ${SRC}/*.mod ${RES}/*

.PHONY: clean_src
clean_src :
	rm -f ${SRC}/*.mod

.PHONY: clean_res
clean_res :
	rm -f ${RES}/*

#----------------------------------------------------------------------
#  Run or Plot
#----------------------------------------------------------------------
.PHONY: r
r :
	mpirun -np ${np} ./diff2d_mpi.e

.PHONY: p
p :
	gnuplot-x11 -e "sp'res/field@000200.dat' u 1:2:3" -pause