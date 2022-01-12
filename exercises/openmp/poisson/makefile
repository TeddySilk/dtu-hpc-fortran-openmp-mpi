#----------------------------------------------------------------------
#  Makefile : poisson
#  Version  : 0.5                                                         
#  Author   : Kristian Ebstrup Jacobsen                                                         
#  Created  : January 11, 2022
#----------------------------------------------------------------------
SHELL       = /bin/sh
TARGET		= poisson

#----------------------------------------------------------------------
#  Compiler settings (Linux)
#----------------------------------------------------------------------
F77         = gfortran
CC          = cc
DEBUG       = -C
DEBUG       = 
OPT         = -O3
FFLAGS      = $(OPT) -free $(DEBUG) -J ./${MOD} -I ./${MOD} -fopenmp -ffast-math
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
	${OBJ}/m_global.o\
	${OBJ}/m_timer.o\
	${OBJ}/m_init.o\
	${OBJ}/m_input.o\
	${OBJ}/m_extract.o\
	${OBJ}/m_solver.o\
	${OBJ}/poisson.o

#----------------------------------------------------------------------
#  Dependencies:                                                       
#  NOTE: add the dependencies here explicitly ! 
#----------------------------------------------------------------------
${OBJ}/poisson.o: ${SRC}/poisson.f90 ${SRC}/m_init.f90 ${SRC}/m_timer.f90 ${SRC}/m_input.f90 ${SRC}/m_global.f90 ${SRC}/m_extract.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_init.o: ${SRC}/m_init.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_timer.o: ${SRC}/m_timer.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_extract.o: ${SRC}/m_extract.f90 ${SRC}/m_global.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_input.o: ${SRC}/m_input.f90 ${SRC}/m_global.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_global.o: ${SRC}/m_global.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_solver.o: ${SRC}/m_solver.f90 ${SRC}/m_global.f90 ${SRC}/m_init.f90
	${F77} ${FFLAGS} -c $< -o $@

#----------------------------------------------------------------------
#  Link                                                                
#----------------------------------------------------------------------
${TARGET}: ${OBJS}
	${F77} ${FFLAGS} -o $@ $+

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