#----------------------------------------------------------------------
#  Makefile : heat_diffusion_2d     
#  Version  : 0.4                                                         
#  Author   : Kristian Ebstrup Jacobsen                                                         
#  Created  : January 7, 2022
#----------------------------------------------------------------------
SHELL       = /bin/sh
TARGET		= heat_diffusion_2d.exe

#----------------------------------------------------------------------
#  Compiler settings (Linux)
#----------------------------------------------------------------------
F77         = f95
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
	${OBJ}/m_global.o\
	${OBJ}/m_arrays.o\
	${OBJ}/m_io.o\
	${OBJ}/m_heat_diffusion.o\
	${OBJ}/main.o

#----------------------------------------------------------------------
#  Dependencies:                                                       
#  NOTE: add the dependencies here explicitly ! 
#----------------------------------------------------------------------
${OBJ}/main.o: ${SRC}/main.f90 ${SRC}/m_global.f90 ${SRC}/m_io.f90 ${SRC}/m_heat_diffusion.f90 ${SRC}/m_arrays.f90
	${F77} ${FFLAGS} -c $< -o $@
${OBJ}/m_heat_diffusion.o: ${SRC}/m_heat_diffusion.f90 ${SRC}/m_io.f90 ${SRC}/m_arrays.f90
	${F77} ${FFLAGS} -c $< -o $@
${OBJ}/m_io.o: ${SRC}/m_io.f90 ${SRC}/m_global.f90
	${F77} ${FFLAGS} -c $< -o $@
${OBJ}/m_global.o: ${SRC}/m_global.f90
	${F77} ${FFLAGS} -c $< -o $@
${OBJ}/m_arrays.o: ${SRC}/m_arrays.f90
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

