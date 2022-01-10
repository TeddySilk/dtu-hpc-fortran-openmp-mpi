#----------------------------------------------------------------------
#  Makefile : heat_diffusion_2d     
#  Version  : 0.5                                                         
#  Author   : Kristian Ebstrup Jacobsen                                                         
#  Created  : January 8, 2022
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
	${OBJ}/m_read_input.o\
	${OBJ}/m_swap.o\
	${OBJ}/m_copy_arrays.o\
	${OBJ}/m_alloc.o\
	${OBJ}/m_diagnostics.o\
	${OBJ}/m_init_tfield.o\
	${OBJ}/m_extract_field.o\
	${OBJ}/m_extract_binary.o\
	${OBJ}/m_simulate_diffusion.o\
	${OBJ}/main.o

#----------------------------------------------------------------------
#  Dependencies:                                                       
#  NOTE: add the dependencies here explicitly ! 
#----------------------------------------------------------------------
${OBJ}/main.o: ${SRC}/main.f90 ${SRC}/m_global.f90 ${SRC}/m_alloc.f90 ${SRC}/m_extract_binary.f90 ${SRC}/m_extract_field.f90 ${SRC}/m_init_tfield.f90 ${SRC}/m_simulate_diffusion.f90 ${SRC}/m_read_input.f90
	${F77} ${FFLAGS} -c $< -o $@
	
${OBJ}/m_simulate_diffusion.o: ${SRC}/m_simulate_diffusion.f90 ${SRC}/m_global.f90 ${SRC}/m_alloc.f90 ${SRC}/m_copy_arrays.f90 ${SRC}/m_swap.f90 ${SRC}/m_diagnostics.f90 ${SRC}/m_extract_field.f90 ${SRC}/m_extract_binary.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_extract_field.o: ${SRC}/m_extract_field.f90 ${SRC}/m_global.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_extract_binary.o: ${SRC}/m_extract_binary.f90 ${SRC}/m_global.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_init_tfield.o: ${SRC}/m_init_tfield.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_diagnostics.o: ${SRC}/m_diagnostics.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_global.o: ${SRC}/m_global.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_alloc.o: ${SRC}/m_alloc.f90 ${SRC}/m_copy_arrays.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_copy_arrays.o: ${SRC}/m_copy_arrays.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_swap.o: ${SRC}/m_swap.f90
	${F77} ${FFLAGS} -c $< -o $@

${OBJ}/m_read_input.o: ${SRC}/m_read_input.f90
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