.SUFFIXES: 

FC = f95

MOD := .mod
ifneq ($(MOD),)
  $(shell test -d $(MOD) || mkdir -p $(MOD))
endif

OBJ := obj
ifneq ($(OBJ),)
  $(shell test -d $(OBJ) || mkdir -p $(OBJ))
endif

SRC := src

EXE = e2a.exe
DEPENDENCIES = ${OBJ}/main.o ${OBJ}/m_init.o ${OBJ}/m_sub.o
${EXE} : ${DEPENDENCIES}
	${FC} -I ./${MOD} $+ -o $@

SRC_FILE=main
DEPENDENCIES = ${OBJ}/m_init.o ${OBJ}/m_sub.o
${OBJ}/${SRC_FILE}.o : ${SRC}/${SRC_FILE}.f90 ${DEPENDENCIES}
	${FC} -J ./${MOD} -free -c $< -o $@

SRC_FILE=m_init
${OBJ}/${SRC_FILE}.o : ${SRC}/${SRC_FILE}.f90
	${FC} -J ./${MOD} -free -c $< -o $@

SRC_FILE=m_sub
${OBJ}/${SRC_FILE}.o : ${SRC}/${SRC_FILE}.f90
	${FC} -J ./${MOD} -free -c $< -o $@

.PHONY: clean
clean :
	rm -f ${OBJ}/* ${MOD}/* ${SRC}/*.mod

.PHONY: clean_src
clean_src :
	rm -f ${SRC}/*.mod

