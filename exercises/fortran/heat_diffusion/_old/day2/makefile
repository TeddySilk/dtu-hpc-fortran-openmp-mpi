.SUFFIXES: 

FC = f95

RES := res
ifneq ($(RES),)
  $(shell test -d $(RES) || mkdir -p $(RES))
endif

MOD := .mod
ifneq ($(MOD),)
  $(shell test -d $(MOD) || mkdir -p $(MOD))
endif

OBJ := obj
ifneq ($(OBJ),)
  $(shell test -d $(OBJ) || mkdir -p $(OBJ))
endif

SRC := src

EXE = e2b.exe
DEPENDENCIES = ${OBJ}/main.o ${OBJ}/m_out.o ${OBJ}/m_init.o ${OBJ}/m_alloc.o ${OBJ}/m_constants.o ${OBJ}/m_copy.o
${EXE} : ${DEPENDENCIES}
	${FC} -I ./${MOD} $+ -o $@

SRC_FILE=main
DEPENDENCIES = ${OBJ}/m_out.o ${OBJ}/m_init.o ${OBJ}/m_alloc.o ${OBJ}/m_constants.o ${OBJ}/m_copy.o
${OBJ}/${SRC_FILE}.o : ${SRC}/${SRC_FILE}.f90 ${DEPENDENCIES}
	${FC} -J ./${MOD} -free -c $< -o $@

SRC_FILE=m_out
DEPENDENCIES = ${OBJ}/m_constants.o
${OBJ}/${SRC_FILE}.o : ${SRC}/${SRC_FILE}.f90 ${DEPENDENCIES}
	${FC} -J ./${MOD} -free -c $< -o $@

SRC_FILE=m_init
${OBJ}/${SRC_FILE}.o : ${SRC}/${SRC_FILE}.f90
	${FC} -J ./${MOD} -free -c $< -o $@

SRC_FILE=m_alloc
${OBJ}/${SRC_FILE}.o : ${SRC}/${SRC_FILE}.f90
	${FC} -J ./${MOD} -free -c $< -o $@

SRC_FILE=m_constants
${OBJ}/${SRC_FILE}.o : ${SRC}/${SRC_FILE}.f90
	${FC} -J ./${MOD} -free -c $< -o $@

SRC_FILE=m_copy
${OBJ}/${SRC_FILE}.o : ${SRC}/${SRC_FILE}.f90
	${FC} -J ./${MOD} -free -c $< -o $@

.PHONY: clean
clean :
	rm -f ${OBJ}/* ${MOD}/* ${SRC}/*.mod ${RES}/*

.PHONY: clean_src
clean_src :
	rm -f ${SRC}/*.mod

.PHONY: clean_res
clean_res :
	rm -f ${RES}/*

