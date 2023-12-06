.PHONY: all

include ../../Makefile.conf

SRC=PythonCVInterface.cpp ActionWithPython.cpp PythonFunction.cpp PlumedPythonEmbeddedModule.cpp

all: PythonCVInterface.$(SOEXT)

PythonCVInterface.$(SOEXT): $(SRC)
	plumed_program_name=$(program_name) python_bin=$(python_bin) ./standaloneCompile.sh

clean:
	rm -f $(OBJS) PythonCVInterface.$(SOEXT)

check: all
	$(MAKE) -C regtest testclean
	$(MAKE) -C regtest checkfail
