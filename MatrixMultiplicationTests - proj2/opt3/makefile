PFUNIT = /opt/pfunit/pfunit-serial
F90_VENDOR = GNU

include $(PFUNIT)/include/base.mk

FFLAGS += -std=f2008 -pedantic -I$(PFUNIT)/mod
LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)

PFS = $(wildcard *.pf)
OBJS = $(PFS:.pf=.o)

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@

%.o: %.F90
	$(F90) $(FFLAGS) -c $<

test: testSuites.inc mult.o $(OBJS)
	$(F90) -o $@ -I. -I$(PFUNIT)/mod -I$(PFUNIT)/include \
		$(PFUNIT)/include/driver.F90 \
		./*$(OBJ_EXT) $(LIBS) $(FFLAGS)

normal:
	$(F90) -O2 -DDOT=0 -DCACHE=0 -DUSENORMAL=0 -std=f2008 mult.F90 measure.F90 -o $@
dot:
	$(F90) -O2 -DDOT=1 -DCACHE=0 -DUSENORMAL=0 -std=f2008 mult.F90 measure.F90 -o $@

cache:
	$(F90) -O2 -DDOT=0 -DCACHE=1 -DUSENORMAL=0 -std=f2008 mult.F90 measure.F90 -o $@

both:
	$(F90) -O2 -DDOT=1 -DCACHE=1 -DUSENORMAL=0 -std=f2008 mult.F90 measure.F90 -o $@

matmul:
	$(F90) -O2 -DUSENORMAL=1 -std=f2008 measure.F90 -o $@	

all:
	make normal
	make dot
	make cache
	make both
	make matmul