PFUNIT = /opt/pfunit/pfunit-serial
F90_VENDOR = GNU

include $(PFUNIT)/include/base.mk

FFLAGS += -std=f2008 -pedantic -I$(PFUNIT)/mod
LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)

PFS = $(wildcard *.pf)
OBJS = $(PFS:.pf=.o)

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@

normal.o: mult.F90
	$(F90) -DDOT=0 -DCACHE=0 $(FFLAGS) -c $<

test_normal: testSuites.inc normal.o $(OBJS)
	$(F90) -o $@ -I. -I$(PFUNIT)/mod -I$(PFUNIT)/include \
		$(PFUNIT)/include/driver.F90 \
		./*$(OBJ_EXT) $(LIBS) $(FFLAGS)

dot.o: mult.F90
	$(F90) -DDOT=1 -DCACHE=0 $(FFLAGS) -c $<

test_dot: testSuites.inc dot.o $(OBJS)
	$(F90) -o $@ -I. -I$(PFUNIT)/mod -I$(PFUNIT)/include \
		$(PFUNIT)/include/driver.F90 \
		./*$(OBJ_EXT) $(LIBS) $(FFLAGS)

cache.o: mult.F90
	$(F90) -DDOT=0 -DCACHE=1 $(FFLAGS) -c $<

test_cache: testSuites.inc cache.o $(OBJS)
	$(F90) -o $@ -I. -I$(PFUNIT)/mod -I$(PFUNIT)/include \
		$(PFUNIT)/include/driver.F90 \
		./*$(OBJ_EXT) $(LIBS) $(FFLAGS)

both.o: mult.F90
	$(F90) -DDOT=1 -DCACHE=1 $(FFLAGS) -c $<

test_both: testSuites.inc both.o $(OBJS)
	$(F90) -o $@ -I. -I$(PFUNIT)/mod -I$(PFUNIT)/include \
		$(PFUNIT)/include/driver.F90 \
		./*$(OBJ_EXT) $(LIBS) $(FFLAGS)

tests:
	make test_normal
	make test_dot
	make test_cache
	make test_both




