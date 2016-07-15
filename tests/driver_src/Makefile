.SUFFIXES: .F90 .o

extension = .F90

COMP_NAME=NONE

OBJ_ROOT = $(realpath ../../obj)
OBJ_DIR = $(OBJ_ROOT)/$(COMP_NAME)/driver
INC_DIR = $(realpath ../../include)
LIB_DIR = $(realpath ../../lib)
EXE_DIR = $(realpath ../../exe)
SRC_DIR = $(realpath ../../src)
DRV_DIR = $(realpath .)

OBJ_LOC = $(realpath $(OBJ_ROOT)/$(COMP_NAME))
INC_LOC = $(realpath $(INC_DIR)/$(COMP_NAME))
ifeq ($(COMP_NAME),NONE)
  LIB_NAME = libmarbl.a
else
  LIB_NAME = libmarbl-$(COMP_NAME).a
endif
LFLAG = -lmarbl-$(COMP_NAME)

COMP_ARGS = OBJ_DIR=$(OBJ_LOC) LIB_DIR=$(LIB_DIR) INC_DIR=$(INC_LOC) CPPDEFS="-DECOSYS_NT=27 -DZOOPLANKTON_CNT=1 -DAUTOTROPH_CNT=3 -DGRAZER_PREY_CNT=3"
EXE       = $(EXE_DIR)/marbl.exe

OBJS = marbl_init_namelist_drv.o       \
       marbl_init_no_namelist_drv.o    \
       marbl_get_put_drv.o             \
       marbl.o

.PHONY: intel pgi gnu nag cray clean clean_exe

all: gnu

$(EXE): $(LIB_DIR)/$(LIB_NAME)           \
        $(addprefix $(OBJ_DIR)/,$(OBJS)) \
        clean_exe
	$(FC) -o $(EXE) $(OBJ_DIR)/*.o -L$(LIB_DIR) $(LFLAG)

$(OBJ_DIR)/%.o: $(DRV_DIR)/%.F90
	$(FC) $(FCFLAGS) $(INC2) -I$(INC_LOC) -c $< -o $@

$(LIB_DIR)/%.a: $(SRC_DIR)/*.F90
	cd $(SRC_DIR) ; make FC=$(FC) FCFLAGS="$(FCFLAGS) $(INC)" USE_DEPS=TRUE $(COMP_ARGS) $@

intel:
	$(MAKE) $(EXE) COMP_NAME=intel FC=ifort FCFLAGS="-O2 -free -cpp -nogen-interface -fp-model source" INC="-module $(OBJ_ROOT)/intel" INC2="-module $(OBJ_ROOT)/intel/driver" 

pgi:
	$(MAKE) $(EXE) COMP_NAME=pgi FC=pgf90 FCFLAGS="-O2 -Mfree" INC="-module $(OBJ_ROOT)/pgi" INC2="-module $(OBJ_ROOT)/pgi/driver"

gnu:
	$(MAKE) $(EXE) COMP_NAME=gnu FC=gfortran FCFLAGS="-O2 -ffree-form -cpp" INC="-J $(OBJ_ROOT)/gnu" INC2="-J $(OBJ_ROOT)/gnu/driver"

nag:
	$(MAKE) $(EXE) COMP_NAME=nag FC=nagfor FCFLAGS="-O2 -free -kind=byte" INC="-mdir $(OBJ_ROOT)/nag -I$(OBJ_ROOT)/nag" INC2="-mdir $(OBJ_ROOT)/nag/driver -I$(OBJ_ROOT)/nag/driver"

cray:
	$(MAKE) $(EXE) COMP_NAME=cray FC=ftn FCFLAGS="-O2 -f free -e mf" INC="-J $(OBJ_ROOT)/cray" INC2="-J $(OBJ_ROOT)/cray/driver"

clean_exe:
	rm -f $(EXE)

clean: clean_exe
	rm -f $(OBJ_ROOT)/*/*.* $(OBJ_ROOT)/*/*/* $(LIB_DIR)/* $(INC_DIR)/*/*
