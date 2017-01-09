.SUFFIXES: .F90 .o

extension = .F90

COMP_NAME=NONE
GPTL_DIR=NONE
USEMPI=FALSE

OBJ_ROOT = $(realpath ../obj)
OBJ_DIR = $(OBJ_ROOT)/$(COMP_NAME)/driver
INC_DIR = $(realpath ../../include)
LIB_DIR = $(realpath ../../lib)
EXE_DIR = $(realpath ../driver_exe)
SRC_DIR = $(realpath ../../src)
DRV_DIR = $(realpath .)

OBJ_LOC = $(realpath $(OBJ_ROOT)/$(COMP_NAME))
INC_LOC = $(realpath $(INC_DIR)/$(COMP_NAME))
ifeq ($(COMP_NAME),NONE)
  LIB_NAME = libmarbl.a
  LFLAG = -lmarbl
else
  LIB_NAME = libmarbl-$(COMP_NAME).a
  LFLAG = -lmarbl-$(COMP_NAME)
endif
CPPDEFS=-DECOSYS_NT=27 -DZOOPLANKTON_CNT=1 -DAUTOTROPH_CNT=3 -DGRAZER_PREY_CNT=3
EXECPPDEFS=
INCLUDES=-I$(INC_LOC)
LINKS=-L$(LIB_DIR) $(LFLAG)

ifneq ($(GPTL_DIR),NONE)
  CPPDEFS+= -DUSE_GPTL
  EXECPPDEFS+= -DUSE_GPTL
  INCLUDES+= -I$(GPTL_DIR)/include
  LINKS+= -L$(GPTL_DIR)/lib -lgptl
endif
ifeq ($(USEMPI),TRUE)
  CPPDEFS+= -DHAVE_MPI
  EXECPPDEFS+= -DHAVE_MPI
  MPISUF=-mpi
  override FC = mpif90
endif

COMP_ARGS = OBJ_DIR=$(OBJ_LOC) LIB_DIR=$(LIB_DIR) INC_DIR=$(INC_LOC) CPPDEFS="$(CPPDEFS)"
EXE       = $(EXE_DIR)/marbl.exe

OBJS = marbl_init_namelist_drv.o       \
       marbl_init_no_namelist_drv.o    \
       marbl_get_put_drv.o             \
       marbl_mpi_mod.o                 \
       marbl.o

MODS = marbl_mpi_mod.mod

.PHONY: intel pgi gnu nag cray clean clean_exe

all: gnu

$(EXE): clean_exe                        \
        $(LIB_DIR)/$(LIB_NAME)           \
        $(addprefix $(OBJ_DIR)/,$(OBJS))
	$(FC) -o $(EXE) $(OBJ_DIR)/*.o $(LINKS)

$(OBJ_DIR)/%.o: $(DRV_DIR)/%.F90
	$(FC) $(EXECPPDEFS) $(FCFLAGS) $(INC2) $(INCLUDES) -c $< -o $@

$(LIB_DIR)/%.a: $(SRC_DIR)/*.F90
	cd $(SRC_DIR) ; make FC=$(FC) FCFLAGS="$(FCFLAGS) $(INC)" USE_DEPS=TRUE $(COMP_ARGS) $@

intel:
	$(MAKE) $(EXE) COMP_NAME=intel$(MPISUF) FC=ifort FCFLAGS="-O2 -free -cpp -nogen-interface -fp-model source" INC="-module $(OBJ_ROOT)/intel$(MPISUF)" INC2="-module $(OBJ_ROOT)/intel$(MPISUF)/driver" 

pgi:
	$(MAKE) $(EXE) COMP_NAME=pgi FC=pgf90 FCFLAGS="-O2 -Mfree" INC="-module $(OBJ_ROOT)/pgi" INC2="-module $(OBJ_ROOT)/pgi/driver"

gnu:
	$(MAKE) $(EXE) COMP_NAME=gnu$(MPISUF) FC=gfortran FCFLAGS="-O2 -ffree-form -cpp" INC="-J $(OBJ_ROOT)/gnu$(MPISUF)" INC2="-J $(OBJ_ROOT)/gnu$(MPISUF)/driver"

nag:
	$(MAKE) $(EXE) COMP_NAME=nag$(MPISUF) FC=nagfor FCFLAGS="-O2 -free -kind=byte" INC="-mdir $(OBJ_ROOT)/nag$(MPISUF) -I$(OBJ_ROOT)/nag$(MPISUF)" INC2="-mdir $(OBJ_ROOT)/nag$(MPISUF)/driver -I$(OBJ_ROOT)/nag$(MPISUF)/driver"

cray:
	$(MAKE) $(EXE) COMP_NAME=cray FC=ftn FCFLAGS="-O2 -f free -e mf" INC="-J $(OBJ_ROOT)/cray" INC2="-J $(OBJ_ROOT)/cray/driver"

clean_exe:
	rm -f $(EXE)

clean: clean_exe
	rm -f $(OBJ_ROOT)/*/*.* $(OBJ_ROOT)/*/*/* $(LIB_DIR)/* $(INC_DIR)/*/*
