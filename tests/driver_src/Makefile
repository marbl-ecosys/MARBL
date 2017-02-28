.SUFFIXES: .F90 .o
extension = .F90

COMP_NAME=NONE
USEMPI=FALSE

# DEFINE VARIOUS DIRECTORIES NEEDED FOR BUILD
OBJ_ROOT = $(realpath ../obj)
INC_DIR = $(realpath ../../include)
LIB_DIR = $(realpath ../../lib)
EXE_DIR = $(realpath ../driver_exe)
SRC_DIR = $(realpath ../../src)
DRV_DIR = $(realpath .)

# ERROR CHECKING: MAKE SURE ALL DIRECTORIES EXIST
ifeq ($(realpath $(OBJ_ROOT)),)
  $(error Can not find $$OBJ_ROOT)
endif

ifeq ($(realpath $(INC_DIR)),)
  $(error Can not find $$INC_DIR)
endif

ifeq ($(realpath $(LIB_DIR)),)
  $(error Can not find $$LIB_DIR)
endif

ifeq ($(realpath $(EXE_DIR)),)
  $(error Can not find $$EXE_DIR)
endif

ifeq ($(realpath $(SRC_DIR)),)
  $(error Can not find $$SRC_DIR)
endif

ifeq ($(realpath $(DRV_DIR)),)
  $(error Can not find $$DRV_DIR)
endif

ifeq ($(COMP_NAME),NONE)
  LIB_NAME = libmarbl.a
  LFLAG = -lmarbl
else
  INC_LOC = $(realpath $(INC_DIR)/$(COMP_NAME))
  ifeq ($(realpath $(INC_LOC)),)
    $(error Can not find $$INC_LOC)
  endif

  OBJ_LOC = $(realpath $(OBJ_ROOT)/$(COMP_NAME))
  ifeq ($(realpath $(OBJ_LOC)),)
    $(error Can not find $$OBJ_LOC)
  endif

  OBJ_DIR = $(realpath $(OBJ_LOC)/driver)
  ifeq ($(realpath $(OBJ_DIR)),)
    $(error Can not find $$OBJ_DIR)
  endif

  LIB_NAME = libmarbl-$(COMP_NAME).a
  LFLAG = -lmarbl-$(COMP_NAME)
endif
LIBCPPDEFS=-DECOSYS_NT=27 -DZOOPLANKTON_CNT=1 -DAUTOTROPH_CNT=3 -DGRAZER_PREY_CNT=3
EXECPPDEFS=
INCLUDES=-I$(INC_LOC)
LINKS=-L$(LIB_DIR) $(LFLAG)

EXE = $(EXE_DIR)/marbl.exe
ifeq ($(USEMPI),TRUE)
  EXECPPDEFS+= -DMARBL_TIMING_OPT=MPI
  LIBCPPDEFS+= -DMARBL_TIMING_OPT=MPI
  MPISUF=-mpi
  override FC = mpif90
  EXE = $(EXE_DIR)/marbl-mpi.exe
endif

COMP_ARGS = OBJ_DIR=$(OBJ_LOC) LIB_DIR=$(LIB_DIR) INC_DIR=$(INC_LOC) CPPDEFS="$(LIBCPPDEFS)"

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
	$(MAKE) $(EXE) COMP_NAME=pgi$(MPISUF) FC=pgf90 FCFLAGS="-O2 -Mfree" INC="-module $(OBJ_ROOT)/pgi$(MPISUF)" INC2="-module $(OBJ_ROOT)/pgi$(MPISUF)/driver"

gnu:
	$(MAKE) $(EXE) COMP_NAME=gnu$(MPISUF) FC=gfortran FCFLAGS="-O2 -ffree-form -cpp" INC="-J $(OBJ_ROOT)/gnu$(MPISUF)" INC2="-J $(OBJ_ROOT)/gnu$(MPISUF)/driver"

nag:
	$(MAKE) $(EXE) COMP_NAME=nag$(MPISUF) FC=nagfor FCFLAGS="-O2 -free -kind=byte" INC="-mdir $(OBJ_ROOT)/nag$(MPISUF) -I$(OBJ_ROOT)/nag$(MPISUF)" INC2="-mdir $(OBJ_ROOT)/nag$(MPISUF)/driver -I$(OBJ_ROOT)/nag$(MPISUF)/driver"

cray:
	$(MAKE) $(EXE) COMP_NAME=cray FC=ftn FCFLAGS="-O2 -f free -e mf" INC="-J $(OBJ_ROOT)/cray" INC2="-J $(OBJ_ROOT)/cray/driver"

clean_exe:
	rm -f $(EXE)

clean: clean_exe
	rm -f $(OBJ_ROOT)/{gnu,intel,nag,pgi,cray}{,-mpi}/driver/marbl_*.{o,mod} $(OBJ_ROOT)/{gnu,intel,nag,pgi,cray}{,-mpi}/driver/marbl.{o,mod}
	cd $(SRC_DIR) ; make clean
