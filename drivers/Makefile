.SUFFIXES: .F90 .o

extension = .F90

LIB_DIR = ../lib
INC_DIR = ../include
OBJ_DIR = ../obj
SRC_DIR = ../src
DRV_DIR = .

OBJ_LOC = $(OBJ_DIR)
LIB_NAME = libmarbl.a
LFLAG = -lmarbl

COMP_ARGS = OBJ_DIR=$(OBJ_DIR) LIB_DIR=$(LIB_DIR) INC_DIR=$(INC_DIR) CPPDEFS="-DECOSYS_NT=27 -DZOOPLANKTON_CNT=1 -DAUTOTROPH_CNT=3 -DGRAZER_PREY_CNT=3"
EXE       = ../exe/marbl.exe

OBJS = marbl_init_namelist_drv.o       \
       marbl_init_no_namelist_drv.o    \
       marbl_get_put_drv.o             \
       marbl.o

.PHONY: intel pgi gnu nag cray clean clean_exe

all: gnu

$(EXE): $(LIB_DIR)/$(LIB_NAME)           \
        $(addprefix $(OBJ_LOC)/,$(OBJS)) \
        clean_exe
	$(FC) -o $(EXE) $(OBJ_LOC)/*.o -L$(LIB_DIR) $(LFLAG)

$(OBJ_LOC)/%.o: $(DRV_DIR)/%.F90
	$(FC) $(FCFLAGS) -c $< -o $@

$(LIB_DIR)/%.a: $(SRC_DIR)/*.F90
	cd $(SRC_DIR) ; make FC=$(FC) FCFLAGS="$(FCFLAGS)" $(COMP_ARGS) LIB_NAME=$@

intel:
	$(MAKE) $(EXE) FC=ifort FCFLAGS="-O2 -free -module $(OBJ_DIR)/intel -cpp -nogen-interface -fp-model source" LIB_NAME=libmarbl-intel.a LFLAG=-lmarbl-intel OBJ_LOC=$(OBJ_DIR)/intel

pgi:
	$(MAKE) $(EXE) FC=pgf90 FCFLAGS="-O2 -Mfree -module $(OBJ_DIR)/pgi" LIB_NAME=libmarbl-pgi.a LFLAG=-lmarbl-pgi OBJ_LOC=$(OBJ_DIR)/pgi

gnu:
	$(MAKE) $(EXE) FC=gfortran FCFLAGS="-O2 -ffree-form -J $(OBJ_DIR)/gnu -cpp" LIB_NAME=libmarbl-gnu.a LFLAG=-lmarbl-gnu OBJ_LOC=$(OBJ_DIR)/gnu

nag:
	$(MAKE) $(EXE) FC=nagfor FCFLAGS="-O2 -free -I $(OBJ_DIR)/nag -mdir $(OBJ_DIR)/nag -kind=byte" LIB_NAME=libmarbl-nag.a LFLAG=-lmarbl-nag OBJ_LOC=$(OBJ_DIR)/nag

cray:
	$(MAKE) $(EXE) FC=ftn FCFLAGS="-O2 -f free -e mf -J $(OBJ_DIR)/cray" LIB_NAME=libmarbl-cray.a LFLAG=-lmarbl-cray OBJ_LOC=$(OBJ_DIR)/cray

clean_exe:
	rm -f $(EXE)

clean: clean_exe
	rm -f $(OBJ_DIR)/*/* $(LIB_DIR)/* $(INC_DIR)/*/*
