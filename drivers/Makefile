.SUFFIXES: .F90 .o

extension = .F90

LIB_DIR = ../lib
INC_DIR = ../include
OBJ_DIR = ../obj
SRC_DIR = ../src
DRV_DIR = .

COMP_ARGS = OBJ_DIR=$(OBJ_DIR) LIB_DIR=$(LIB_DIR) INC_DIR=$(INC_DIR) CPPDEFS="-DECOSYS_NT=27 -DZOOPLANKTON_CNT=1 -DAUTOTROPH_CNT=3 -DGRAZER_PREY_CNT=3"
EXE       = ../exe/marbl.exe

OBJS = marbl_init_namelist_drv.o       \
       marbl_init_no_namelist_drv.o    \
       marbl.o

.PHONY: intel pgi gnu nag cray clean

all: gnu

$(EXE): $(LIB_DIR)/libmarbl.a            \
        $(addprefix $(OBJ_DIR)/,$(OBJS))
	$(FC) -o $(EXE) $(OBJ_DIR)/*.o -L$(LIB_DIR) -lmarbl

$(OBJ_DIR)/%.o: $(DRV_DIR)/%.F90
	$(FC) $(FCFLAGS) -c $< -o $@

$(LIB_DIR)/libmarbl.a: $(SRC_DIR)/*.F90
	cd $(SRC_DIR) ; make FC=$(FC) FCFLAGS="$(FCFLAGS)" $(COMP_ARGS) LIBNAME=$@

intel:
	$(MAKE) $(EXE) FC=ifort FCFLAGS="-O2 -free -module $(OBJ_DIR) -cpp -nogen-interface -fp-model source"

pgi:
	$(MAKE) $(EXE) FC=pgf90 FCFLAGS="-O2 -Mfree -module $(OBJ_DIR)"

gnu:
	$(MAKE) $(EXE) FC=gfortran FCFLAGS="-O2 -ffree-form -J $(OBJ_DIR) -cpp"

nag:
	$(MAKE) $(EXE) FC=nagfor FCFLAGS="-O2 -free -I $(OBJ_DIR) -mdir $(OBJ_DIR) -kind=byte"

cray:
	$(MAKE) $(EXE) FC=ftn FCFLAGS="-O2 -f free -e mf -J $(OBJ_DIR)"

clean:
	rm -f $(OBJ_DIR)/* $(LIB_DIR)/* $(INC_DIR)/* $(EXE)
