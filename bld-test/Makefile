SRC_DIR=../src
OBJ_DIR=../obj
LIB_DIR=../lib
INC_DIR=../include

COMP_ARGS=OBJ_DIR=$(OBJ_DIR) LIB_DIR=$(LIB_DIR) INC_DIR=$(INC_DIR) CPPDEFS="-DECOSYS_NT=27 -DZOOPLANKTON_CNT=1 -DAUTOTROPH_CNT=3 -DGRAZER_PREY_CNT=3"

all: intel

intel:
	cd $(SRC_DIR) ; make FC=ifort FCFLAGS="-O2 -free -module $(OBJ_DIR) -cpp -nogen-interface -fp-model source" $(COMP_ARGS) LIB_NAME=$(LIB_DIR)/libmarbl-intel.a

pgi:
	cd $(SRC_DIR) ; make FC=pgf90 FCFLAGS="-O2 -Mfree -module $(OBJ_DIR)" $(COMP_ARGS) LIB_NAME=$(LIB_DIR)/libmarbl-pgi.a

gnu:
	cd $(SRC_DIR) ; make FC=gfortran FCFLAGS="-O2 -ffree-form -J $(OBJ_DIR) -cpp" $(COMP_ARGS) LIB_NAME=$(LIB_DIR)/libmarbl-gnu.a

clean:
	rm -f $(OBJ_DIR)/* $(LIB_DIR)/* $(INC_DIR)/*
