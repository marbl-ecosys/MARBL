SRC_DIR=../src
OBJ_DIR=../obj
LIB_DIR=../lib
INC_DIR=../include

COMP_ARGS=OBJ_DIR=$(OBJ_DIR) LIB_DIR=$(LIB_DIR) INC_DIR=$(INC_DIR) CPPDEFS="-DECOSYS_NT=27 -DZOOPLANKTON_CNT=1 -DAUTOTROPH_CNT=3 -DGRAZER_PREY_CNT=3"

.PHONY: intel pgi gnu nag clean

all:gnu

intel:
	$(MAKE) $(LIB_DIR)/libmarbl-intel.a FC=ifort FCFLAGS="-O2 -free -module $(OBJ_DIR) -cpp -nogen-interface -fp-model source"

pgi:
	$(MAKE) $(LIB_DIR)/libmarbl-pgi.a FC=pgf90 FCFLAGS="-O2 -Mfree -module $(OBJ_DIR)"

gnu:
	$(MAKE) $(LIB_DIR)/libmarbl-gnu.a FC=gfortran FCFLAGS="-O2 -ffree-form -J $(OBJ_DIR) -cpp"

nag:
	$(MAKE) $(LIB_DIR)/libmarbl-nag.a FC=nagfor FCFLAGS="-O2 -free -I $(OBJ_DIR) -mdir $(OBJ_DIR) -kind=byte"

# Actual library build

$(LIB_DIR)/%.a: $(SRC_DIR)/*.F90
	cd $(SRC_DIR) ; make FC=$(FC) FCFLAGS="$(FCFLAGS)" $(COMP_ARGS) LIB_NAME=$@

# Clean up

clean:
	rm -f $(OBJ_DIR)/* $(LIB_DIR)/* $(INC_DIR)/*
