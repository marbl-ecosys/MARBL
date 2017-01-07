.SUFFIXES: .F90 .o
extension = .F90

GPTL_DIR=NONE
USEMPI=FALSE

SRC_DIR=.
OBJ_DIR=../tests/obj
LIB_DIR=../lib
INC_DIR=../include
FC = NONE
GPTL_DIR = NONE
INCLUDES=

ifneq ($(INC_DIR),$(OBJ_DIR))
  DIFF_INC = TRUE
 endif

MODULES = $(shell ls *.F90)

# Some compilers produce ALL_UPPER_CASE.mod files
ifeq ($(UCASE),TRUE)
  MODS_TMP = $(shell echo $(MODULES) | tr '[a-z]' '[A-Z]')
else
  MODS_TMP = $(MODULES)
endif
ifneq ($(OBJ_DIR),$(INC_DIR))
  INCS = $(addprefix $(INC_DIR)/,${MODS_TMP:.F90=.mod})
endif
MODS = $(addprefix $(OBJ_DIR)/,${MODS_TMP:.F90=.mod}) \
       $(INCS)
OBJS = $(addprefix $(OBJ_DIR)/,${MODULES:.F90=.o})

CPPDEFS=-DECOSYS_NT=27 -DZOOPLANKTON_CNT=1 -DAUTOTROPH_CNT=3 -DGRAZER_PREY_CNT=3

ifneq ($(GPTL_DIR),NONE)
  CPPDEFS+= -DUSE_GPTL
  INCLUDES+= -I$(GPTL_DIR)/include
endif

ifeq ($(USEMPI),TRUE)
  CPPDEFS+= -DHAVE_MPI
  MPISUF=-mpi
  override FC = mpif90
endif

# Dependency Generation
MAKE_DEP = $(SRC_DIR)/makedep.py
DEP_FILE = $(OBJ_DIR)/shared_deps.d

ifeq ($(USE_DEPS),TRUE)
  include $(DEP_FILE)
endif

### TARGETS ###

all : gnu
.PHONY : all gnu intel pgi nag cray mv_mods depends clean

# Shorthand for specific compilers
gnu:
	$(MAKE) $(LIB_DIR)/libmarbl-gnu$(MPISUF).a USE_DEPS=TRUE FC=gfortran FCFLAGS="-O2 -ffree-form -J $(OBJ_DIR)/gnu$(MPISUF) -cpp" OBJ_DIR=$(OBJ_DIR)/gnu$(MPISUF) INC_DIR=$(INC_DIR)/gnu$(MPISUF)

intel:
	$(MAKE) $(LIB_DIR)/libmarbl-intel.a USE_DEPS=TRUE FC=ifort FCFLAGS="-O2 -free -module $(OBJ_DIR)/intel -cpp -nogen-interface -fp-model source" OBJ_DIR=$(OBJ_DIR)/intel INC_DIR=$(INC_DIR)/intel

pgi:
	$(MAKE) $(LIB_DIR)/libmarbl-pgi.a USE_DEPS=TRUE FC=pgf90 FCFLAGS="-O2 -Mfree -module $(OBJ_DIR)/pgi OBJ_DIR=$(OBJ_DIR)/pgi" INC_DIR=$(INC_DIR)/pgi

nag:
	$(MAKE) $(LIB_DIR)/libmarbl-nag$(MPISUF).a USE_DEPS=TRUE FC=nagfor FCFLAGS="-O2 -free -I $(OBJ_DIR)/nag$(MPISUF) -mdir $(OBJ_DIR)/nag$(MPISUF) -kind=byte" OBJ_DIR=$(OBJ_DIR)/nag$(MPISUF) INC_DIR=$(INC_DIR)/nag$(MPISUF)

cray:
	$(MAKE) $(LIB_DIR)/libmarbl-cray.a USE_DEPS=TRUE FC=ftn FCFLAGS="-O2 -f free -e mf -J $(OBJ_DIR)/cray" OBJ_DIR=$(OBJ_DIR)/cray INC_DIR=$(INC_DIR)/cray

# Remove library, object files, module files, and dependency file
# Create all object and module files

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.F90
	$(FC) $(CPPDEFS) $(FCFLAGS) $(INCLUDES) -c $< -o $@

# Note that .mod files need to be copied to INC_DIR if OBJ_DIR != INC_DIR
# Shorthand for moving files in $INC_DIR (if needed)
mv_mods: $(INCS)

$(INC_DIR)/%.mod: $(OBJ_DIR)/%.mod
	@$(if $(DIFF_INC), cp $< $@)

# Shorthand for making dependency file
depends: $(DEP_FILE)

$(DEP_FILE): $(MAKE_DEP) $(SRC_DIR)/*.F90
	@$(MAKE_DEP) $(DEP_FILE) $(OBJ_DIR) $(SRC_DIR); echo "Generated dependency file $(DEP_FILE)"

clean:
	/bin/rm -f $(OBJ_DIR)/*/*.* $(LIB_DIR)/* $(INC_DIR)/*/*

# Actual library build

%.a: depends $(OBJS)
	$(MAKE) mv_mods ; ar -ru $@ $(OBJS)

