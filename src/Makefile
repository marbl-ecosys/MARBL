.SUFFIXES: .F90 .o
extension = .F90

USEMPI=FALSE

###############################################
# DEFINE VARIOUS DIRECTORIES NEEDED FOR BUILD #
###############################################

# (1) The default directories are all defined relative to the location of this
#     Makefile

# Not all versions of gmake use MAKEFILE_LIST; if it exists then we determine
# the directory that contains the Makefile otherwise we assume Makefile is in
# the current working directory

# $(word n,VAR) looks for the nth word in $(VAR)
# so ThisMakefile is the first word in $(MAKEFILE_LIST) (which might be empty)
override ThisMakefile=$(word 1,$(MAKEFILE_LIST))

# if there are any / in $(ThisMakefile), strip everything after the last /
ifeq (/,$(findstring /,$(ThisMakefile)))
  override MAKE_DIR=$(realpath $(shell ThisMakefile=$(ThisMakefile) && echo $${ThisMakefile%/*}))
else
  override MAKE_DIR=$(realpath .)
endif

# (2) Default directories (can be changed by invoking "make SRC_DIR=...", etc)
SRC_DIR=$(realpath $(MAKE_DIR)/.)
OBJ_DIR=$(realpath $(MAKE_DIR)/../tests/obj)
INC_DIR=$(realpath $(MAKE_DIR)/../include)
LIB_DIR=$(realpath $(MAKE_DIR)/../lib)

###################################################
# ERROR CHECKING: MAKE SURE ALL DIRECTORIES EXIST #
###################################################

ifeq ($(realpath $(OBJ_DIR)),)
  $(error Can not find $$OBJ_DIR)
endif

ifeq ($(realpath $(INC_DIR)),)
  $(error Can not find $$INC_DIR)
endif

ifeq ($(realpath $(LIB_DIR)),)
  $(error Can not find $$LIB_DIR)
endif

ifeq ($(realpath $(SRC_DIR)),)
  $(error Can not find $$SRC_DIR)
endif

##############################################
# DEFAULT VALUES FOR INTERNAL MAKE VARIABLES #
##############################################

FC = NONE

ifneq ($(INC_DIR),$(OBJ_DIR))
  DIFF_INC = TRUE
 endif

MODULES = $(shell cd $(SRC_DIR); ls *.F90)

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

CPPDEFS=

ifeq ($(USEMPI),TRUE)
  CPPDEFS+= -DMARBL_TIMING_OPT=MPI
  MPISUF=-mpi
  override FC = mpif90
endif

#########################
# DEPENDENCY GENERATION #
#########################

MAKE_DEP = $(SRC_DIR)/makedep.py
DEP_FILENAME = shared_deps.d
DEP_FILE = $(OBJ_DIR)/$(DEP_FILENAME)

ifeq ($(USE_DEPS),TRUE)
  include $(DEP_FILE)
endif

###########
# TARGETS #
###########

# default is to build with gfortran
.PHONY: all
all : gnu

# Shorthand for specific compilers (gnu, intel, pgi, nag, cray)

.PHONY: gnu
gnu:
	$(MAKE) -f $(MAKE_DIR)/Makefile $(LIB_DIR)/libmarbl-gnu$(MPISUF).a USE_DEPS=TRUE FC=gfortran FCFLAGS="-Wall -Wextra -Wno-compare-reals -Werror -O2 -ffree-form -J $(OBJ_DIR)/gnu$(MPISUF) -cpp" OBJ_DIR=$(OBJ_DIR)/gnu$(MPISUF) INC_DIR=$(INC_DIR)/gnu$(MPISUF)

.PHONY: intel
intel:
	$(MAKE) -f $(MAKE_DIR)/Makefile $(LIB_DIR)/libmarbl-intel$(MPISUF).a USE_DEPS=TRUE FC=ifort FCFLAGS="-O2 -free -module $(OBJ_DIR)/intel$(MPISUF) -cpp -nogen-interface -fp-model source" OBJ_DIR=$(OBJ_DIR)/intel$(MPISUF) INC_DIR=$(INC_DIR)/intel$(MPISUF)

.PHONY: pgi
pgi:
	$(MAKE) -f $(MAKE_DIR)/Makefile $(LIB_DIR)/libmarbl-pgi$(MPISUF).a USE_DEPS=TRUE FC=pgf90 FCFLAGS="-O2 -Mfree -module $(OBJ_DIR)/pgi$(MPISUF)" OBJ_DIR=$(OBJ_DIR)/pgi$(MPISUF) INC_DIR=$(INC_DIR)/pgi$(MPISUF)

.PHONY: nag
nag:
	$(MAKE) -f $(MAKE_DIR)/Makefile $(LIB_DIR)/libmarbl-nag$(MPISUF).a USE_DEPS=TRUE FC=nagfor FCFLAGS="-colour -O2 -free -I $(OBJ_DIR)/nag$(MPISUF) -mdir $(OBJ_DIR)/nag$(MPISUF)" OBJ_DIR=$(OBJ_DIR)/nag$(MPISUF) INC_DIR=$(INC_DIR)/nag$(MPISUF)

.PHONY: cray
cray:
	$(MAKE) -f $(MAKE_DIR)/Makefile $(LIB_DIR)/libmarbl-cray.a USE_DEPS=TRUE FC=ftn FCFLAGS="-O2 -f free -e mf -J $(OBJ_DIR)/cray" OBJ_DIR=$(OBJ_DIR)/cray INC_DIR=$(INC_DIR)/cray

# Create all object and module files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.F90
	$(FC) $(CPPDEFS) $(FCFLAGS) $(INCLUDES) -c $< -o $@

# Actual library build
%.a: depends $(OBJS)
	$(MAKE) -f $(MAKE_DIR)/Makefile cp_mods ; ar -ru $@ $(OBJS)

# Shorthand for copying files to INC_DIR, if needed
.PHONY: cp_mods
cp_mods: $(INCS)

# .mod files need to be copied to INC_DIR if OBJ_DIR != INC_DIR
$(INC_DIR)/%.mod: $(OBJ_DIR)/%.mod
	@$(if $(DIFF_INC), cp $< $@)

# Shorthand for making dependency file
.PHONY: depends
depends: $(DEP_FILE)

$(DEP_FILE): $(MAKE_DEP) $(SRC_DIR)/*.F90
	@$(MAKE_DEP) $(DEP_FILE) $(OBJ_DIR) $(SRC_DIR); echo "Generated dependency file $(DEP_FILE)"

# clean up previous builds
.PHONY: clean
clean:
	rm -f $(OBJ_DIR)/*/$(DEP_FILENAME) $(LIB_DIR)/libmarbl*.a
	find $(OBJ_DIR) -maxdepth 2 -name marbl_*.o | xargs rm -f
	find $(OBJ_DIR) -maxdepth 2 -name marbl_*.mod | xargs rm -f
