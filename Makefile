########################################################################
####################### Makefile Template ##############################
########################################################################

# Compiler settings - Can be customized.
CC = gfortran
CXXFLAGS = -g -O0 -Wall
LDFLAGS = 

# Makefile settings - Can be customized.
UNITY_TESTS = Unity_tests
MODULAR_TESTS = Modular_tests
EXT = .F90
SRCDIR = src
TESTDIR = test
OBJDIR = obj

############## Do not change anything from here downwards! #############
SRC = $(wildcard $(SRCDIR)/*/*$(EXT))

OBJ = $(SRC:$(SRCDIR)/%$(EXT)=$(OBJDIR)/%.o)
MOD = *.mod
OBJTEST = $(TEST:$(TESTDIR)/%$(EXT)=$(TESTDIR)/%.o)

UNITY_TESTS_APP = ${OBJDIR}/test/${UNITY_TESTS}
MODULAR_TESTS_APP = ${OBJDIR}/test/${MODULAR_TESTS}

RM = rm -rf
########################################################################
####################### Targets beginning here #########################
########################################################################

all: $(UNITY_TESTS) $(MODULAR_TESTS)

# Builds the app Unity Tests
$(UNITY_TESTS): $(OBJ) 
	$(CC) $(CXXFLAGS) -o ${UNITY_TESTS_APP} ${TESTDIR}/${UNITY_TESTS}$(EXT) $^ $(LDFLAGS)


# Builds the app Modular Tests
$(MODULAR_TESTS): $(OBJ) 
	$(CC) $(CXXFLAGS) -o ${MODULAR_TESTS_APP} ${TESTDIR}/${MODULAR_TESTS}$(EXT) $^ $(LDFLAGS)


# Building rule for .o files
$(OBJDIR)/%.o: $(SRCDIR)/%$(EXT)
	@mkdir -p $(OBJDIR)/model
	@mkdir -p $(OBJDIR)/controller
	@mkdir -p $(OBJDIR)/test
	$(CC) $(CXXFLAGS) -o $@ -c $<


################### Cleaning rules for Unix-based OS ###################
# Cleans complete project
.PHONY: clean

clean:
	$(RM) $(OBJ) $(OBJTEST) $(MOD) ${MODULAR_TESTS_APP} ${UNITY_TESTS_APP}
