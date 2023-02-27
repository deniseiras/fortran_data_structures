########################################################################
####################### Makefile Template ##############################
########################################################################

# Compiler settings - Can be customized.
CC = gfortran
CXXFLAGS = -g -O0 -Wall
LDFLAGS = 

# Makefile settings - Can be customized.
APPNAME = Unity_tests
EXT = .F90
SRCDIR = src
TESTDIR = test
OBJDIR = obj

############## Do not change anything from here downwards! #############
SRC = $(wildcard $(SRCDIR)/*/*$(EXT))

OBJ = $(SRC:$(SRCDIR)/%$(EXT)=$(OBJDIR)/%.o)
MOD = $(OBJ:$(OBJDIR)/%.o=%.mod)
OBJTEST = $(TEST:$(TESTDIR)/%$(EXT)=$(TESTDIR)/%.o)

# UNIX-based OS variables & settings
RM = rm -rf
DELOBJ = $(OBJ) $(OBJTEST)
APP = ${OBJDIR}/test/${APPNAME}


########################################################################
####################### Targets beginning here #########################
########################################################################

all: $(APPNAME)

# Builds the app
$(APPNAME): $(OBJ) 
	echo "building test app"
	$(CC) $(CXXFLAGS) -o ${APP} ${TESTDIR}/${APPNAME}$(EXT) $^ $(LDFLAGS)
#	$(RM) *.d *.mod


# Building rule for .o files and its .c/.cpp in combination with all .h
$(OBJDIR)/%.o: $(SRCDIR)/%$(EXT)
	echo "building model"
	@mkdir -p $(OBJDIR)/model
	@mkdir -p $(OBJDIR)/controller
	$(CC) $(CXXFLAGS) -o $@ -c $<


################### Cleaning rules for Unix-based OS ###################
# Cleans complete project
.PHONY: clean

clean:
	echo "Cleaning ..."
	$(RM) $(DELOBJ) $(MOD) ${APP} 
