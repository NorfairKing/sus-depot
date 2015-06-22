NAME = xmonad
SRC = xmonad.hs
BIN = $(NAME)

GHC = ghc
GHC_OPTIMISATION 	= 	-O2
GHC_FLAGS 			= 	-Wall \
						-fno-warn-missing-signatures \
						-fwarn-unused-imports \
						-fforce-recomp
GHC_SRC_DIRS 		=  -i$(SRC_DIR)
GHC_OPTIONS 		= 	-threaded \
						$(GHC_OPTIMISATION) \
						$(GHC_FLAGS) \
						$(GHC_SRC_DIRS)


SRC_DIR = lib

all: bin
	
bin:
	$(GHC) $(GHC_OPTIONS) -o $(BIN) --make $(SRC)

love:
	@echo "not war"
	
DIRTY_EXT = *.o *.hi *.bin

clean:
	rm -f $(BIN) $(DIRTY_EXT)
