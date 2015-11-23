PROJECT_NAME = Hash
TOP_LEVEL_DIR = $(shell pwd)
SRC = src
INC = inc
BUILD = build
FLAGS = -threaded

all:
	cd $(SRC)/;\
	ghc $(FLAGS) $(PROJECT_NAME).hs -hidir $(TOP_LEVEL_DIR)/$(BUILD) -odir $(TOP_LEVEL_DIR)/$(BUILD)
	mv $(SRC)/$(PROJECT_NAME) $(PROJECT_NAME)

clean:
	-rm $(PROJECT_NAME) $(BUILD)/*
