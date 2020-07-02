.PHONY: all clean repl ide

default: all

PS_SRC = src
OUTPUT = output
PS_SOURCEFILES = $(shell find -L  ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find -L ${PS_SRC} -type f -name \*.erl)
COMPILED_PS = ../src/compiled_ps

all: $(COMPILED_PS)

$(COMPILED_PS): output
	mkdir -p $(COMPILED_PS)
	cp -r output/* $(COMPILED_PS)
	@touch $(COMPILED_PS)

output: $(PS_SOURCEFILES) $(PS_ERL_FFI) spago.dhall packages.dhall
	spago build
	@touch output

ide: $(PS_SOURCEFILES) $(PS_ERL_FFI) spago.dhall packages.dhall
	spago build -u '--json-errors'


clean:
	rm -rf $(OUTPUT)
