.PHONY: all clean

PS_SRC = src
OUTPUT = output

all: bundle.js

bundle.js: output
	spago bundle-app -t bundle.js -m 'BookClient.Client'
	cp bundle.js ../priv/www/assets/

output: $(PS_SRC)/*.purs $(PS_SRC)/**/*.purs spago.dhall packages.dhall
	spago build
	touch output

ide: $(PS_SRC)/*.purs $(PS_SRC)/**/*.purs spago.dhall packages.dhall
	spago build -u '--json-errors'
	touch output

clean:
	rm -rf $(OUTPUT)/*

