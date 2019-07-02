.PHONY: all clean

PS_SRC = src
OUTPUT = output

all: bundle.js

bundle.js: output
	purs bundle 'output/**/*.js' -o bundle.js -m 'BookClient.Client' --main 'BookClient.Client'
	cp bundle.js ../priv/www/assets/

#$(PS_SRC)/**/*.purs 

output: $(PS_SRC)/*.purs $(PS_SRC)/**/*.purs .psc-package
	psc-package sources | xargs purs compile --codegen js  '$(PS_SRC)/**/*.purs' 
	touch output

.psc-package: psc-package.json
	psc-package install
	touch .psc-package

clean:
	rm -rf $(OUTPUT)/*

