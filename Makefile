.PHONY: all server client clean

all: server  client

server: 
	$(MAKE) -C server all

client: 
	$(MAKE) -C client all

clean:
	rm -rf src/compiled_ps
	rm -rf server/output
	rm -rf _build/default/lib/demo_ps/ebin/*
