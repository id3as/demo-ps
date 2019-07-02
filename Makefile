.PHONY: all server client

all: server  client

server: 
	$(MAKE) -C server all

client: 
	$(MAKE) -C client all
