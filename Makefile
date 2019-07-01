.PHONY: all server client

all: server 

server: 
	$(MAKE) -C server all

client: 
	$(MAKE) -C client all
