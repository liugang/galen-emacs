include ../../../.bootstrap

laddr: laddr.c
	$(CC) -o laddr laddr.c

load:
	- mkdir -p tmp
	cp -f laddr tmp
	- ecmd $(IP) download tmp/laddr
	- ecmd $(IP) red chmod +x /tmp/laddr
