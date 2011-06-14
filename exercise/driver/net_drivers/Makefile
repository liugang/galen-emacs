obj-m := net_drivers.o
# KDIR := /lib/modules/$(shell uname -r)/build
# PWD := $(shell pwd)
# default:
# 	$(MAKE) -C $(KDIR) SUBDIRS=$(PWD) modules
all:
	$(MAKE) -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

clean:
	$(MAKE) -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean
