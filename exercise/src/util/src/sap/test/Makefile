PROGRAM=testsdp

all:${PROGRAM}

VPATH=../

OBJECTS=sap.o sdp.o testsdp.o
                   
INCLUDES=-I../../../include

CFLAGS += -O0 -g -Wall -D_POSIX  -D_UNIX 

.c.o:
	${CC} $(CFLAGS) -c -o $@ $< ${INCLUDES}

${PROGRAM}:  $(OBJECTS) 
	$(CC) $(CFLAGS) -o ${PROGRAM}  ${OBJECTS} ${INCLUDES}

clean :
	rm -f $(OFILES) ${PROGRAM} *.o
