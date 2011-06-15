# Copyright (c) 2011 Bart Van Der Meerssche

# Compiler flag to set the C Standard level.
#     c89   = "ANSI" C
#     gnu89 = c89 plus GCC extensions
#     c99   = ISO C99 standard (not yet fully implemented)
#     gnu99 = c99 plus GCC extensions
CSTANDARD = -std=gnu99

all: luad.o
ifeq ($(OPENWRT_BUILD),1)
	$(CC) $(LDFLAGS) $(CSTANDARD) -ldl -lm -lcrypt -ldaemon -llua luad.o -o luad
else
	$(CC) $(LDFLAGS) $(CSTANDARD) -ldl -lm -lcrypt -ldaemon -llua5.1 luad.o -o luad
endif

luad.o: luad.c
ifeq ($(OPENWRT_BUILD),1)
	$(CC) $(CFLAGS) $(CSTANDARD) -Wall -D OPENWRT_BUILD=1 -c luad.c
else
	$(CC) $(CFLAGS) $(CSTANDARD) -Wall -c luad.c
endif

clean:
	rm *.o luad
