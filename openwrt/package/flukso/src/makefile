INCDIR = -I/usr/include/lua5.1
LIBDIR = -L/usr/lib
LUALIB = lua5.1

ifeq ($(OPENWRT_BUILD),1)
INCDIR =
LIBDIR =
LUALIB = lua
endif

EXE = luad
OBJS = luad.o
LIBS = -ldl -lm -lcrypt -ldaemon -l$(LUALIB)
CSTD = -std=gnu99
WARN = -Wall -pedantic

CFLAGS += $(CSTD) $(WARN) $(INCDIR)
LDFLAGS += $(CSTD) $(LIBDIR)

$(EXE): $(OBJS)
	$(CC) $(LDFLAGS) $(LIBS) $(OBJS) -o $@

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<

clean:
	rm -f $(OBJS) $(EXE)
