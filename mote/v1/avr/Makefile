# Arduino 0011 Makefile
# Arduino adaptation by mellis, eighthave, oli.keller
#
# This makefile allows you to build sketches from the command line
# without the Arduino environment (or Java).
#
# Detailed instructions for using the makefile:
#
#  1. Copy this file into the folder with your sketch. There should be a
#     file with the extension .pde (e.g. foo.pde)
#
#  2. Below, modify the line containing "TARGET" to refer to the name of
#     of your program's file without an extension (e.g. TARGET = foo).
#
#  3. Modify the line containg "INSTALL_DIR" to point to the directory that
#     contains the Arduino installation (for example, under Mac OS X, this
#     might be /Applications/arduino-0011).
#
#  4. Modify the line containing "PORT" to refer to the filename
#     representing the USB or serial connection to your Arduino board
#     (e.g. PORT = /dev/tty.USB0).  If the exact name of this file
#     changes, you can use * as a wildcard (e.g. PORT = /dev/tty.USB*).
#
#  5. Set the line containing "MCU" to match your board's processor. 
#     Older one's are atmega8 based, newer ones like Arduino Mini, Bluetooth
#     or Diecimila have the atmega168.  If you're using a LilyPad Arduino,
#     change F_CPU to 8000000.
#
#  6. At the command line, change to the directory containing your
#     program's file and the makefile.
#
#  7. Type "make" and press enter to compile/verify your program.
#
#  8. Type "make upload", reset your Arduino board, and press enter to
#     upload your program to the Arduino board.
#
# $Id$

###############################################################################
#
#      project specific settings:
#
TARGET = main
#
# Predefine the TYPE and SENSORx C macros in main.h via this Makefile.
# Override the defaults on the command line by typing:
# make PHASE=x METERCONST=y SENSOR0=z ...
#
DBG = 0
#
TYPE = 2300501
PHASE = 1
METERCONST= 5508
#
SENSOR0 = 0123456789abcdef0123456789abcde0
SENSOR1 = 0123456789abcdef0123456789abcde1
SENSOR2 = 0123456789abcdef0123456789abcde2
SENSOR3 = 0123456789abcdef0123456789abcde3
#
CEXTRA = -D DBG=$(DBG) -D PHASE=$(PHASE) -D METERCONST=$(METERCONST) -D 'SENSOR0="$(SENSOR0)"' -D 'SENSOR1="$(SENSOR1)"' -D 'SENSOR2="$(SENSOR2)"' -D 'SENSOR3="$(SENSOR3)"'
###############################################################################


###############################################################################
#
#      serial uploader settings
#
PORT			= /dev/ttyUSB*
UPLOAD_RATE		= 19200
AVRDUDE_PROGRAMMER	= usbtiny
#
# HINT: If you want to use the automatic reset feature which comes with Arduino 
# Diecimila, put the follwoing in your avrdude.conf: 
# (Use the systemwide $(INSTALL_DIR)/tools/avr/etc/avrdude.conf or create a  
#  local $HOME/.avrduderc file)
#
#  programmer
#     id    = "arduino";
#     desc  = "Arduino Serial Bootloader";
#     type  =  stk500;
#     reset = 7;
#     # since Arduino Diecimila the serial DTR line can be used to trigger a reset!
#  ;
#
# After this you can specify AVRDUDE_PROGRAMMER = arduino, above.
# On older boards you can manually ad this reset feature. Wire a cable from the
# FTDI 232 Chip's DTR pin (the number differs from package to package, see datasheet) 
# to the RESET line of the ATmega. Inbetween this connection must be a 100nF capacitor.
#####################################################################################


#####################################################################################
#
#      hardware dependent settings
#
MCU	= atmega48
F_CPU	= 1000000
#####################################################################################

#####################################################################################
#
#      Below here nothing should be changed...
#
#####################################################################################

AVR_TOOLS_PATH = /usr/bin
SRC = wiring/serial.c
FORMAT = ihex

# Name of this Makefile (used for "make depend").
MAKEFILE = Makefile

# Debugging format.
# Native formats for AVR-GCC's -g are stabs [default], or dwarf-2.
# AVR (extended) COFF requires stabs, plus an avr-objcopy run.
DEBUG = stabs

OPT = s

# Place -D or -U options here
CDEFS = -DF_CPU=$(F_CPU)
CXXDEFS = -DF_CPU=$(F_CPU)

# Compiler flag to set the C Standard level.
# c89   - "ANSI" C
# gnu89 - c89 plus GCC extensions
# c99   - ISO C99 standard (not yet fully implemented)
# gnu99 - c99 plus GCC extensions
CSTANDARD = -std=gnu99
CDEBUG = -g$(DEBUG)
CWARN = -Wall -Wstrict-prototypes
CTUNING = -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums
#CEXTRA = -Wa,-adhlns=$(<:.c=.lst)

CFLAGS = $(CDEBUG) $(CDEFS) $(CINCS) -O$(OPT) $(CWARN) $(CSTANDARD) $(CEXTRA)
CXXFLAGS = $(CDEFS) $(CINCS) -O$(OPT)
#ASFLAGS = -Wa,-adhlns=$(<:.S=.lst),-gstabs 
LDFLAGS = -lm


# Programming support using avrdude. Settings and variables.
AVRDUDE_PORT = $(PORT)
AVRDUDE_WRITE_FUSES= -U lfuse:w:0x6E:m -U hfuse:w:0xD6:m -U efuse:w:0x1:m
AVRDUDE_WRITE_FLASH = -U flash:w:bin/$(TARGET).hex
AVRDUDE_WRITE_EEPROM = -U eeprom:w:bin/$(TARGET).eep
AVRDUDE_FLAGS = -p $(MCU) -c $(AVRDUDE_PROGRAMMER) -b $(UPLOAD_RATE)

# Program settings
CC = $(AVR_TOOLS_PATH)/avr-gcc
CXX = $(AVR_TOOLS_PATH)/avr-g++
OBJCOPY = $(AVR_TOOLS_PATH)/avr-objcopy
OBJDUMP = $(AVR_TOOLS_PATH)/avr-objdump
AR  = $(AVR_TOOLS_PATH)/avr-ar
SIZE = $(AVR_TOOLS_PATH)/avr-size
NM = $(AVR_TOOLS_PATH)/avr-nm
AVRDUDE = avrdude
REMOVE = rm -f
MV = mv -f

# Define all object files.
OBJ = $(SRC:.c=.o) $(CXXSRC:.cpp=.o) $(ASRC:.S=.o) 

# Define all listing files.
LST = $(ASRC:.S=.lst) $(CXXSRC:.cpp=.lst) $(SRC:.c=.lst)

# Combine all necessary flags and optional flags.
# Add target processor to flags.
ALL_CFLAGS = -mmcu=$(MCU) -I. $(CFLAGS)
ALL_CXXFLAGS = -mmcu=$(MCU) -I. $(CXXFLAGS)
ALL_ASFLAGS = -mmcu=$(MCU) -I. -x assembler-with-cpp $(ASFLAGS)


# Default target.
all: bin_files build sizeafter

build: elf hex eep

bin_files: $(TARGET).c
	@test -d bin || mkdir bin
	@cp $(TARGET).c  bin/$(TARGET).c

elf: bin/$(TARGET).elf
hex: bin/$(TARGET).hex
eep: bin/$(TARGET).eep
lss: bin/$(TARGET).lss 
sym: bin/$(TARGET).sym

# Program the device.
upload: bin/$(TARGET).hex
	@$(AVRDUDE) $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_FUSES) $(AVRDUDE_WRITE_FLASH) $(AVRDUDE_WRITE_EEPROM)

# Display size of file.
HEXSIZE = $(SIZE) --target=$(FORMAT) bin/$(TARGET).hex
ELFSIZE = $(SIZE)  bin/$(TARGET).elf
sizebefore:
	@if [ -f bin/$(TARGET).elf ]; then echo; echo $(MSG_SIZE_BEFORE); $(HEXSIZE); echo; fi

sizeafter:
	@if [ -f bin/$(TARGET).elf ]; then echo; echo $(MSG_SIZE_AFTER); $(HEXSIZE); echo; fi


# Convert ELF to COFF for use in debugging / simulating in AVR Studio or VMLAB.
COFFCONVERT=$(OBJCOPY) --debugging \
--change-section-address .data-0x800000 \
--change-section-address .bss-0x800000 \
--change-section-address .noinit-0x800000 \
--change-section-address .eeprom-0x810000 


coff: bin/$(TARGET).elf
	$(COFFCONVERT) -O coff-avr bin/$(TARGET).elf $(TARGET).cof


extcoff: $(TARGET).elf
	$(COFFCONVERT) -O coff-ext-avr bin/$(TARGET).elf $(TARGET).cof

.SUFFIXES: .elf .hex .eep .lss .sym

.elf.hex:
	@$(OBJCOPY) -O $(FORMAT) -R .eeprom $< $@

.elf.eep:
	@$(OBJCOPY) -j .eeprom --set-section-flags=.eeprom="alloc,load" \
	--change-section-lma .eeprom=0 -O $(FORMAT) $< $@

# Create extended listing file from ELF output file.
.elf.lss:
	$(OBJDUMP) -h -S $< > $@

# Create a symbol table from ELF output file.
.elf.sym:
	$(NM) -n $< > $@

	# Link: create ELF output file from library.
bin/$(TARGET).elf: $(TARGET).c bin/core.a 
	@$(CC) $(ALL_CFLAGS) -o $@ bin/$(TARGET).c -L. bin/core.a $(LDFLAGS)

bin/core.a: $(OBJ)
	@for i in $(OBJ); do $(AR) rcs bin/core.a $$i; done

# Compile: create object files from C++ source files.
.cpp.o:
	@$(CXX) -c $(ALL_CXXFLAGS) $< -o $@ 

# Compile: create object files from C source files.
.c.o:
	@$(CC) -c $(ALL_CFLAGS) $< -o $@ 


# Compile: create assembler files from C source files.
.c.s:
	$(CC) -S $(ALL_CFLAGS) $< -o $@


# Assemble: create object files from assembler source files.
.S.o:
	$(CC) -c $(ALL_ASFLAGS) $< -o $@



# Target: clean project.
clean:
	@$(REMOVE) bin/$(TARGET).hex bin/$(TARGET).eep bin/$(TARGET).cof bin/$(TARGET).elf \
	bin/$(TARGET).map bin/$(TARGET).sym bin/$(TARGET).lss bin/$(TARGET).c bin/core.a \
	$(OBJ) $(LST) $(SRC:.c=.s) $(SRC:.c=.d) $(CXXSRC:.cpp=.s) $(CXXSRC:.cpp=.d)

depend:
	if grep '^# DO NOT DELETE' $(MAKEFILE) >/dev/null; \
	then \
		sed -e '/^# DO NOT DELETE/,$$d' $(MAKEFILE) > \
			$(MAKEFILE).$$$$ && \
		$(MV) $(MAKEFILE).$$$$ $(MAKEFILE); \
	fi
	echo '# DO NOT DELETE THIS LINE -- make depend depends on it.' \
		>> $(MAKEFILE); \
	$(CC) -M -mmcu=$(MCU) $(CDEFS) $(CINCS) $(SRC) $(ASRC) >> $(MAKEFILE)

.PHONY:	all build elf hex eep lss sym program coff extcoff clean depend bin_files sizebefore sizeafter
