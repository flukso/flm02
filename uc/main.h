//
// main.h : AVR uC header file for flukso sensor board
// Copyright (c) 2008-2009 jokamajo.org
// Copyright (c) 2010      flukso.net
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//
// $Id$
//

# define PULSE 0
# define POWER 1

# define WATT 1000000000
# define SECOND 624 // 625Hz - 1

#ifndef SENSOR0 
  #define SENSOR0 "0123456789abcdef0123456789abcde0"
#endif

#ifndef SENSOR1
  #define SENSOR1 "0123456789abcdef0123456789abcde1"
#endif

#ifndef SENSOR2
  #define SENSOR2 "0123456789abcdef0123456789abcde2"
#endif

#ifndef SENSOR3
  #define SENSOR3 "0123456789abcdef0123456789abcde3"
#endif

#ifndef TYPE
  #define TYPE 2300501
#endif

#ifndef METERCONST // @ 625Hz sampling rate
  #if TYPE == 2200501
    #define METERCONST 5299
    #warning "220V - 50A - 1-phase selected. METERCONST set to 5299"

  #elif TYPE == 2200503
    #define METERCONST 5251
    #warning "220V - 50A - 3-phase selected. METERCONST set to 5251"

  #elif TYPE == 2300501
    #define METERCONST 5540
    #warning "230V - 50A - 1-phase selected. METERCONST set to 5540"

  #elif TYPE == 2300503
    #define METERCONST 5489
    #warning "230V - 50A - 3-phase selected. METERCONST set to 5489"

  #elif TYPE == 2400501
    #define METERCONST 5780
    #warning "240V - 50A - 1-phase selected. METERCONST set to 5780"

  #elif TYPE == 2400503
    #define METERCONST 5727
    #warning "240V - 50A - 3-phase selected. METERCONST set to 5727"
  #endif
#endif

//#define POWERCONST (METERCONST*1758)/1000 // in mW

#define START 0
#define END3 0xffffffff
#define END2 0xeeeeeeee
#define END1 0xdddddddd
#define END0 0xcccccccc


// This macro performs a 16x16 -> 32 unsigned MAC in 37 cycles with operands and results in memory
// based on http://www2.ife.ee.ethz.ch/~roggend/publications/wear/DSPMic_v1.1.pdf par 3.4 and table 31.
#define MacU16X16to32(uint_32Acc, uint_16In1, uint_16In2) \
asm volatile ( \
  "clr r2 \n\t" \
  "mul %B2, %B1 \n\t" \
  "movw r4, r0 \n\t" \
  "mul %A2, %A1 \n\t" \
  "add %A0, r0 \n\t" \
  "adc %B0, r1 \n\t" \
  "adc %C0, r4 \n\t" \
  "adc %D0, r5 \n\t" \
  "mul %B2, %A1 \n\t" \
  "add %B0, r0 \n\t" \
  "adc %C0, r1 \n\t" \
  "adc %D0, r2 \n\t" \
  "mul %A2, %B1 \n\t" \
  "add %B0, r0 \n\t" \
  "adc %C0, r1 \n\t" \
  "adc %D0, r2 \n\t" \
  "clr r1 \n\t" \
  : \
  "+r" (uint_32Acc) \
  : \
  "a" (uint_16In1), \
  "a" (uint_16In2) \
  : \
  "r2", "r4", "r5" \
)

// datastructures
struct state {
  boolean pulse;
  boolean toggle;
  uint32_t nano;
  uint16_t adc;

  boolean  power;
  uint32_t nano_start;
  uint32_t nano_end;
  uint8_t  pulse_count;
  uint8_t  pulse_count_final;
};

struct sensor {
  char id[33];
  uint32_t value;
};

// prototypes
void WDT_off(void);
void WDT_on(void);
void send(uint8_t msg_type, const struct sensor *measurement, const struct state *aux);
