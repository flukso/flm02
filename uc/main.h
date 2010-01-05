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
  #define TYPE 2301
#endif


#if TYPE == 2201   // 220V - 1-phase @ 488.28Hz sampling rate
  #define METERCONST 6783
  #define MUXN 0
  #warning "220V - 1-phase selected. METERCONST set to 6783"

#elif TYPE == 2203 // 220V - 3-phase @ 488.28Hz sampling rate
  #define METERCONST 6721
  #define MUXN 1
  #warning "220V - 3-phase selected. METERCONST set to 6721"

#elif TYPE == 2301   // 230V - 1-phase @ 488.28Hz sampling rate
  #define METERCONST 7091
  #define MUXN 0
  #warning "230V - 1-phase selected. METERCONST set to 7091"

#elif TYPE == 2303 // 230V - 3-phase @ 488.28Hz sampling rate
  #define METERCONST 7026
  #define MUXN 1
  #warning "230V - 3-phase selected. METERCONST set to 7026"

#elif TYPE == 2401 // 240V - 1-phase @ 488.28Hz sampling rate
  #define METERCONST 7399
  #define MUXN 0
  #warning "240V - 1-phase selected. METERCONST set to 7399"

#elif TYPE == 2403 // 240V - 3-phase @ 488.28Hz sampling rate
  #define METERCONST 7331
  #define MUXN 1
  #warning "240V - 3-phase selected. METERCONST set to 7331"
#endif

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
  uint16_t debug;
};

struct sensor {
  char id[33];
  uint32_t value;
};

// prototypes
void WDT_off(void);
void WDT_on(void);
void send(const struct sensor *measurement);

