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


  #elif TYPE == 2201001
    #define METERCONST 10598
    #warning "220V - 100A - 1-phase selected. METERCONST set to 10598"

  #elif TYPE == 2201003
    #define METERCONST 10502
    #warning "220V - 100A - 3-phase selected. METERCONST set to 10502"

  #elif TYPE == 2301001
    #define METERCONST 11080
    #warning "230V - 100A - 1-phase selected. METERCONST set to 11080"

  #elif TYPE == 2301003
    #define METERCONST 10978
    #warning "230V - 100A - 3-phase selected. METERCONST set to 10978"

  #elif TYPE == 2401001
    #define METERCONST 11560
    #warning "240V - 100A - 1-phase selected. METERCONST set to 11560"

  #elif TYPE == 2401003
    #define METERCONST 11454
    #warning "240V - 100A - 3-phase selected. METERCONST set to 11454"


  #elif TYPE == 2202501
    #define METERCONST 26495
    #warning "220V - 250A - 1-phase selected. METERCONST set to 26495"

  #elif TYPE == 2202503
    #define METERCONST 26255
    #warning "220V - 250A - 3-phase selected. METERCONST set to 26255"

  #elif TYPE == 2302501
    #define METERCONST 27700
    #warning "230V - 250A - 1-phase selected. METERCONST set to 27700"

  #elif TYPE == 2302503
    #define METERCONST 27445
    #warning "230V - 250A - 3-phase selected. METERCONST set to 27445"

  #elif TYPE == 2402501
    #define METERCONST 28900
    #warning "240V - 250A - 1-phase selected. METERCONST set to 28900"

  #elif TYPE == 2402503
    #define METERCONST 28635
    #warning "240V - 250A - 3-phase selected. METERCONST set to 28635"


  #elif TYPE == 2205001
    #define METERCONST 52990
    #warning "220V - 500A - 1-phase selected. METERCONST set to 52990"

  #elif TYPE == 2205003
    #define METERCONST 52510
    #warning "220V - 500A - 3-phase selected. METERCONST set to 52510"

  #elif TYPE == 2305001
    #define METERCONST 55400
    #warning "230V - 500A - 1-phase selected. METERCONST set to 55400"

  #elif TYPE == 2305003
    #define METERCONST 54890
    #warning "230V - 500A - 3-phase selected. METERCONST set to 54890"

  #elif TYPE == 2405001
    #define METERCONST 57800
    #warning "240V - 500A - 1-phase selected. METERCONST set to 57800"

  #elif TYPE == 2405003
    #define METERCONST 57270
    #warning "240V - 500A - 3-phase selected. METERCONST set to 57270"
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
