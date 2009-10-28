//
// main.1mhz.h : AVR uC header file for flukso sensor board
// Copyright (c) 2008-2009 jokamajo.org
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

#define SENSOR0 "0123456789abcdef0123456789abcde0"
#define SENSOR1 "0123456789abcdef0123456789abcde1"
#define SENSOR2 "0123456789abcdef0123456789abcde2"
#define SENSOR3 "0123456789abcdef0123456789abcde3"

#define TYPE 2301

#if TYPE == 2301   // 230V - 1-phase @ 488.28Hz sampling rate
  #define METERCONST 7091
  #warning "230V - 1-phase selected. METERCONST set to 7091"

#elif TYPE == 2303 // 230V - 3-phase @ 488.28Hz sampling rate
  #define METERCONST 7026
  #warning "230V - 3-phase selected. METERCONST set to 7026"

#elif TYPE == 2401 // 240V - 1-phase @ 488.28Hz sampling rate
  #define METERCONST 7399
  #warning "240V - 1-phase selected. METERCONST set to 2401"

#elif TYPE == 2403 // 240V - 3-phase @ 488.28Hz sampling rate
  #define METERCONST 7331
  #warning "240V - 3-phase selected. METERCONST set to 7331"
#endif

#define START 0
#define END3 0xffffffff
#define END2 0xeeeeeeee
#define END1 0xdddddddd
#define END0 0xcccccccc

// datastructures
uint8_t i;

struct state {
  boolean pulse;
  boolean toggle;
  uint32_t nano;
};

struct sensor {
  char id[33];
  uint32_t value;
};

// prototypes
void WDT_off(void);
void WDT_on(void);
void send(const struct sensor *measurement);

