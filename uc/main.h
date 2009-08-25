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

#define SENSOR0 "0123456789abcdef0123456789abcdef"
#define SENSOR1 "0123456789abcdef0123456789abcdef"
#define SENSOR2 "0123456789abcdef0123456789abcdef"
#define SENSOR3 "0123456789abcdef0123456789abcdef"

#define METERCONST 45205 // 29165 * 1.55

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

