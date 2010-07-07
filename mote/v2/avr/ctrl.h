//
// ctrl.h : AVR uC code for ctrl buffer initialisation and put/get ops
//
// Copyright (c) 2010 flukso.net
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

#ifndef CTRL_H
#define CTRL_H
#endif

#include <inttypes.h>
#include "buffer.h"

#ifndef CTRL_RX_BUFFER_SIZE
#define CTRL_RX_BUFFER_SIZE 16
#endif
#ifndef CTRL_TX_BUFFER_SIZE
#define CTRL_TX_BUFFER_SIZE 16
#endif

void ctrlInit(void);

uint8_t ctrlAddToRxBuffer(uint8_t data);

uint8_t ctrlGetFromTxBuffer(uint8_t* data);

uint8_t ctrlAddToTxBuffer(uint8_t data);

void ctrlLoop(void); 
