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
#define CTRL_RX_BUFFER_SIZE 32
#endif
#ifndef CTRL_TX_BUFFER_SIZE
#define CTRL_TX_BUFFER_SIZE 32
#endif

/**
 * Initialize the ctrl receive and transmit buffers.
 *
 * Overrule the default Rx and Tx ctrl buffer size (32 bytes) in the makefile.
 */
void ctrlInit(void);

/**
 * Check whether the ctrl Tx buffer is empty.
 *
 * @return	TRUE/FALSE if empty/not empty
 */
uint8_t ctrlTxBufferIsEmpty(void);

/**
 * Add a byte to the ctrl Tx buffer's tail.
 *
 * @param data 	the byte to be added to the buffer's tail
 * @return 	TRUE/FALSE if byte could/could not be written
 */
uint8_t ctrlAddToTxBuffer(uint8_t data);

/**
 * Fetch a byte from the ctrl Tx buffer's head.
 *
 * @param data	pointer where the byte has to be written
 * @return	TRUE/FALSE if a byte could be fetched/not fetched
 */
uint8_t ctrlGetFromTxBuffer(uint8_t* data);

/**
 * Check whether the ctrl Rx buffer is empty.
 *
 * @return	TRUE/FALSE if empty/not empty
 */
uint8_t ctrlRxBufferIsEmpty(void);

/**
 * Add a byte to the ctrl Rx buffer.
 *
 * @param data	the byte to be added to the buffer's tail
 * @return	TRUE/FALSE if empty/not empty
 */
uint8_t ctrlAddToRxBuffer(uint8_t data);

/**
 * Fetch a byte from the ctrl Rx buffer's head.
 *
 * @param data  pointer where the byte has to be written
 * @return      TRUE/FALSE if a byte could be fetched/not fetched
 */
uint8_t ctrlGetFromRxBuffer(uint8_t* data);

/**
 * Loop all bytes from the ctrl Rx to Tx buffer.
 *
 */
void ctrlRxToTxLoop(void); 
