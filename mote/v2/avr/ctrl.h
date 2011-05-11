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
 * Flush the ctrl Rx buffer.
 *
 */
void ctrlFlushRxBuffer(void);

/**
 * Flush the ctrl Tx buffer.
 *
 */
void ctrlFlushTxBuffer(void);

/**
 * Loop all bytes from the ctrl Rx to Tx buffer.
 *
 */
void ctrlRxToTxLoop(void);

/**
 * Calculate the CRC-8 checksum over the bytes in the buffer.
 *
 * @param buffer  pointer to the buffer containing the data
 * @param chop    chop number of bytes from end of buffer for crc calc
 * @return        CRC-8 checksum
 */
uint8_t ctrlCalcCrc8(cBuffer* buffer, uint8_t chop);


/**
 * Extract the CRC-8 checksum out of the message in the buffer.
 *
 * @param buffer  pointer to the buffer containing the message
 * @return        CRC-8 checksum
 */
uint8_t ctrlExtractCrc8fromMessage(cBuffer* buffer);

/**
 * Decode the message in the ctrl Rx buffer and dispatch to either ctrlCmdGet,
 * ctrlCmdSet or ctrlCmdCommit.
 *
 */
void ctrlDecode(void);

/**
 * Execute the get command with parameters present in the ctrl Rx buffer.
 * The command's reply is written to the ctrl Tx buffer.
 *
 * @param cmd  get command issued
 */
void ctrlCmdGet(uint8_t cmd);

/**
 * Execute the set command with parameters present in the ctrl Rx buffer.
 * The command's reply is written to the ctrl Tx buffer. In case of a set
 * command this will typically only be the two-letter command ID issued.
 *
 * @param cmd  set command issued
 */
void ctrlCmdSet(uint8_t cmd);

/**
 * Commit all previous changes by writing the datastructures to EEPROM.
 *
 */
void ctrlCmdCommit(void);
