//
// ctrl.c : AVR uC code for ctrl buffer initialisation and put/get ops
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

#include "global.h"
#include "buffer.h"
#include "ctrl.h"

cBuffer ctrlRxBuffer; // ctrl receive buffer
cBuffer ctrlTxBuffer; // ctrl transmit buffer

static char ctrlRxData[CTRL_RX_BUFFER_SIZE];
static char ctrlTxData[CTRL_TX_BUFFER_SIZE];

void ctrlInit(void)
{
	// initialize the CTRL receive buffer
	bufferInit(&ctrlRxBuffer, (u08*) ctrlRxData, CTRL_RX_BUFFER_SIZE);
	// initialize the CTRL transmit buffer
	bufferInit(&ctrlTxBuffer, (u08*) ctrlTxData, CTRL_TX_BUFFER_SIZE);
}

uint8_t ctrlTxBufferIsEmpty(void)
{
	if(ctrlTxBuffer.datalength == 0) {
		return TRUE;
	}
	else {
		return FALSE;
	}
}

uint8_t ctrlAddToTxBuffer(uint8_t data)
{
	return bufferAddToEnd(&ctrlTxBuffer, data);
}

uint8_t ctrlGetFromTxBuffer(uint8_t* data) {
	// make sure we have data in the Tx buffer
	if(ctrlTxBuffer.datalength) {
		// get byte from beginning of buffer
		*data = bufferGetFromFront(&ctrlTxBuffer);
		return TRUE;
	}
	else {
		// no data
		return FALSE;
	}
}

uint8_t ctrlRxBufferIsEmpty(void)
{
	if(ctrlRxBuffer.datalength == 0) {
		return TRUE;
	}
	else {
		return FALSE;
	}
}

uint8_t ctrlAddToRxBuffer(uint8_t data)
{
	return bufferAddToEnd(&ctrlRxBuffer, data);
}

uint8_t ctrlGetFromRxBuffer(uint8_t* data)
{
	// make sure we have data in the Rx buffer
	if(ctrlRxBuffer.datalength) {
		// get byte from beginning of buffer
		*data = bufferGetFromFront(&ctrlRxBuffer);
		return TRUE;
	}
	else {
		// no data
		return FALSE;
	}
}

void ctrlRxToTxLoop(void)
{
	uint8_t data;

	while (ctrlGetFromRxBuffer(&data)) {
		ctrlAddToTxBuffer(data);
	}
}
