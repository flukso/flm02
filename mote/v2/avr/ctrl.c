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
#include "encode.h"

cBuffer ctrlRxBuffer; // ctrl receive buffer
cBuffer ctrlTxBuffer; // ctrl transmit buffer

static char ctrlRxData[CTRL_RX_BUFFER_SIZE];
static char ctrlTxData[CTRL_TX_BUFFER_SIZE];

extern uint8_t phy_to_log[];

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

void ctrlFlushReceiveBuffer(void)
{
	ctrlRxBuffer.datalength = 0;
}

void ctrlRxToTxLoop(void)
{
	uint8_t data;

	while (ctrlGetFromRxBuffer(&data)) {
		ctrlAddToTxBuffer(data);
	}
}

void ctrlDecode(void)
{
	uint8_t cmd[2];

	if (ctrlGetFromRxBuffer(cmd) && ctrlGetFromRxBuffer(cmd+1)) {
		ctrlAddToTxBuffer(cmd[0]);
		ctrlAddToTxBuffer(cmd[1]);

		switch (cmd[0]) {
		case 'g':
			ctrlCmdGet(cmd[1]);
			break;
		case 's':
			ctrlCmdSet(cmd[1]);
			break;
		case 'c':
			if (cmd[1] == 't') ctrlCmdCommit();
			break;
		}

		ctrlAddToTxBuffer('.');
	}

	ctrlFlushReceiveBuffer();
}

void ctrlCmdGet(uint8_t cmd)
{
	uint8_t i;
	uint16_t hex;

	switch (cmd) {
	case 'p':
		for (i = 0 ; i < MAX_SENSORS; i++) {
			hex = btoh(phy_to_log[i]);
			ctrlAddToTxBuffer((uint8_t)(hex >> 8));
			ctrlAddToTxBuffer((uint8_t)hex);
		}
		break;
	}
}

void ctrlCmdSet(uint8_t cmd)
{
	uint8_t i, high_hex, low_hex;

	switch (cmd) {
	case 'p':
		for (i = 0 ; i < MAX_SENSORS; i++) {
			ctrlGetFromRxBuffer(&high_hex);
			ctrlGetFromRxBuffer(&low_hex);
			phy_to_log[i] = htob(((uint16_t)high_hex << 8) + low_hex);
		}
		break;
	}
}

void ctrlCmdCommit(void)
{
	/* TODO */
}
