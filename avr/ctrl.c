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

#include <avr/eeprom.h>
#include <util/crc16.h>

#include "global.h"
#include "main.h"
#include "buffer.h"
#include "ctrl.h"
#include "encode.h"

#include "rfm12.h"

cBuffer ctrlRxBuffer; // ctrl receive buffer
cBuffer ctrlTxBuffer; // ctrl transmit buffer

static char ctrlRxData[CTRL_RX_BUFFER_SIZE];
static char ctrlTxData[CTRL_TX_BUFFER_SIZE];

extern version_t EEMEM version_eep;
extern version_t version;

extern event_t EEMEM event_eep;
extern event_t event;

extern uint8_t max_analog_sensors;

extern uint8_t EEMEM port_config_eep;
extern uint8_t port_config;

extern uint8_t EEMEM enabled_eep;
extern uint8_t enabled;

extern uint8_t EEMEM phy_to_log_eep[MAX_SENSORS];
extern uint8_t phy_to_log[MAX_SENSORS];

extern sensor_t EEMEM sensor_eep[MAX_SENSORS];
extern sensor_t sensor[MAX_SENSORS];

extern state_t state[MAX_SENSORS];

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

uint8_t ctrlGetFromRxBuffer(uint8_t* pdata)
{
	// make sure we have data in the Rx buffer
	if(ctrlRxBuffer.datalength) {
		// get byte from beginning of buffer
		*pdata = bufferGetFromFront(&ctrlRxBuffer);
		return TRUE;
	}
	else {
		// no data
		return FALSE;
	}
}

void ctrlFlushRxBuffer(void)
{
	ctrlRxBuffer.datalength = 0;
}

void ctrlFlushTxBuffer(void)
{
	ctrlTxBuffer.datalength = 0;
}

uint8_t ctrlReadCharFromRxBuffer(uint8_t* pdata)
{
	uint8_t high_hex, low_hex;

	if (ctrlGetFromRxBuffer(&high_hex) && ctrlGetFromRxBuffer(&low_hex)) {
		htob(high_hex, low_hex, pdata);
		return TRUE;
	}
	else {
		return FALSE;
	}	

}

uint8_t ctrlReadShortFromRxBuffer(uint16_t* pdata) 
{
	uint8_t high_char, low_char;

	if(ctrlReadCharFromRxBuffer(&high_char) && ctrlReadCharFromRxBuffer(&low_char)) {
		*pdata = ((uint16_t)high_char << 8) + low_char;
		return TRUE;
	}
	else {
		return FALSE;
	}
}

uint8_t ctrlReadLongFromRxBuffer(uint32_t* pdata) 
{
	uint16_t high_short, low_short;

	if(ctrlReadShortFromRxBuffer(&high_short) && ctrlReadShortFromRxBuffer(&low_short)) {
		*pdata = ((uint32_t)high_short << 16) + low_short;
		return TRUE;
	}
	else {
		return FALSE;
	}
}

uint8_t ctrlWriteCharToTxBuffer(uint8_t data)
{
	uint8_t high_hex, low_hex;

	btoh(data, &high_hex, &low_hex);
	if (ctrlAddToTxBuffer(high_hex) && ctrlAddToTxBuffer(low_hex)) {
		return TRUE;
	}
	else {
		return FALSE;
	}
}

uint8_t ctrlWriteShortToTxBuffer(uint16_t data)
{
	if (ctrlWriteCharToTxBuffer((uint8_t)(data >> 8)) && ctrlWriteCharToTxBuffer((uint8_t)data)) {
		return TRUE;
	}
	else {
		return FALSE;
	}
}

uint8_t ctrlWriteLongToTxBuffer(uint32_t data)
{
	if (ctrlWriteShortToTxBuffer((uint16_t)(data >> 16)) && ctrlWriteShortToTxBuffer((uint16_t)data)) {
		return TRUE;
	}
	else {
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

uint8_t ctrlCalcCrc8(cBuffer* buffer, uint8_t chop)
{
	uint8_t i, crc = 0;

	for (i = 0; i < buffer->datalength - chop; i++) {
		crc = _crc_ibutton_update(crc, bufferGetAtIndex(buffer, i));
	}
	return crc;
}

uint8_t ctrlExtractCrc8fromMessage(cBuffer* buffer)
{
	uint8_t crc, high_hex, low_hex;

	high_hex = bufferGetAtIndex(buffer, buffer->datalength - 2);
	low_hex  = bufferGetAtIndex(buffer, buffer->datalength - 1);

	htob(high_hex, low_hex, &crc);
	return crc;
}

void ctrlDecode(void)
{
	uint8_t cmd[2], crc;

	ctrlFlushTxBuffer();

	crc = ctrlExtractCrc8fromMessage(&ctrlRxBuffer);
	if (ctrlCalcCrc8(&ctrlRxBuffer, 2) != crc) {
		ctrlAddToTxBuffer('z');
		ctrlAddToTxBuffer('z');
	}
	else if (ctrlGetFromRxBuffer(cmd) && ctrlGetFromRxBuffer(cmd+1)) {
		ctrlAddToTxBuffer(cmd[0]);
		ctrlAddToTxBuffer(cmd[1]);

		switch (cmd[0]) {
		case 'g':	/* get */
			ctrlCmdGet(cmd[1]);
			break;
 
		case 's':	/* set */
			ctrlCmdSet(cmd[1]);
			break;

		case 'c':	/* commit */
			if (cmd[1] == 't') ctrlCmdCommit();
			break;
		
		case 'r':	/* rf */
			ctrlCmdRf(cmd[1]);
			break;
		}
	}
	else {
		ctrlAddToTxBuffer('z');
		ctrlAddToTxBuffer('y');
	}

	crc = ctrlCalcCrc8(&ctrlTxBuffer, 0);
	ctrlWriteCharToTxBuffer(crc);

	ctrlAddToTxBuffer('.');

	ctrlFlushRxBuffer();
}

void ctrlCmdGet(uint8_t cmd)
{
	uint8_t i = 0;
	uint32_t tmp32, tmp32_bis;

	switch (cmd) {
	case 'h':		/* hardware {major,minor} version */
		ctrlWriteShortToTxBuffer(version.hw_major);
		ctrlWriteCharToTxBuffer(version.hw_minor);
		break;

	case 's':		/* software {major,minor} version */
		ctrlWriteCharToTxBuffer(version.sw_major);
		ctrlWriteCharToTxBuffer(version.sw_minor);
		break;

	case 'e':		/* sensor enabled | disabled */
		ctrlReadCharFromRxBuffer(&i);

		if (i < MAX_SENSORS) {
			ctrlWriteCharToTxBuffer(i);
			ctrlWriteCharToTxBuffer((enabled >> i) & 0x01);
		}
		break;

	case 'p':		/* phy-to-logical mapping */
		for (i = 0 ; i < MAX_SENSORS; i++) {
			ctrlWriteCharToTxBuffer(phy_to_log[i]);
		}
		break;

	case 'c':		/* sensor counter value */
		ctrlReadCharFromRxBuffer(&i);

		if (i < MAX_SENSORS) {
			cli();
			tmp32 = sensor[i].counter;
			sei();

			ctrlWriteCharToTxBuffer(i);
			ctrlWriteLongToTxBuffer(tmp32);
		}
		break;

	case 'm':		/* sensor meterconstant */
		ctrlReadCharFromRxBuffer(&i);

		if (i < MAX_SENSORS) {
			ctrlWriteCharToTxBuffer(i);
			ctrlWriteShortToTxBuffer(sensor[i].meterconst);
		}
		break;

	case 'f':		/* sensor meterconstant fraction */
		ctrlReadCharFromRxBuffer(&i);

		if (i < MAX_SENSORS) {
			ctrlWriteCharToTxBuffer(i);
			ctrlWriteShortToTxBuffer(sensor[i].fraction);
		}
		break;

	case 'w':		/* watchdog counter */
		ctrlWriteShortToTxBuffer(event.wdt);
		break;

	case 'b':		/* brown-out counter */
		ctrlWriteShortToTxBuffer(event.brown_out);
		break;

	case 'd':		/* delta: all changes since last gd */
		for (i = 0 ; i < MAX_SENSORS; i++) {
			if (state[i].flags & (STATE_PULSE | STATE_POWER)) {
				ctrlWriteCharToTxBuffer(i);

				cli();
				tmp32 = sensor[i].counter;
				tmp32_bis = (i < max_analog_sensors) ? state[i].power : state[i].timestamp;
				state[i].flags &= ~(STATE_PULSE | STATE_POWER);
				sei();

				ctrlWriteLongToTxBuffer(tmp32);
				ctrlWriteLongToTxBuffer(tmp32_bis);
			}
		}
		break;
	}
}

void ctrlCmdSet(uint8_t cmd)
{
	uint8_t i = 0, tmp8 = 0;
	uint16_t tmp16 = 0;
	uint32_t tmp32 = 0;

	switch (cmd) {
	case 'h':		/* hardware {major,minor} version */
		ctrlReadShortFromRxBuffer(&version.hw_major);
		ctrlReadCharFromRxBuffer(&version.hw_minor);

		ctrlWriteShortToTxBuffer(version.hw_major);
		ctrlWriteCharToTxBuffer(version.hw_minor);
		break;

	case 's':		/* software {major,minor} version */
		ctrlReadCharFromRxBuffer(&version.sw_major);
		ctrlReadCharFromRxBuffer(&version.sw_minor);

		ctrlWriteCharToTxBuffer(version.sw_major);
		ctrlWriteCharToTxBuffer(version.sw_minor);
		break;

	case 'e':		/* sensor enabled | disabled */
		ctrlReadCharFromRxBuffer(&i);

		if (i < MAX_SENSORS) {
			ctrlReadCharFromRxBuffer(&tmp8);

			if (tmp8) {
				enabled |= (1 << i);
			}
			else {
				enabled &= ~(1 << i);
			}

			ctrlWriteCharToTxBuffer(i);
			ctrlWriteCharToTxBuffer((enabled >> i) & 0x01);
		}
		break;

	case 'p':		/* phy-to-logical mapping */
		for (i = 0 ; i < MAX_SENSORS; i++) {
			ctrlReadCharFromRxBuffer(&tmp8);

			cli();
			phy_to_log[i] = tmp8;
			sei();

			ctrlWriteCharToTxBuffer(phy_to_log[i]);
		}
		break;

	case 'c':		/* sensor counter value */
		ctrlReadCharFromRxBuffer(&i);

		if (i < MAX_SENSORS) {
			ctrlReadLongFromRxBuffer(&tmp32);

			cli();
			sensor[i].counter = tmp32;
			sei();

			ctrlWriteCharToTxBuffer(i);
			ctrlWriteLongToTxBuffer(tmp32);
		}
		break;

	case 'm':		/* sensor meterconstant */
		ctrlReadCharFromRxBuffer(&i);

		if (i < MAX_SENSORS) {
			ctrlReadShortFromRxBuffer(&tmp16);

			cli();
			sensor[i].meterconst = tmp16;	
			sei();

			ctrlWriteCharToTxBuffer(i);
			ctrlWriteShortToTxBuffer(sensor[i].meterconst);
		}
		break;

	case 'f':		/* sensor meterconstant fraction */
		ctrlReadCharFromRxBuffer(&i);

		if (i < MAX_SENSORS) {
			ctrlReadShortFromRxBuffer(&tmp16);

			cli();
			sensor[i].fraction = tmp16;	
			sei();

			ctrlWriteCharToTxBuffer(i);
			ctrlWriteShortToTxBuffer(sensor[i].fraction);
		}
		break;

	case 'w':		/* watchdog counter */
		ctrlReadShortFromRxBuffer(&tmp16);

		cli();
		event.wdt = tmp16;
		sei();

		ctrlWriteShortToTxBuffer(event.wdt);
		break;

	case 'b':		/* brown-out counter */
		ctrlReadShortFromRxBuffer(&tmp16);

		cli();
		event.brown_out = tmp16;
		sei();

		ctrlWriteShortToTxBuffer(event.brown_out);
		break;
	}
}

void ctrlCmdCommit(void)
{
	cli();
	eeprom_update_block((const void*)&version, (void*)&version_eep, sizeof(version));
	eeprom_update_block((const void*)&event, (void*)&event_eep, sizeof(event));
	eeprom_update_block((const void*)&port_config, (void*)&port_config_eep, sizeof(port_config));
	eeprom_update_block((const void*)&enabled, (void*)&enabled_eep, sizeof(enabled));
	eeprom_update_block((const void*)&phy_to_log, (void*)&phy_to_log_eep, sizeof(phy_to_log));
	eeprom_update_block((const void*)&sensor, (void*)&sensor_eep, sizeof(sensor));
	sei();
}

void ctrlCmdRf(uint8_t cmd)
{
	switch (cmd) {
    }
}

