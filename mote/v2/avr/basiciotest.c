//
// basiciotest.c : test code for the io and buffer ops of the UART and SPI ports
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

#include <avr/io.h>		// include I/O definitions (port names, pin names, etc)
#include <avr/interrupt.h>	// include interrupt support

#include "uart.h"		// include uart function library
#include "spi.h"
#include "ctrl.h"


#define NO_OP_1		1
#define NO_OP_2		2
#define TRANSMIT	4
#define HIGH_HEX	8
#define TO_FROM_UART	16
#define NEW_CTRL_MSG	32

#define SPI_END_OF_TX			0x00
#define SPI_END_OF_MESSAGE		':'
#define SPI_FORWARD_TO_UART_PORT	'u'
#define SPI_FORWARD_TO_CTRL_PORT	'l' // 'l'ocal port


volatile uint8_t high_hex;
volatile uint8_t spi_status;

// hex to binary/byte decoding
uint8_t htob(uint16_t hex) {
	uint8_t low_hex = (uint8_t) hex;
	uint8_t high_hex = (uint8_t) (hex >> 8); 
	uint8_t byte;

	byte = (high_hex > 0x40) ? (high_hex & 0x0F) + 9 : high_hex & 0x0F;
	byte = byte << 4;
        byte |= (low_hex > 0x40) ? (low_hex & 0x0F) + 9 : low_hex & 0x0F;
	return byte;
}

// binary/byte to hex encoding
uint16_t btoh(uint8_t byte) {
	uint8_t low_nibble = (byte & 0x0F);
	uint8_t high_nibble = (byte & 0xF0) >> 4;
	uint16_t hex;

	hex = (high_nibble > 0x09) ? high_nibble - 9 + 0x60 : high_nibble + 0x30;
	hex = hex << 8;
	hex |= (low_nibble > 0x09) ? low_nibble - 9 + 0x60 : low_nibble + 0x30;
	return hex;
}


SIGNAL(SPI_STC_vect) {
	uint8_t spi_rx, spi_tx, uart_tx; 

	// the SPI is double-buffered, requiring two NO_OPs when switching from Tx to Rx
	if (spi_status & (NO_OP_1 | NO_OP_2)) {
		spi_status--;
		return;
	}

	// are we in Tx mode?
	if (spi_status & TRANSMIT) {
		if (spi_status & TO_FROM_UART) {

		}
		else {
			if (ctrlGetFromTxBuffer(&spi_tx)) {
				received_from_spi(spi_tx);
			}
			else {
				received_from_spi(SPI_END_OF_TX);
				spi_status &= ~TRANSMIT;
				spi_status |= NO_OP_2;
				uartAddToTxBuffer('r'); //debugging
			}
		}

		return;
	}

	// we're in Rx mode
	switch (spi_rx = received_from_spi(0x00)) {
		case SPI_END_OF_TX:
			spi_status |= TRANSMIT; 
			spi_status &= ~(HIGH_HEX | TO_FROM_UART);
			uartAddToTxBuffer('t'); //debugging
			break;
		case SPI_END_OF_MESSAGE:
			if (spi_status & TO_FROM_UART) {
				spi_status &= ~TO_FROM_UART;
			}
			else {
				ctrlAddToRxBuffer(spi_rx);
				spi_status |= NEW_CTRL_MSG;
			}
			break;
		case SPI_FORWARD_TO_UART_PORT:
			spi_status |= TO_FROM_UART;
			break;
		case SPI_FORWARD_TO_CTRL_PORT:
			spi_status &= ~TO_FROM_UART;
			break;
		default:
			//check whether the incoming hex-encoded stream needs to be forwarded to the UART port
			if (spi_status & TO_FROM_UART) {
				if (spi_status & HIGH_HEX) {
					uart_tx = htob(((uint16_t)high_hex << 8) + spi_rx);
					uartAddToTxBuffer(uart_tx);
				}
				else {
					high_hex = spi_rx;
				}
				// toggle to the HEX bit in spi_status
				spi_status ^= HIGH_HEX;
			}
			else {
				// forward to CTRL_RX buffer
				ctrlAddToRxBuffer(spi_rx);
			} 
	}
}

int main(void) {
	// initialize the CTRL buffers
	ctrlInit();

	// initialize the UART buffers with a default UART baud rate of 4800
	uartInit();

	// initialize the SPI in slave mode
	setup_spi(SPI_MODE_0, SPI_MSB, SPI_INTERRUPT, SPI_SLAVE);

	uint8_t data;
	uint16_t send;

	for(;;) {
		if (uartReceiveByte(&data)) {
			// check the HEX bit in spi_status
			if (spi_status & HIGH_HEX) {
				// loopback on the UART itf
				send = btoh(htob(((uint16_t)high_hex << 8) + data));
				uartAddToTxBuffer((uint8_t)(send >> 8));
				uartAddToTxBuffer((uint8_t)(send));
			}
			else {
				high_hex = data;
			}
			// toggle to the HEX bit in spi_status
			spi_status ^= HIGH_HEX;

		}

		if (spi_status & NEW_CTRL_MSG) {
			ctrlLoop();	
			spi_status &= ~NEW_CTRL_MSG;
		}
	}

	return 0;
}
