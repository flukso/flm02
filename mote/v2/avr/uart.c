//
// uart.c : UART driver with buffer support
//
// Copyright (c) 2000-2002 Pascal Stang
//		 2010 flukso.net
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

#include <avr/io.h>
#include <avr/interrupt.h>

#include "buffer.h"
#include "uart.h"

// UART global variables
// flag variables
volatile u08   uartReadyTx;		///< uartReadyTx flag
volatile u08   uartBufferedTx;		///< uartBufferedTx flag
// receive and transmit buffers
cBuffer uartRxBuffer;			///< uart receive buffer
cBuffer uartTxBuffer;			///< uart transmit buffer
unsigned short uartRxOverflow;		///< receive overflow counter

#ifndef UART_BUFFERS_EXTERNAL_RAM
	// using internal ram,
	// automatically allocate space in ram for each buffer
	static char uartRxData[UART_RX_BUFFER_SIZE];
	static char uartTxData[UART_TX_BUFFER_SIZE];
#endif

typedef void (*voidFuncPtru08)(unsigned char);
volatile static voidFuncPtru08 UartRxFunc;

// enable and initialize the uart
void uartInit(void)
{
	// initialize the buffers
	uartInitBuffers();
	// initialize user receive handler
	UartRxFunc = 0;

	// enable RxD/TxD and interrupts
	outb(UCR, BV(RXCIE)|BV(RXEN)|BV(TXEN));

	// set default baud rate
	uartSetBaudRate(UART_DEFAULT_BAUD_RATE);  
	// initialize states
	uartReadyTx = TRUE;
	uartBufferedTx = FALSE;
	// clear overflow count
	uartRxOverflow = 0;
	// enable interrupts
	sei();
}

// create and initialize the uart transmit and receive buffers
void uartInitBuffers(void)
{
	#ifndef UART_BUFFERS_EXTERNAL_RAM
		// initialize the UART receive buffer
		bufferInit(&uartRxBuffer, (u08*) uartRxData, UART_RX_BUFFER_SIZE);
		// initialize the UART transmit buffer
		bufferInit(&uartTxBuffer, (u08*) uartTxData, UART_TX_BUFFER_SIZE);
	#else
		// initialize the UART receive buffer
		bufferInit(&uartRxBuffer, (u08*) UART_RX_BUFFER_ADDR, UART_RX_BUFFER_SIZE);
		// initialize the UART transmit buffer
		bufferInit(&uartTxBuffer, (u08*) UART_TX_BUFFER_ADDR, UART_TX_BUFFER_SIZE);
	#endif
}

// redirects received data to a user function
void uartSetRxHandler(void (*rx_func)(unsigned char c))
{
	// set the receive interrupt to run the supplied user function
	UartRxFunc = rx_func;
}

// set the uart baud rate
void uartSetBaudRate(u32 baudrate)
{
	// calculate division factor for requested baud rate, and set it
	u16 bauddiv = ((F_CPU+(baudrate*8L))/(baudrate*16L)-1);
	outb(UBRRL, bauddiv);
	#ifdef UBRRH
	outb(UBRRH, bauddiv>>8);
	#endif
}

// returns the receive buffer structure 
cBuffer* uartGetRxBuffer(void)
{
	// return rx buffer pointer
	return &uartRxBuffer;
}

// returns the transmit buffer structure 
cBuffer* uartGetTxBuffer(void)
{
	// return tx buffer pointer
	return &uartTxBuffer;
}

// transmits a byte over the uart
void uartSendByte(u08 txData)
{
	// wait for the transmitter to be ready
	while(!uartReadyTx);
	// send byte
	outb(UDR, txData);
	// set ready state to FALSE
	uartReadyTx = FALSE;
}

// gets a single byte from the uart receive buffer (getchar-style)
int uartGetByte(void)
{
	u08 c;
	if(uartReceiveByte(&c))
		return c;
	else
		return -1;
}

// gets a byte (if available) from the uart receive buffer
u08 uartReceiveByte(u08* rxData)
{
	// make sure we have a receive buffer
	if(uartRxBuffer.size)
	{
		// make sure we have data
		if(uartRxBuffer.datalength)
		{
			// get byte from beginning of buffer
			*rxData = bufferGetFromFront(&uartRxBuffer);
			return TRUE;
		}
		else
		{
			// no data
			return FALSE;
		}
	}
	else
	{
		// no buffer
		return FALSE;
	}
}

// flush all data out of the receive buffer
void uartFlushReceiveBuffer(void)
{
	// flush all data from receive buffer
	//bufferFlush(&uartRxBuffer);
	// same effect as above
	uartRxBuffer.datalength = 0;
}

// return true if uart receive buffer is empty
u08 uartReceiveBufferIsEmpty(void)
{
	if(uartRxBuffer.datalength == 0)
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

// add byte to end of uart Tx buffer
u08 uartAddToTxBuffer(u08 data)
{
	u08 status;
	// add data byte to the end of the tx buffer
	status = bufferAddToEnd(&uartTxBuffer, data);
	// turn on buffered transmit
	uartBufferedTx = TRUE;
	// enable UDRIE0 interrupt
	UCR |= 1<<UDRIE0;

	return status;
}

// start transmission of the current uart Tx buffer contents
void uartSendTxBuffer(void)
{
	// turn on buffered transmit
	uartBufferedTx = TRUE;
	// send the first byte to get things going by interrupts
	uartSendByte(bufferGetFromFront(&uartTxBuffer));
}
/*
// transmit nBytes from buffer out the uart
u08 uartSendBuffer(char *buffer, u16 nBytes)
{
	register u08 first;
	register u16 i;

	// check if there's space (and that we have any bytes to send at all)
	if((uartTxBuffer.datalength + nBytes < uartTxBuffer.size) && nBytes)
	{
		// grab first character
		first = *buffer++;
		// copy user buffer to uart transmit buffer
		for(i = 0; i < nBytes-1; i++)
		{
			// put data bytes at end of buffer
			bufferAddToEnd(&uartTxBuffer, *buffer++);
		}

		// send the first byte to get things going by interrupts
		uartBufferedTx = TRUE;
		uartSendByte(first);
		// return success
		return TRUE;
	}
	else
	{
		// return failure
		return FALSE;
	}
}
*/
// UART Data Register Empty Interrupt Handler
UART_INTERRUPT_HANDLER(SIG_UART_DATA)
{
	// check if buffered tx is enabled
	if(uartBufferedTx)
	{
		// check if there's data left in the buffer
		if(uartTxBuffer.datalength)
		{
			// set RS-485 into transmit mode
			PORTD |= (1<<PD5);
			// send byte from top of buffer
			outb(UDR, bufferGetFromFront(&uartTxBuffer));
		}
		else
		{
			// no data left
			uartBufferedTx = FALSE;
			// return to ready state
			uartReadyTx = TRUE;
			// disable UDRIE0 interrupt
			UCR &= ~(1<<UDRIE0);
			// enable TXCIE0 interrupt
			UCR |= (1<<TXCIE0);
		}
	}
	else
	{
		// we're using single-byte tx mode
		// indicate transmit complete, back to ready
		uartReadyTx = TRUE;
	}
}

// UART Transmit Complete Interrupt Handler
UART_INTERRUPT_HANDLER(SIG_UART_TRANS)
{
	// set RS-485 into receive mode
	PORTD &= ~(1<<PD5);
	// disable TXCIE0 interrupt
	UCR &= ~(1<<TXCIE0);
}

// UART Receive Complete Interrupt Handler
UART_INTERRUPT_HANDLER(SIG_UART_RECV)
{
	u08 c;
	
	// get received char
	c = inb(UDR);

	// if there's a user function to handle this receive event
	if(UartRxFunc)
	{
		// call it and pass the received data
		UartRxFunc(c);
	}
	else
	{
		// otherwise do default processing
		// put received char in buffer
		// check if there's space
		if( !bufferAddToEnd(&uartRxBuffer, c) )
		{
			// no space in buffer
			// count overflow
			uartRxOverflow++;
		}
	}
}
