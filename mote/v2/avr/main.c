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

#include <stdlib.h>

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/eeprom.h>

#include <util/delay.h>

#include "main.h"
#include "uart.h"
#include "spi.h"
#include "ctrl.h"
#include "global.h"
#include "encode.h"

volatile uint8_t spi_status, spi_high_hex;

uint8_t EEMEM first_EEPROM_byte_not_used_to_protect_from_brownout_corruption = 0xab;

volatile struct event_struct EEMEM EEPROM_event = {0, 0};
volatile struct event_struct event;

uint8_t EEMEM EEPROM_phy_to_log[MAX_SENSORS] = {0, 1, 2, 3, 4, 5};
uint8_t phy_to_log[MAX_SENSORS];

volatile struct sensor_struct EEMEM EEPROM_sensor[MAX_SENSORS];
volatile struct sensor_struct sensor[MAX_SENSORS];

volatile struct state_struct state[MAX_SENSORS];

volatile uint8_t muxn = 0;
volatile uint16_t timer = 0;

volatile struct time_struct time = {0, 0};

ISR(SPI_STC_vect)
{
	uint8_t spi_rx, rx, tx; 
	uint16_t spi_tx;

	// the SPI is double-buffered, requiring two NO_OPs when switching from Tx to Rx
	if (spi_status & (SPI_NO_OP_1 | SPI_NO_OP_2)) {
		spi_status--;
		return;
	}

	// do we have to transmit the first byte?
	if (spi_status & SPI_START_TX) {
		received_from_spi(SPI_FORWARD_TO_CTRL_PORT);
		spi_status &= ~SPI_START_TX;
		return;
	}

	// are we in Tx mode?
	if (spi_status & SPI_TRANSMIT) {
		if (spi_status & SPI_HIGH_HEX) {
			received_from_spi(spi_high_hex);
			spi_status &= ~SPI_HIGH_HEX;
			return;
		}

		if (spi_status & SPI_TO_FROM_UART) {
			if (!uartReceiveByte(&tx)) {
				received_from_spi(SPI_END_OF_TX);
				spi_status &= ~SPI_TRANSMIT;
				spi_status |= SPI_NO_OP_2;
				return;
			}
		}
		else {
			if (ctrlGetFromTxBuffer(&tx)) {
				received_from_spi(tx);
				return;
			}
			else {
				received_from_spi(SPI_FORWARD_TO_UART_PORT);
				spi_status |= SPI_TO_FROM_UART;
				return;
			}
		}

		spi_tx = btoh(tx);
		spi_high_hex = (uint8_t)spi_tx;
		spi_status |= SPI_HIGH_HEX;
		received_from_spi((uint8_t)(spi_tx >> 8));
		return;
	}

	// we're in Rx mode
	switch (spi_rx = received_from_spi(0x00)) {
		case SPI_END_OF_TX:
			spi_status |= SPI_TRANSMIT | SPI_START_TX; 
			spi_status &= ~(SPI_HIGH_HEX | SPI_TO_FROM_UART);
			break;
		case SPI_END_OF_MESSAGE:
			if (!(spi_status & SPI_TO_FROM_UART)) {
				ctrlAddToRxBuffer(spi_rx);
				spi_status |= SPI_NEW_CTRL_MSG;
			}
			break;
		case SPI_FORWARD_TO_UART_PORT:
			spi_status |= SPI_TO_FROM_UART;
			break;
		case SPI_FORWARD_TO_CTRL_PORT:
			spi_status &= ~SPI_TO_FROM_UART;
			break;
		default:
			if (spi_status & SPI_HIGH_HEX) {
				rx = htob(((uint16_t)spi_high_hex << 8) + spi_rx);
				uartAddToTxBuffer(rx);
			}
			else {
				if (spi_status & SPI_TO_FROM_UART) {
					spi_high_hex = spi_rx;
				}
				else {
					ctrlAddToRxBuffer(spi_rx);
					return;
				}

			}
			// toggle the HEX bit in spi_status
			spi_status ^= SPI_HIGH_HEX;
	}
}

ISR(TIMER1_COMPA_vect)
{
	uint8_t muxn_l = phy_to_log[muxn];
	
	MacU16X16to32(state[muxn_l].nano, sensor[muxn_l].meterconst, ADC);

	if (state[muxn_l].nano > WATT) {
		sensor[muxn_l].counter++;

		state[muxn_l].flags |= STATE_PULSE;
		state[muxn_l].nano -= WATT;
		state[muxn_l].pulse_count++;
	}

	if ((timer == SECOND) && (muxn == muxn_l)) {
		state[muxn].nano_start = state[muxn].nano_end;
		state[muxn].nano_end = state[muxn].nano;
		state[muxn].pulse_count_final = state[muxn].pulse_count;
		state[muxn].pulse_count = 0;
		state[muxn].flags |= STATE_POWER_CALC;
	}

	/* Cycle through the available ADC input channels (0/1/2). */
	muxn++;
	if (!(muxn %= 3)) timer++;
	if (timer > SECOND) timer = 0;

	/* In order to map this to 1000Hz (=ms) we have to skip every second interrupt. */
	if (!time.skip) time.ms++ ;
	time.skip ^= 1;

	ADMUX &= 0xF8;
	ADMUX |= muxn;
	/* Start a new ADC conversion. */
	ADCSRA |= (1<<ADSC);
}

ISR(ANALOG_COMP_vect)
{
	uint8_t i;

	PORTB |= (1<<PB0);

	//disable uC sections to consume less power while writing to EEPROM
	//disable UART Tx and Rx:
	UCSR0B &= ~((1<<RXEN0) | (1<<TXEN0));
	//disable ADC:
	ADCSRA &= ~(1<<ADEN);

	for (i=0; i<128; i++)
		eeprom_write_byte((uint8_t *)i, i); 			

	//enable UART Tx and Rx:
	UCSR0B |= (1<<RXEN0) | (1<<TXEN0);
	// enable ADC and start a first ADC conversion
	ADCSRA |= (1<<ADEN) | (1<<ADSC);

	PORTB &= ~(1<<PB0);
}

void setup_datastructs(void)
{
	eeprom_read_block((void*)&event, (const void*)&EEPROM_event, sizeof(event));
	eeprom_read_block((void*)&phy_to_log, (const void*)&EEPROM_phy_to_log, sizeof(phy_to_log));
	eeprom_read_block((void*)&sensor, (const void*)&EEPROM_sensor, sizeof(sensor));
}

void setup_pulse_input(void)
{
	// PD2=INT0 and PD3=INT1 configuration
	// set as input pin with 20k pull-up enabled
	PORTD |= (1<<PD2) | (1<<PD3);
	// INT0 and INT1 to trigger an interrupt on a falling edge
	EICRA = (1<<ISC01) | (1<<ISC11);
	// enable INT0 and INT1 interrupts
	EIMSK = (1<<INT0) | (1<<INT1);
}

void setup_adc(void)
{
	// disable digital input cicuitry on ADCx pins to reduce leakage current
	DIDR0 |= (1<<ADC5D) | (1<<ADC4D) | (1<<ADC3D) | (1<<ADC2D) | (1<<ADC1D) | (1<<ADC0D);

	// select VBG as reference for ADC
	ADMUX |= (1<<REFS1) | (1<<REFS0);
	// ADC prescaler set to 32 => 3686.4kHz / 32 = 115.2kHz (DS p.258)
	ADCSRA |= (1<<ADPS2) | (1<<ADPS0);
	// enable ADC and start a first ADC conversion
	ADCSRA |= (1<<ADEN) | (1<<ADSC);
}

void setup_timer1(void)
{
	// Timer1 clock prescaler set to 1 => fTOV1 = 3686.4kHz / 65536 = 56.25Hz (DS p.134)
	TCCR1B |= (1<<CS10);
	// Increase sampling frequency to 2kHz (= 667Hz per channel) with an error of 0.01% (DS p.122)
	OCR1A = 0x0732;
	// Timer1 set to CTC mode (DS p.133)
	TCCR1B |= 1<<WGM12;
	// Enable output compare match interrupt for timer1 (DS p.136)
	TIMSK1 |= (1<<OCIE1A);
#if DBG > 0
	// Set PB1=OC1A as output pin
	DDRB |= (1<<DDB1);
	// Toggle pin OC1A=PB1 on compare match
	TCCR1A |= 1<<COM1A0;
#endif
}

void setup_analog_comparator(void)
{
	// analog comparator setup for brown-out detection
	// PD7=AIN1 configured by default as input to obtain high impedance

	// disable digital input cicuitry on AIN0 and AIN1 pins to reduce leakage current
	DIDR1 |= (1<<AIN1D) | (1<<AIN0D);

	// comparing AIN1 (Vcc/4.4) to bandgap reference (1.1V)
	// bandgap select | AC interrupt enable | AC interrupt on rising edge (DS p.243)
	ACSR |= (1<<ACBG) | (1<<ACIE) | (1<<ACIS1) | (1<<ACIS0);
}

void calculate_power(struct state_struct *pstate)
{
	int32_t rest, power = 0;
	uint8_t pulse_count;

	cli();
	rest = pstate->nano_end - pstate->nano_start;
	pulse_count = pstate->pulse_count_final;
	sei();

	// Since the AVR has no dedicated floating-point hardware, we need 
	// to resort to fixed-point calculations for converting nWh/s to W.
	// 1W = 10^6/3.6 nWh/s
	// value[watt] = 3.6/10^6 * rest[nWh/s]
	// value[watt] = 3.6/10^6 * 65536 * (rest[nWh/s] / 65536)
	// value[watt] = 3.6/10^6 * 65536 * 262144 / 262144 * (rest[nWh/s] / 65536)
	// value[watt] = 61847.53 / 262144 * (rest[nWh/s] / 65536)
	// We round the constant down to 61847 to prevent 'underflow' in the
	// consecutive else statement.
	// The error introduced in the fixed-point rounding equals 8.6*10^-6.
	MacU16X16to32(power, (uint16_t)(labs(rest)/65536), 61847);
	power /= 262144;

	if (rest >= 0) {
		power += pulse_count*3600;
	}
	else {
		power = pulse_count*3600 - power;
	}

	pstate->power = power;
}

int main(void)
{
	uint8_t i;

	// RS-485: Configure PD5=DE as output pin with low as default
	DDRD |= (1<<DDD5);
	// set high to transmit
	//PORTD |= (1<<PD5);

	setup_datastructs();
	setup_adc();
	setup_timer1();
	setup_pulse_input();
	setup_analog_comparator();
	
	// initialize the CTRL buffers
	ctrlInit();
	// initialize the UART hardware and buffers
	uartInit();
	// initialize the SPI in slave mode
	setup_spi(SPI_MODE_0, SPI_MSB, SPI_INTERRUPT, SPI_SLAVE);


	for(;;) {
		if (spi_status & SPI_NEW_CTRL_MSG) {
			//ctrlRxToTxLoop();
			ctrlDecode();
			spi_status &= ~SPI_NEW_CTRL_MSG;
		}

		for (i = 0; i < 3; i++) {
			if (state[i].flags & STATE_POWER_CALC) {
				calculate_power((struct state_struct *)&state[i]);
				state[i].flags &= ~STATE_POWER_CALC;
				state[i].flags |= STATE_POWER;
			}
		}

		// toggle the LED=PB0 pin
		_delay_ms(50);
		 DDRB ^= (1<<PB0);
	}

	return 0;
}
