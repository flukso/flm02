//
// main.c : init functions and primary interrupt routines
//
// Copyright (c) 2010-2011 bart.vandermeerssche@flukso.net
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
#include <avr/power.h>

#include <util/delay.h>

#include "debug.h"
#include "main.h"
#include "spi.h"
#include "ctrl.h"
#include "global.h"
#include "encode.h"

#include "rfm12.h"

register uint8_t spi_status asm("r7");
uint8_t spi_high_hex;
uint8_t spi_uart_tx_bytes = 0;

uint8_t EEMEM first_EEPROM_byte_not_used_to_protect_from_brownout_corruption = 0xbe;

version_t EEMEM EEPROM_version =
	{HW_VERSION_MAJOR, HW_VERSION_MINOR, SW_VERSION_MAJOR, SW_VERSION_MINOR};
version_t version;

event_t EEMEM EEPROM_event = {0, 0};
event_t event;

uint8_t EEMEM EEPROM_enabled = DISABLE_ALL_SENSORS;
uint8_t enabled;

uint8_t EEMEM EEPROM_phy_to_log[MAX_SENSORS] =
	{DISABLE_PORT, DISABLE_PORT, DISABLE_PORT, DISABLE_PORT, DISABLE_PORT, DISABLE_PORT};
uint8_t phy_to_log[MAX_SENSORS];

sensor_t EEMEM EEPROM_sensor[MAX_SENSORS];
volatile sensor_t sensor[MAX_SENSORS];

volatile state_t state[MAX_SENSORS];

time_t time = {0, 0};

ISR(SPI_STC_vect)
{
	uint8_t spi_rx, spi_tx, rx, tx; 

	DBG_ISR_BEGIN();

	// the SPI is double-buffered, requiring two NO_OPs when switching from Tx to Rx
	if (spi_status & (SPI_NO_OP_1 | SPI_NO_OP_2)) {
		spi_status--;
		DBG_LED_ON();
		goto finish;
	}

	// do we have to transmit the first byte?
	if (spi_status & SPI_START_TX) {
		received_from_spi(SPI_FORWARD_TO_CTRL_PORT);
		spi_status &= ~SPI_START_TX;
		goto finish;
	}

	// are we in Tx mode?
	if (spi_status & SPI_TRANSMIT) {
		if (spi_status & SPI_HIGH_HEX) {
			received_from_spi(spi_high_hex); /* actually low hex ! */
			spi_status &= ~SPI_HIGH_HEX;
			goto finish;
		}

		if (spi_status & SPI_TO_FROM_UART) {
			if (spi_uart_tx_bytes) {
				tx = rfm12_rx_byte(rfm12_rx_len() - spi_uart_tx_bytes--);
				btoh(tx, &spi_tx, (uint8_t *)&spi_high_hex); /* actually low hex ! */
				spi_status |= SPI_HIGH_HEX;
				received_from_spi(spi_tx);
				goto finish;
			}
			else {
				rfm12_rx_clear();

				received_from_spi(SPI_END_OF_TX);
				spi_status &= ~SPI_TRANSMIT;
				spi_status |= SPI_NO_OP_2;
				goto finish;
			}
		}
		else {
			if (ctrlGetFromTxBuffer(&tx)) {
				received_from_spi(tx);
				goto finish;
			}
			else {
				received_from_spi(SPI_FORWARD_TO_UART_PORT);
				spi_status |= SPI_TO_FROM_UART;

				spi_uart_tx_bytes = (rfm12_rx_status() == STATUS_COMPLETE) ? rfm12_rx_len() : 0;

				goto finish;
			}
		}
	}

	// we're in Rx mode
	switch (spi_rx = received_from_spi(0x00)) {
		case SPI_END_OF_TX:
			spi_status |= SPI_TRANSMIT | SPI_START_TX; 
			spi_status &= ~(SPI_HIGH_HEX | SPI_TO_FROM_UART);
			rfm12_tx_start();
			break;
		case SPI_END_OF_MESSAGE:
			if (!(spi_status & SPI_TO_FROM_UART)) {
				spi_status |= SPI_NEW_CTRL_MSG;
			}
			break;
		case SPI_FORWARD_TO_UART_PORT:
			spi_status |= SPI_TO_FROM_UART;
			rfm12_tx_occupy();
			DBG_LED_OFF();
			break;
		case SPI_FORWARD_TO_CTRL_PORT:
			spi_status &= ~SPI_TO_FROM_UART;
			DBG_LED_OFF();
			break;
		default:
			if (spi_status & SPI_HIGH_HEX) {
				htob(spi_high_hex, spi_rx, &rx);
				rfm12_tx_buffer_add(rx);
			}
			else {
				if (spi_status & SPI_TO_FROM_UART) {
					spi_high_hex = spi_rx;
				}
				else {
					ctrlAddToRxBuffer(spi_rx);
					goto finish;
				}

			}
			// toggle the HEX bit in spi_status
			spi_status ^= SPI_HIGH_HEX;
	}

finish:
	DBG_ISR_END();
}

/** TODO remap to pins PC4/5 and use PCINT for detecting pulses
ISR(INT0_vect)
{
	DBG_ISR_BEGIN();

	uint8_t sensor_id = phy_to_log[PORT_PULSE_1];

	if (ENABLED(sensor_id))
		register_pulse(&sensor[sensor_id], &state[sensor_id]);

	DBG_ISR_END();
}

ISR(INT1_vect)
{
	DBG_ISR_BEGIN();

	uint8_t sensor_id = phy_to_log[PORT_PULSE_2];

	if (ENABLED(sensor_id))
		register_pulse(&sensor[sensor_id], &state[sensor_id]);

	DBG_ISR_END();
}
**/
void register_pulse(volatile sensor_t *psensor, volatile state_t *pstate)
{
	psensor->counter += psensor->meterconst;
	pstate->milli += psensor->fraction;

	if (psensor->meterconst || pstate->milli >= M_UNIT) {
		pstate->flags |= STATE_PULSE;
		pstate->timestamp = time.ms;

		if (pstate->milli >= M_UNIT) {
			pstate->milli -= M_UNIT;
			psensor->counter++;
		}
	}
}

ISR(TIMER1_COMPA_vect)
{
	DBG_ISR_BEGIN();

	static uint8_t muxn = 0;
	static uint16_t timer = 0;

	uint8_t sensor_id = phy_to_log[muxn];

	if (ENABLED(sensor_id)) {
		/*  clear the power calculation lock when starting a new 1sec cycle */
		if (timer == 0)
			state[sensor_id].flags &= ~STATE_POWER_LOCK;


		MacU16X16to32(state[sensor_id].nano, sensor[sensor_id].meterconst, ADC);

		if (state[sensor_id].nano >= N_UNIT) {
			sensor[sensor_id].counter++;

			state[sensor_id].flags |= STATE_PULSE;
			state[sensor_id].nano -= N_UNIT;
			state[sensor_id].pulse_count++;
		}

		if ((timer == SECOND) && !(state[sensor_id].flags & STATE_POWER_LOCK)) {
			state[sensor_id].nano_start = state[sensor_id].nano_end;
			state[sensor_id].nano_end = state[sensor_id].nano;
			state[sensor_id].pulse_count_final = state[sensor_id].pulse_count;
			state[sensor_id].pulse_count = 0;
			state[sensor_id].flags |= STATE_POWER_CALC | STATE_POWER_LOCK;
		}
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

	DBG_ISR_END();
}

void setup_led(void)
{
	// set output low (= LED enabled)
	PORTB &= ~(1<<PB0);
	// set LED pin (PB0) as output pin
	DDRB |= (1<<DDB0);
}

void disable_led(void)
{
	// set LED pin (PB0) as input pin
	DDRB &= ~(1<<DDB0);
	// disable pull-up
	PORTB &= ~(1<<PB0);
}

ISR(TIMER1_CAPT_vect)
{
	disable_led();
	SPI_DISABLE();

	// throttle the cpu clock to draw less amps
	// raises the number of bytes that can be written to EEPROM from 43 to 48
	clock_prescale_set(clock_div_64);

	event.brown_out++;

#if DBG > 0 
	uint8_t i;

	eeprom_update_block((const void*)&event, (void*)&EEPROM_event, sizeof(event));

	for (i=0; i<128; i++)
		eeprom_write_byte((uint8_t *)(i + 0x0100), i);
#else
	eeprom_update_block((const void*)&sensor, (void*)&EEPROM_sensor, sizeof(sensor));
	eeprom_update_block((const void*)&event, (void*)&EEPROM_event, sizeof(event));
#endif

	// restore the original clock setting
	clock_prescale_set(clock_div_1);

	SPI_ENABLE();
	setup_led();
	FLAG_CLR_ICF1();
}

void setup_datastructs(void)
{
	eeprom_read_block((void*)&version, (const void*)&EEPROM_version, sizeof(version));
	eeprom_read_block((void*)&event, (const void*)&EEPROM_event, sizeof(event));
	eeprom_read_block((void*)&enabled, (const void*)&EEPROM_enabled, sizeof(enabled));
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
	// Timer1 prescaler set to 64 giving us a base freq of 125kHz (DS p.134)
	TCCR1B |= (1<<CS11) | (1<<CS10);
	// Decrease timer freq to 2kHz (DS p.122)
	OCR1A = 0x7c;
	// Timer1 set to CTC mode (DS p.133)
	TCCR1B |= 1<<WGM12;
	// Enable output compare match interrupt for timer1 (DS p.136)
	TIMSK1 |= (1<<OCIE1A);

	
	// Activate the input capture noise canceler and trigger the IC on a positive edge (DS p.133)
	TCCR1B |= (1<<ICNC1) | (1<<ICES1);
	// Enable input capture interrupt (DS p.136)
	TIMSK1 |= (1<<ICIE1);

	DBG_OC1A_TOGGLE();
}

void setup_analog_comparator(void)
{
	// analog comparator setup for brown-out detection
	// PD7=AIN1 configured by default as input to obtain high impedance

	// disable digital input cicuitry on AIN0 and AIN1 pins to reduce leakage current
	DIDR1 |= (1<<AIN1D) | (1<<AIN0D);

	// comparing AIN1 (Vcc/4.4) to bandgap reference (1.1V)
	// select bandgap reference and enable input capture function in timer1 (DS p.244 & 116)
	ACSR |= (1<<ACBG) | (1<<ACIC);
}

void calculate_power(volatile state_t *pstate)
{
	int32_t rest;
	uint32_t pulse_power, urest, power = 0;
	uint8_t pulse_count;

	cli();
	rest = pstate->nano_end - pstate->nano_start;
	pulse_count = pstate->pulse_count_final;
	sei();

	urest = labs(rest);

	// Since the AVR has no dedicated floating-point hardware, we need 
	// to resort to fixed-point calculations for converting nWh/s to W.
	// 1W = 10^6/3.6 nWh/s
	// power[watt] = 3.6/10^6 * rest[nWh/s]
	// power[watt] = 3.6/10^6 * 65536 * (rest[nWh/s] / 65536)
	// power[watt] = 3.6/10^6 * 65536 * 262144 / 262144 * (rest[nWh/s] / 65536)
	// power[watt] = 61847.53 / 262144 * (rest[nWh/s] / 65536)
	// We have to correct for only using 666 samples iso 2000/3, so:
	// power[watt] = 61847.53 * 1/666 * 2000/3 / 262144 * (rest[nWh/s] / 65536)
	// power[watt] = 61909.44 / 262144 * (rest[nWh/s] / 65536)
	// We round the constant down to 61909 to prevent 'underflow' in the
	// consecutive else statement.
	// The error introduced in the fixed-point rounding equals 7.1*10^-6.
	MacU16X16to32(power, (uint16_t)(urest/65536U), 61909U);
	power /= 262144U;

	pulse_power = pulse_count*3600UL;

	if (rest >= 0) {
		power += pulse_power;
	}
	else {
		power = pulse_power - power;

		// guard against unsigned integer wrapping
		if (power > pulse_power) {
			power = 0;
		}
	}

	pstate->power = power;
}

int main(void)
{
	uint8_t i;

	cli();
	//TODO replace by an ana comp polling loop
	_delay_ms(10);

	setup_datastructs();
	setup_led();
	//setup_adc();
	//setup_pulse_input();
	setup_analog_comparator();
	setup_timer1();
	
	// initialize the CTRL buffers
	ctrlInit();
	// initialize the SPI in slave mode
	setup_spi(SPI_MODE_0, SPI_MSB, SPI_INTERRUPT, SPI_SLAVE);

	// initialize the Si4421/RFM12 radio and buffers
	rfm12_init();

	// the clk/8 fuse bit is set
	clock_prescale_set(clock_div_1);
	FLAG_CLR_ICF1();
	sei();


	for(;;) {
		if (spi_status & SPI_NEW_CTRL_MSG) {
			ctrlDecode();
			spi_status &= ~SPI_NEW_CTRL_MSG;
		}

		for (i = 0; i < MAX_ANALOG_SENSORS; i++) {
			if (state[i].flags & STATE_POWER_CALC) {
				calculate_power(&state[i]);
				state[i].flags &= ~STATE_POWER_CALC;
				state[i].flags |= STATE_POWER;
			}
		}

		rfm12_tick();
	}

	return 0;
}
