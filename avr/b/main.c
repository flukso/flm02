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
#include <stdbool.h>

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/eeprom.h>
#include <avr/power.h>
#include <avr/wdt.h>
#include <util/delay.h>

#include "debug.h"
#include "spi.h"
#include "ctrl.h"
#include "global.h"
#include "encode.h"
#include "main.h"

#include "rfm12.h"

register uint8_t spi_status asm("r7");
uint8_t spi_high_hex;
uint8_t spi_uart_tx_bytes = 0;

uint8_t EEMEM _do_not_use_eep = 0xbe;

version_t EEMEM version_eep = {
	HW_VERSION_MAJOR,
	HW_VERSION_MINOR,
	SW_VERSION_MAJOR,
	SW_VERSION_MINOR
};
version_t version;

event_t EEMEM event_eep = {0, 0};
event_t event;

uint8_t max_analog_sensors = DEFAULT_MAX_ANALOG_SENSORS;

uint8_t EEMEM port_config_eep = ANALOG_EN;
uint8_t port_config;

uint8_t EEMEM enabled_eep = DISABLE_ALL_SENSORS;
uint8_t enabled;

uint8_t EEMEM phy_to_log_eep[MAX_SENSORS] = {
	DISABLE_PORT,
	DISABLE_PORT,
	DISABLE_PORT,
	DISABLE_PORT,
	DISABLE_PORT,
	DISABLE_PORT
};
uint8_t phy_to_log[MAX_SENSORS];

const uint8_t phy_to_pin[MAX_SENSORS] = {PC7, PC0, PC1, PC4, PC5};

sensor_t EEMEM sensor_eep[MAX_SENSORS];
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
		DBG_LED_DELAY_ON();
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

static inline bool port_enabled(uint8_t i)
{
	/* 0xff is the default sensor id for non-assigned ports and is disabled by default
	   a further check is done against the 'enabled' bitfield */
	return (i != 0xff) && (enabled & (1 << i));
}

static inline bool port_high(uint8_t pinx, uint8_t i)
{
	return pinx & (1 << phy_to_pin[i]);
}

static inline bool port_low(uint8_t pinx, uint8_t i)
{
	return !port_high(pinx, i);
}

static inline bool high_to_low(state_t state, uint8_t pinx, uint8_t i)
{
	return (state.flags & STATE_PULSE_HIGH) && port_low(pinx, i);
}

static inline bool low_to_high(state_t state, uint8_t pinx, uint8_t i)
{
	return !(state.flags & STATE_PULSE_HIGH) && port_high(pinx, i);
}

static inline void register_pulse(volatile sensor_t *psensor, volatile state_t *pstate)
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

ISR(PCINT1_vect)
{
	DBG_ISR_BEGIN();

	uint8_t i;
	// take a snapshot of the pin state at the start of the ISR
	uint8_t pinc = PINC;

	//check each of the pulse inputs for a high-to-low state transition
	for (i = max_analog_sensors; i < MAX_SENSORS; i++) {
		if (high_to_low(state[i], pinc, i)) {
			state[i].flags &= ~STATE_PULSE_HIGH;

			if (port_enabled(i))
				register_pulse(&sensor[i], &state[i]);
		}

		if (low_to_high(state[i], pinc, i)) {
			state[i].flags |= STATE_PULSE_HIGH;
		}		
	}

	DBG_ISR_END();
}

ISR(TIMER1_COMPA_vect)
{
	DBG_ISR_BEGIN();

	static uint8_t muxn = 0;
	static uint16_t timer = 0;

	uint8_t sensor_id = phy_to_log[muxn];

	if ((muxn < max_analog_sensors) && port_enabled(sensor_id)) {
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
	ADMUX |= phy_to_pin[muxn];
	/* Start a new ADC conversion. */
	ADCSRA |= (1<<ADSC);

	DBG_ISR_END();
}

ISR(TIMER0_OVF_vect)
{
	DBG_LED_ON();
}

static inline void setup_led(void)
{
	// set output low (= LED enabled)
	PORTB &= ~(1<<PB0);
	// set LED pin (PB0) as output pin
	DDRB |= (1<<DDB0);
}

static inline void disable_led(void)
{
	// set LED pin (PB0) as input pin
	DDRB &= ~(1<<DDB0);
	// disable pull-up
	PORTB &= ~(1<<PB0);
}

ISR(TIMER1_CAPT_vect)
{
	disable_led();
	disable_spi();
	rfm12_spi_disable();

	// force UART_TX_INV to 0	
	PORTD &= ~(1<<PD6);

	// throttle the cpu clock to draw less amps
	// raises the number of bytes that can be written to EEPROM from 43 to 48
	clock_prescale_set(clock_div_64);

	event.brown_out++;

#if DBG > 0 
	uint8_t i;

	eeprom_update_block((const void*)&event, (void*)&event_eep, sizeof(event));

	for (i=0; i<128; i++)
		eeprom_write_byte((uint8_t *)(i + 0x0100), i);
#else
	eeprom_update_block((const void*)&sensor, (void*)&sensor_eep, sizeof(sensor));
	eeprom_update_block((const void*)&event, (void*)&event_eep, sizeof(event));
#endif

	wdt_enable(WDTO_250MS);
	_delay_ms(1000);
}

static inline void setup_datastructs(void)
{
	eeprom_read_block((void*)&version, (const void*)&version_eep, sizeof(version));
	eeprom_read_block((void*)&event, (const void*)&event_eep, sizeof(event));
	eeprom_read_block((void*)&port_config, (const void*)&port_config_eep, sizeof(port_config));
	eeprom_read_block((void*)&enabled, (const void*)&enabled_eep, sizeof(enabled));
	eeprom_read_block((void*)&phy_to_log, (const void*)&phy_to_log_eep, sizeof(phy_to_log));
	eeprom_read_block((void*)&sensor, (const void*)&sensor_eep, sizeof(sensor));
}

void setup_ar_uart(void)
{
	// set PD5 & PD6 as output pins
	DDRD |= (1<<DDD5) | (1<<DDD6);

	if (port_config & UART_RX_INV) {
		PORTD |= (1<<PD5);
	} else {
		PORTD &= ~(1<<PD5);
	}

	if (port_config & UART_TX_INV) {
		PORTD |= (1<<PD6);
	} else {
		PORTD &= ~(1<<PD6);
	}
}

void setup_adc(void)
{
	// disable digital input cicuitry on ADCx pins to reduce leakage current
	DIDR0 |= (1<<ADC3D) | (1<<ADC2D);

	// set PC2 as output pin
	DDRC |= (1<<DDC2);

	if (port_config & ANALOG_EN) {
		DIDR0 |= (1<<ADC1D) | (1<<ADC0D);
		PORTC |= (1<<PC2);
	} else {
		max_analog_sensors = 1;
		PORTC &= ~(1<<PC2);
	}

	// select VBG as reference for ADC
	ADMUX |= (1<<REFS1) | (1<<REFS0);
	// ADC prescaler set to 128 => 16MHz / 128 = 125kHz (DS p.258)
	ADCSRA |= (1<<ADPS2) | (1<<ADPS1) | (1<<ADPS0);
	// set ADC mux to first analog port
	ADMUX |= phy_to_pin[0];
	// enable ADC and start a first ADC conversion
	ADCSRA |= (1<<ADEN) | (1<<ADSC);
}

void setup_pulse_input(void)
{
	uint8_t i, pinc;

	//set PC4 (=PCINT12) & PC5 (=PCINT13) as input pins with internal pull-ups
	PORTC |= (1<<PC4) | (1<<PC5);
	//enable pin change interrupts on PCINT12 & 13
	PCMSK1 |= (1<<PCINT12) | (1<<PCINT13);

	if (port_config & ANALOG_EN) {
		//disable PC0 & PC1 internal pull-ups
		PORTC &= ~((1<<PC0) | (1<<PC1));
		//disable pin change interrupts on PCINT8 & 9
		PCMSK1 &= ~((1<<PCINT8) | (1<<PCINT9));
	} else {
		//set PC0 (=PCINT8) & PC1 (=PCINT9) as input pins with internal pull-ups
		PORTC |= (1<<PC0) | (1<<PC1);
		//enable pin change interrupts on PCINT8 & 9
		PCMSK1 |= (1<<PCINT8) | (1<<PCINT9);
	}

	pinc = PINC;	

	//sample each of the pulse inputs and store the logic state
	for (i = max_analog_sensors; i < MAX_SENSORS; i++) {
		if (port_high(pinc, i)) {
			state[i].flags = STATE_PULSE_HIGH;
		}
	}

	//enable pin change interrupt 1
	PCICR |= (1<<PCIE1);
}

static inline void setup_timer0(void)
{
	// Timer0 prescaler set to 1024 giving us a base freq of 7.8125kHz (DS p.106)
	// With timer0 in normal operation, an overflow will occur in 32.768ms
	TCCR0B |= (1<<CS02) | (1<<CS00);
}

static inline void setup_timer1(void)
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

static inline void setup_analog_comparator(void)
{
	// analog comparator setup for brown-out detection
	// PD7=AIN1 configured by default as input to obtain high impedance

	// disable digital input cicuitry on AIN0 and AIN1 pins to reduce leakage current
	DIDR1 |= (1<<AIN1D) | (1<<AIN0D);

	// comparing AIN1 (Vcc/4.4) to bandgap reference (1.1V)
	// select bandgap reference and enable input capture function in timer1 (DS p.244 & 116)
	ACSR |= (1<<ACBG) | (1<<ACIC);
}

static inline void calculate_power(volatile state_t *pstate)
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
	MCUSR = 0;
	wdt_disable();

	//TODO replace by an ana comp polling loop
	_delay_ms(10);

	setup_datastructs();
	setup_led();
	setup_ar_uart();
	setup_adc();
	setup_pulse_input();
	setup_analog_comparator();
	setup_timer0();
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

		for (i = 0; i < max_analog_sensors; i++) {
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
