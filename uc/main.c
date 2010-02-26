//
// main.c : AVR uC code for flukso sensor board
//
// Copyright (c) 2008-2009 jokamajo.org
//               2010      flukso.net
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

#include <string.h>
#include <stdlib.h>

#include "wiring/wiring_private.h"

#include "main.h"

#include <avr/io.h>
// pin/register/ISR definitions
#include <avr/interrupt.h>

// eeprom library
#include <avr/eeprom.h>

// watchdog timer library
#include <avr/wdt.h>

// variable declarations
volatile struct state aux[4] = {{false, false, START, 0, 0}, {false, false, START, 0, 244}, {false, false, START, 0, 0}, {false, false, START, 0, 0}};

volatile struct sensor EEMEM EEPROM_measurements[4] = {{SENSOR0, START}, {SENSOR1, START}, {SENSOR2, START}, {SENSOR3, START}};
volatile struct sensor measurements[4];

volatile uint8_t muxn = 0;

// interrupt service routine for INT0
ISR(INT0_vect) {
  measurements[2].value++;
  aux[2].pulse = true;
}

// interrupt service routine for INT1
ISR(INT1_vect) {
  measurements[3].value++;
  aux[3].pulse = true;
}

// interrupt service routine for PCI2 (PCINT20)
/**
ISR(PCINT2_vect) {
  if (aux[4].toggle == false) {
    aux[4].toggle = true;
  }
  else {
    measurements[4].value++;
    aux[4].pulse = true;
    aux[4].toggle = false;
  }
}
**/

// interrupt service routine for ADC
ISR(TIMER2_OVF_vect) {
  // read ADC result
  // add to nano(Wh) counter
  if (muxn < 2) {
    MacU16X16to32(aux[muxn].nano, METERCONST, ADC);

    if (++aux[muxn].count > 487) {
      aux[muxn].adc = ADC;
      aux[muxn].toggle = true;
      aux[muxn].count = 0;
    }

    if (aux[muxn].nano > 1000000000) {
       measurements[muxn].value++;
       aux[muxn].pulse = true;
       aux[muxn].nano -= 1000000000;
    }
  }

  // rotate amongst the available ADC input channels (0 to 7)
  muxn++;
  muxn &= 0x7;

  // but only use ADC0 and 1
  if (muxn < 2) {
    ADMUX &= 0xF8;
    ADMUX |= muxn;
  
    // start a new ADC conversion
    ADCSRA |= (1<<ADSC);
  }
}

// interrupt service routine for analog comparator
ISR(ANALOG_COMP_vect) {
  uint8_t i;

  //debugging:
  //measurements[3].value = END3;
  //measurements[2].value = END2;
  //measurements[1].value = END1;
  //measurements[0].value = END0;

  //disable uC sections to consume less power while writing to EEPROM

  //disable UART Tx and Rx:
  UCSR0B &= ~((1<<RXEN0) | (1<<TXEN0));
  //disable ADC:
  ADCSRA &= ~(1<<ADEN);

  for (i=0; i<4; i++)
    eeprom_write_block((const void*)&measurements[i].value, (void*)&EEPROM_measurements[i].value, 4);

  //indicate writing to EEPROM has finished by lighting up the green LED
  PORTB |= (1<<PB5);

  //enable UART Tx and Rx:
  UCSR0B |= (1<<RXEN0) | (1<<TXEN0);
  // enable ADC and start a first ADC conversion
  ADCSRA |= (1<<ADEN) | (1<<ADSC);

  printString("msg BROWN-OUT\n");
}

// interrupt service routine for watchdog timeout
ISR(WDT_vect) {
  uint8_t i;

  for (i=0; i<4; i++)
    eeprom_write_block((const void*)&measurements[i].value, (void*)&EEPROM_measurements[i].value, 4);

  printString("msg WDT\n");
}

// disable WDT
void WDT_off(void) {
  cli();
  wdt_reset();
  // clear the WDT reset flag in the status register
  MCUSR &= ~(1<<WDRF);
  // timed sequence to be able to change the WDT settings afterwards
  WDTCSR |= (1<<WDCE) | (1<<WDE);
  // disable WDT
  WDTCSR = 0x00;
}

// enable WDT
void WDT_on(void) {
  // enable the watchdog timer (1s)
  wdt_enable(WDTO_2S);
  // set watchdog interrupt enable flag
  WDTCSR |= (1<<WDIE);
}

void setup()
{
  // WDT_off(); -> moved the call to this function to start of the main loop, before init

  // clock settings: divide by 8 to get a 1Mhz clock, allows us to set the BOD level to 1.8V (DS p.37)
  CLKPR = (1<<CLKPCE);
  CLKPR = (1<<CLKPS1) | (1<<CLKPS0);

  // load meterid's and metervalues from EEPROM
  eeprom_read_block((void*)&measurements, (const void*)&EEPROM_measurements, sizeof(measurements));

  // init serial port
  beginSerial(4800);
  _delay_ms(100);

  //LEDPIN=PB5/SCK configured as output pin
  DDRB |= (1<<PB5);

  // PD2=INT0 and PD3=INT1 configuration
  // set as input pin with 20k pull-up enabled
  PORTD |= (1<<PD2) | (1<<PD3);
  // INT0 and INT1 to trigger an interrupt on a falling edge
  EICRA = (1<<ISC01) | (1<<ISC11);
  // enable INT0 and INT1 interrupts
  EIMSK = (1<<INT0) | (1<<INT1);


  // PD4=PCINT20 configuration
  // set as input pin with 20k pull-up enabled
  PORTD |= (1<<PD4);
  //enable pin change interrupt on PCINT20
  PCMSK2 |= (1<<PCINT20);
  //pin change interrupt enable 2
  PCICR |= (1<<PCIE2);

  // analog comparator setup for brown-out detection
  // PD7=AIN1 configured by default as input to obtain high impedance
 
  // disable digital input cicuitry on AIN0 and AIN1 pins to reduce leakage current
  DIDR1 |= (1<<AIN1D) | (1<<AIN0D);  
 
  // comparing AIN1 (Vcc/4.4) to bandgap reference (1.1V)
  // bandgap select | AC interrupt enable | AC interrupt on rising edge (DS p.243)
  ACSR |= (1<<ACBG) | (1<<ACIE) | (1<<ACIS1) | (1<<ACIS0);

  // Timer2 normal operation
  // Timer2 clock prescaler set to 1 => fTOV2 = 1000kHz / 256 / 1 = 3906.24Hz (DS p.158)
  TCCR2B |= (1<<CS20);
  TIMSK2 |= (1<<TOIE2);

  // disable digital input cicuitry on ADCx pins to reduce leakage current
  DIDR0 |= (1<<ADC5D) | (1<<ADC4D) | (1<<ADC3D) | (1<<ADC2D) | (1<<ADC1D) | (1<<ADC0D);

  // select VBG as reference for ADC
  ADMUX |= (1<<REFS1) | (1<<REFS0);
  // ADC prescaler set to 8 => 1000kHz / 8 = 125kHz (DS p.258)
  ADCSRA |= (1<<ADPS1) | (1<<ADPS0);
  // enable ADC and start a first ADC conversion
  ADCSRA |= (1<<ADEN) | (1<<ADSC);

  //set global interrupt enable in SREG to 1 (DS p.12)
  sei();
}

void send(uint8_t msg_type, const struct sensor *measurement, const struct state *aux)
{
  uint8_t i = 46;
  char message[49];
  uint32_t value = 0;

  switch (msg_type) {
  case PULSE:
    // blink the green LED
    PORTB |= (1<<PB5);
    _delay_ms(50);
    PORTB &= ~(1<<PB5);

    cli();
    value = measurement->value;
    sei();
 
    strcpy(message, "pls ");
    break;

  case POWER:
    cli();
    uint16_t adc = aux->adc;
    sei();

    MacU16X16to32(value, adc, POWERCONST);
    value /= 1000;

    strcpy(message, "pwr ");
    break;
  }

  strcpy(&message[4], measurement->id);
  strcpy(&message[36], ":0000000000\n");

  do {                                // generate digits in reverse order
    message[i--] = '0' + value % 10;  // get next digit
  } while ((value /= 10) > 0);        // delete it

  printString(message);
}

void loop()
{
  uint8_t i;
 
  // check whether we have to send out a pls or pwr to the deamon
  for (i=0; i<4; i++) {
    if (aux[i].pulse == true) {
      send(PULSE, (const struct sensor *)&measurements[i], (const struct state *)&aux[i]);
      aux[i].pulse = false;
    }

    if (aux[i].toggle == true && i < 2) {
      send(POWER, (const struct sensor *)&measurements[i], (const struct state *)&aux[i]);
      aux[i].toggle = false;
    }  
  }
  wdt_reset();
}

int main(void)
{
  uint8_t i;

  WDT_off();
  setup();

  // insert a startup delay of 20s to prevent interference with redboot
  // interrupts are already enabled at this stage
  // so the pulses are counted but not sent to the deamon
  for (i=0; i<4; i++) _delay_ms(5000);

  serialFlush();
  printString("\n");

  WDT_on();

  for (;;) loop();
  return 0;
}
