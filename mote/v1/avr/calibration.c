// Copyright (c) 2008 jokamajo.org
// $Id$

// define section, move to main.h later on

// macro's
#ifndef __AVR_ATmega168__
	#define __AVR_ATmega168__
#endif

#define METER0 "cccccccccccccccccccccccccccccccc"
#define METER1 "dddddddddddddddddddddddddddddddd"
#define METER2 "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
#define METER3 "ffffffffffffffffffffffffffffffff"

#define START 0
#define END3 0xffffffff
#define END2 0xeeeeeeee
#define END1 0xdddddddd
#define END0 0xcccccccc

// pin definitions
#define METER0PIN 2
#define METER1PIN 3
#define METER2PIN 4
#define METER3PIN 9

#define POTPIN0 6
#define POTPIN1 7
#define POTPIN2 8

#define LEDPIN 13

// end of define


// pin/register/ISR definitions
#include <avr/interrupt.h>

// eeprom library
#include <avr/eeprom.h>

// watchdog timer library
#include <avr/wdt.h>

// variable declarations
uint16_t i;

typedef struct {
  boolean pulse0;
  boolean toggle0;
  boolean pulse1;
  boolean toggle1;
  boolean pulse2;
  boolean toggle2;
  boolean pulse3;
  boolean toggle3;
} struct_aux;

volatile struct_aux aux = {false, false, false, false, false, false, false, false};


typedef struct {
 char meter[513]; //don't forget to reserve a byte for a terminating NULL
} struct_meas;

volatile struct_meas EEMEM EEPROM_measurements;
volatile struct_meas measurements;


// interrupt service routine for analog comparator
ISR(ANALOG_COMP_vect) {
  digitalWrite(LEDPIN, HIGH);   // sets the LED on
  UCSR0B &= ~((1<<RXEN0) | (1<<TXEN0)); //disable UART Tx and Rx
  ADCSRA &= ~(1<<ADEN); //disable ADC
  ACSR |= (1<<ACD); //disable AC

//  PRR |= (1<<PRUSART0) | (1<<PRADC);
  eeprom_write_block((const void*)&measurements, (void*)&EEPROM_measurements, sizeof(measurements));
}

// interrupt service routine for watchdog timeout
ISR(WDT_vect) {
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
  wdt_enable(WDTO_1S);
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
//  eeprom_read_block((void*)&measurements, (const void*)&EEPROM_measurements, sizeof(measurements));

  // init serial port
  Serial.begin(4800);
  delay(100);

  pinMode(LEDPIN, OUTPUT);

  // DP2=INT0 configuration
  pinMode(METER0PIN, INPUT);
  // turn on the internal 20k pull-up resistor
  digitalWrite(METER0PIN, HIGH);
  // set INT0 (=METER0PIN) active LOW interrupt

  // DP3=INT1 configuration
  pinMode(METER1PIN, INPUT);
  // turn on the internal 20k pull-up resistor
  digitalWrite(METER1PIN, HIGH);
  // set INT1 (=METER1PIN) active LOW interrupt

  // PD4=PCINT20 configuration
  pinMode(METER2PIN, INPUT);
  // turn on the internal 20k pull-up resistor
  digitalWrite(METER2PIN, HIGH);
  //enable pin change interrupt on PCINT20
  PCMSK2 |= (1<<PCINT20);
  //pin change interrupt enable 2
  PCICR |= (1<<PCIE2);

  // PB1=PCINT1 configuration
  pinMode(METER3PIN, INPUT);
  // turn on the internal 20k pull-up resistor
  digitalWrite(METER3PIN, HIGH);
  //enable pin change interrupt on PCINT1
  PCMSK0 |= (1<<PCINT1);
  //pin change interrupt enable 0
  PCICR |= (1<<PCIE0);

  // analog comparator setup for brown-out detection
  // DP6=Vcc+R20k configuration
  pinMode(POTPIN0, INPUT);
  // turn on the internal 20k pull-up resistor
  digitalWrite(POTPIN0, HIGH); 
  // DP7=AIN1 just configure as input to obtain high impedance
  pinMode(POTPIN1, INPUT);
  // DP8=GND configuration + connect the DP8 pin to GND
  pinMode(POTPIN2, INPUT);
  // comparing AIN1 (Vcc/4.4) to bandgap reference (1.1V)
  // bandgap select | AC interrupt enable | AC interrupt on rising edge (DS p.243)
  ACSR |= (1<<ACBG) | (1<<ACIE) | (1<<ACIS1) | (1<<ACIS0);

  WDT_on();

  //set global interrupt enable in SREG to 1 (DS p.12)
  sei();

  for(i=0; i<sizeof(measurements.meter); i++)
    measurements.meter[i] = 0x12;
}


void loop()
{
  // reset the watchdog timer
  wdt_reset();
  Serial.println("msg testing the integrity of the UART interface");
  delay(500);
}
