#if DBG > 0
	/* set LED pin high/low at the start/end of an ISR */
	#define DBG_ISR_BEGIN()		PORTB |= (1<<PB0)
	#define DBG_ISR_END()		PORTB &= ~(1<<PB0)

	/* Set PB1=OC1A as output pin and toggle this pin on TIMER1 compare match */
	#define  DBG_OC1A_TOGGLE()	DDRB |= (1<<DDB1); \
					TCCR1A |= 1<<COM1A0

	#define DBG_LED_DELAY_ON()	/* nothing */
	#define DBG_LED_ON()		/* nothing */
	#define DBG_LED_OFF()		/* nothing */
#else
	#define DBG_ISR_BEGIN()		/* nothing */
	#define DBG_ISR_END()		/* nothing */
	#define DBG_OC1A_TOGGLE()	/* nothing */

	/* LED behaviour in non-debugging mode */
	#define DBG_LED_DELAY_ON()	TCNT0 = 0; TIMSK0 |= (1<<TOIE0)
	#define DBG_LED_ON()		TIMSK0 &= ~(1<<TOIE0); PORTB &= ~(1<<PB0) /* and disable timer */
	#define DBG_LED_OFF()		PORTB |= (1<<PB0)
#endif
