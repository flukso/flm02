#if DBG > 0
	/* set LED pin high/low at the start/end of an ISR */
	#define DBG_ISR_BEGIN	PORTB |= (1<<PB0);
	#define DBG_ISR_END	PORTB &= ~(1<<PB0);

	/* Set PB1=OC1A as output pin and toggle this pin on TIMER1 compare match */
	#define  DBG_OC1A_TOGGLE	DDRB |= (1<<DDB1); \
					TCCR1A |= 1<<COM1A0;
#else
	#define DBG_ISR_BEGIN	/* nothing */
	#define DBG_ISR_END	/* nothing */
	#define DBG_OC1A_TOGGLE	/* nothing */
#endif

