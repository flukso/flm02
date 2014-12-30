#ifndef _main_h__
#define _main_h__

typedef struct {
	uint16_t hw_major;
	uint8_t  hw_minor;
	uint8_t  sw_major;
	uint8_t  sw_minor;
} version_t;

#define PC7					7

#define ANALOG_EN			0x01
#define UART_RX_INV			0x02
#define UART_TX_INV			0x04

#define SPI_NO_OP_1			0x01
#define SPI_NO_OP_2			0x02
#define SPI_START_TX		0x04
#define SPI_TRANSMIT		0x08
#define SPI_HIGH_HEX		0x10
#define SPI_TO_FROM_UART	0x20
#define SPI_NEW_CTRL_MSG	0x40

#define SPI_END_OF_TX				0x00
#define SPI_END_OF_MESSAGE			'.'
#define SPI_FORWARD_TO_UART_PORT	'u'
#define SPI_FORWARD_TO_CTRL_PORT	'l' // 'l'ocal port

typedef struct {
	uint16_t wdt;
	uint16_t brown_out;
} event_t;

typedef struct {
	uint32_t counter;
	uint16_t meterconst;
	uint16_t fraction;	// expressed in milli
} sensor_t;

# define M_UNIT 1000U		// milli
# define N_UNIT 1000000000U	// nano

# define SECOND 665U // 666Hz - 1

#define STATE_PULSE_HIGH	0x01
#define STATE_PULSE			0x02
#define STATE_POWER_LOCK	0x04
#define STATE_POWER_CALC	0x08
#define STATE_POWER			0x10

#define PULSE_MASK_MS 5

typedef struct {
	uint8_t  flags;

	uint32_t nano;
	uint32_t nano_start;
	uint32_t nano_end;
	uint8_t  pulse_count;
	uint8_t  pulse_count_final;
	uint8_t  pulse_mask; /* ignore state changes while mask > 0 (ms) */

	uint16_t milli;

	uint32_t power;
	uint32_t timestamp;
} state_t;

typedef struct {
	uint8_t	 skip;
	uint32_t ms;
} time_t;

/* 
 * This macro performs a 16x16 -> 32 unsigned MAC in 37 cycles with operands and results in memory
 * based on http://www2.ife.ee.ethz.ch/~roggend/publications/wear/DSPMic_v1.1.pdf par 3.4 and table 31.
 */
#define MacU16X16to32(uint_32Acc, uint_16In1, uint_16In2) \
asm volatile ( \
	"clr r2 \n\t" \
	"mul %B2, %B1 \n\t" \
	"movw r4, r0 \n\t" \
	"mul %A2, %A1 \n\t" \
	"add %A0, r0 \n\t" \
	"adc %B0, r1 \n\t" \
	"adc %C0, r4 \n\t" \
	"adc %D0, r5 \n\t" \
	"mul %B2, %A1 \n\t" \
	"add %B0, r0 \n\t" \
	"adc %C0, r1 \n\t" \
	"adc %D0, r2 \n\t" \
	"mul %A2, %B1 \n\t" \
	"add %B0, r0 \n\t" \
	"adc %C0, r1 \n\t" \
	"adc %D0, r2 \n\t" \
	"clr r1 \n\t" \
	: \
	"+r" (uint_32Acc) \
	: \
	"a" (uint_16In1), \
	"a" (uint_16In2) \
	: \
	"r2", "r4", "r5" \
)

#define FLAG_CLR_ICF1() TIFR1 |= (1<<ICF1)

/* globals */
extern version_t EEMEM version_eep;
extern version_t version;

extern event_t EEMEM event_eep;
extern event_t event;

extern uint8_t max_analog_sensors;

extern uint8_t EEMEM port_config_eep;
extern uint8_t port_config;

extern uint8_t EEMEM port_led_eep;
extern uint8_t port_led;

extern uint8_t EEMEM enabled_eep;
extern uint8_t enabled;

extern uint8_t EEMEM phy_to_log_eep[MAX_SENSORS];
extern uint8_t phy_to_log[MAX_SENSORS];

extern sensor_t EEMEM sensor_eep[MAX_SENSORS];
extern volatile sensor_t sensor[MAX_SENSORS];

extern volatile state_t state[MAX_SENSORS];

/* prototypes */
void setup_ar_uart(void);
void setup_adc(void);
void setup_pulse_input(void);

#endif
