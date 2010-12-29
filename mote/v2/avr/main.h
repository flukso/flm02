#define SPI_NO_OP_1		1
#define SPI_NO_OP_2		2
#define SPI_START_TX		4
#define SPI_TRANSMIT		8
#define SPI_HIGH_HEX		16
#define SPI_TO_FROM_UART	32
#define SPI_NEW_CTRL_MSG	64

#define SPI_END_OF_TX			0x00
#define SPI_END_OF_MESSAGE		'.'
#define SPI_FORWARD_TO_UART_PORT	'u'
#define SPI_FORWARD_TO_CTRL_PORT	'l' // 'l'ocal port

struct event_struct {
	uint16_t wdt;
	uint16_t brown_out;
};

struct sensor_struct {
	uint32_t counter;
	uint16_t meterconst;
};

# define WATT 1000000000
# define SECOND 665 // 666Hz - 1

#define STATE_PULSE		1
#define STATE_SKIP		2
#define STATE_POWER_CALC	4
#define STATE_POWER		8

struct state_struct {
	uint8_t  flags;

	uint32_t nano;
	uint32_t nano_start;
	uint32_t nano_end;
	uint8_t  pulse_count;
	uint8_t  pulse_count_final;

	uint32_t power;
	uint32_t timestamp;
};

struct time_struct {
	uint8_t	 skip;
	uint32_t ms;
};

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

void register_pulse(volatile struct sensor_struct *psensor, volatile struct state_struct *pstate);
void setup_datastructs(void);
void setup_pulse_input(void);
void setup_adc(void);
void setup_timer1(void);
void setup_analog_comparator(void);
void calculate_power(volatile struct state_struct *pstate);
