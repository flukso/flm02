#include <stdint.h>

// hex to binary/byte decoding
static inline void htob(uint8_t high_hex, uint8_t low_hex, uint8_t *pbyte)
{
	*pbyte = (high_hex > 0x40) ? (high_hex & 0x0F) + 9 : high_hex & 0x0F;
	*pbyte = *pbyte << 4;
	*pbyte |= (low_hex > 0x40) ? (low_hex & 0x0F) + 9 : low_hex & 0x0F;
}

// binary/byte to hex encoding
static inline void btoh(uint8_t byte, uint8_t *phigh_hex, uint8_t *plow_hex)
{
	*plow_hex = byte & 0x0F;
	*plow_hex = (*plow_hex > 0x09) ? *plow_hex - 9 + 0x60 : *plow_hex + 0x30;
	*phigh_hex = (byte & 0xF0) >> 4;
	*phigh_hex = (*phigh_hex > 0x09) ? *phigh_hex - 9 + 0x60 : *phigh_hex + 0x30;
}
