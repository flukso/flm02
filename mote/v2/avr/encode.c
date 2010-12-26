#include <stdint.h>

// hex to binary/byte decoding
uint8_t htob(uint16_t hex)
{
	uint8_t low_hex = (uint8_t) hex;
	uint8_t high_hex = (uint8_t) (hex >> 8);
	uint8_t byte;

	byte = (high_hex > 0x40) ? (high_hex & 0x0F) + 9 : high_hex & 0x0F;
	byte = byte << 4;
	byte |= (low_hex > 0x40) ? (low_hex & 0x0F) + 9 : low_hex & 0x0F;
	return byte;
}

// binary/byte to hex encoding
uint16_t btoh(uint8_t byte)
{
	uint8_t low_nibble = (byte & 0x0F);
	uint8_t high_nibble = (byte & 0xF0) >> 4;
	uint16_t hex;

	hex = (high_nibble > 0x09) ? high_nibble - 9 + 0x60 : high_nibble + 0x30;
	hex = hex << 8;
	hex |= (low_nibble > 0x09) ? low_nibble - 9 + 0x60 : low_nibble + 0x30;
	return hex;
}
