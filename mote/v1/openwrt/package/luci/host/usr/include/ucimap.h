/*
 * ucimap - library for mapping uci sections into data structures
 * Copyright (C) 2008 Felix Fietkau <nbd@openwrt.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2
 * as published by the Free Software Foundation
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */
#include <stdbool.h>
#include "uci_list.h"
#include "uci.h"

#ifndef ARRAY_SIZE
#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))
#endif

#define BITFIELD_SIZE(_fields) (((_fields) / 8) + 1)

#define CLR_BIT(_name, _bit) do { \
	_name[(_bit) / 8] &= ~(1 << ((_bit) % 8)); \
} while (0)

#define SET_BIT(_name, _bit) do { \
	_name[(_bit) / 8] |=  (1 << ((_bit) % 8)); \
} while (0)

#define TEST_BIT(_name, _bit) \
	(_name[(_bit) / 8] & (1 << ((_bit) % 8)))

#define OPTMAP_OPTION(_maptype, _type, _field, ...) \
	{ \
		.type = _maptype, \
		.name = #_field, \
		.offset = offsetof(_type, _field), \
		__VA_ARGS__  \
	}

struct uci_sectmap;
struct uci_optmap;

struct uci_map {
	struct uci_sectmap **sections;
	unsigned int n_sections;
	struct list_head sdata;
	struct list_head fixup;

	void *priv; /* user data */
};

enum ucimap_type {
	/* types */
	UCIMAP_SIMPLE   = 0x00,
	UCIMAP_LIST     = 0x10,
	UCIMAP_TYPE     = 0xf0, /* type mask */

	/* subtypes */
	UCIMAP_STRING   = 0x0,
	UCIMAP_BOOL     = 0x1,
	UCIMAP_INT      = 0x2,
	UCIMAP_SECTION  = 0x3,
	UCIMAP_SUBTYPE  = 0xf, /* subtype mask */
};

union uci_datamap {
	int i;
	bool b;
	const char *s;
	void *section;
	struct list_head list;
};

struct uci_listmap {
	struct list_head list;
	union uci_datamap data;
};

struct uci_sectmap {
	/* type string for the uci section */
	const char *type;

	/* length of the struct to map into */
	unsigned int alloc_len;

	/* give the caller time to initialize the preallocated struct */
	int (*init_section)(struct uci_map *map, void *section, struct uci_section *s);

	/* pass the fully processed struct to the callback after the section end */
	int (*add_section)(struct uci_map *map, void *section);

	/* let the callback clean up its own stuff in the section */
	int (*free_section)(struct uci_map *map, void *section);

	/* list of option mappings for this section */
	struct uci_optmap *options;
	unsigned int n_options;
};

struct uci_optmap {
	unsigned int offset;
	const char *name;
	enum ucimap_type type;
	union {
		struct {
			int base;
			int min;
			int max;
		} i;
		struct {
			int maxlen;
		} s;
		struct uci_sectmap *sm;
	} data;
};

extern int ucimap_init(struct uci_map *map);
extern void ucimap_cleanup(struct uci_map *map);
extern void ucimap_set_changed(void *section, void *field);
extern int ucimap_store_section(struct uci_map *map, struct uci_package *p, void *section);
extern void ucimap_parse(struct uci_map *map, struct uci_package *pkg);

