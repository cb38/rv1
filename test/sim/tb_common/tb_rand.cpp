#include "tb.h"

#include <stdint.h>

// TB pseudorandom number generator, using xoroshiro256++ -- original
// copyright notice follows.

/*  Written in 2019 by David Blackman and Sebastiano Vigna (vigna@acm.org)

To the extent possible under law, the author has dedicated all copyright
and related and neighboring rights to this software to the public domain
worldwide.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. */

/* This is xoshiro256++ 1.0, one of our all-purpose, rock-solid generators.
   It has excellent (sub-ns) speed, a state (256 bits) that is large
   enough for any parallel application, and it passes all tests we are
   aware of.

   For generating just floating-point numbers, xoshiro256+ is even faster.

   The state must be seeded so that it is not everywhere zero. If you have
   a 64-bit seed, we suggest to seed a splitmix64 generator and use its
   output to fill s. */

static inline uint64_t rotl(const uint64_t x, int k) {
	return (x << k) | (x >> (64 - k));
}

uint32_t tb_top::rand(void) {
	const uint64_t result = rotl(rand_state[0] + rand_state[3], 23) + rand_state[0];

	const uint64_t t = rand_state[1] << 17;

	rand_state[2] ^= rand_state[0];
	rand_state[3] ^= rand_state[1];
	rand_state[1] ^= rand_state[2];
	rand_state[0] ^= rand_state[3];

	rand_state[2] ^= t;

	rand_state[3] = rotl(rand_state[3], 45);

	return result >> 32;
}

void tb_top::seed_rand(const uint8_t *data, size_t len) {
	// Initial state must not be all-zeroes
	for (unsigned int i = 0; i < 4; ++i) {
		rand_state[i] = 0xf005ba11u + i;
	}
	// Pour + stir method: XOR data in one bit at a time, with a xoroshiro
	// permutation between each.
	for (size_t i = 0; i < 8u * len; ++i) {
		if (data[i / 8u] & (1u << (i % 8u))) {
			rand_state[0] ^= 1u;
		}
		(void)rand();
	}
}
