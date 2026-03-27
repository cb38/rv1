#ifndef _SOC_IO_H
#define _SOC_IO_H

#include <stdint.h>

/*
 * SoC peripheral memory map (CPU periph_axi, addr[31:17] == 0x0100)
 *
 *  0x0200_0000 .. 0x0200_0FFF   CLINT (mtime / mtimecmp)
 *  0x0201_0000 .. 0x0201_0FFF   UART
 *  0x0201_1000 .. 0x0201_1FFF   GPIO-A  (buttons, 4-bit input)
 *  0x0201_2000 .. 0x0201_2FFF   GPIO-B  (LEDs,    4-bit output)
 *
 * Apb3Gpio register offsets (byte address within the 4 KiB slave window):
 *   +0x00  value      R    current pin state (post-synchroniser)
 *   +0x04  out        RW   output data register
 *   +0x08  oe         RW   output-enable register (1 = drive output)
 */

typedef struct {
    volatile uint32_t value;  /* +0x00 */
    volatile uint32_t out;    /* +0x04 */
    volatile uint32_t oe;     /* +0x08 */
} gpio_hw_t;

#define GPIOA_BASE  0x02011000u
#define GPIOB_BASE  0x02012000u

#define mm_gpioa  ((gpio_hw_t *const)GPIOA_BASE)
#define mm_gpiob  ((gpio_hw_t *const)GPIOB_BASE)

/* Convenience bit masks for buttons and LEDs */
#define BTN0  (1u << 0)
#define BTN1  (1u << 1)
#define BTN2  (1u << 2)
#define BTN3  (1u << 3)

#define LED0  (1u << 0)
#define LED1  (1u << 1)
#define LED2  (1u << 2)
#define LED3  (1u << 3)

#endif /* _SOC_IO_H */
