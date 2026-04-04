/* fpga test: blink LEDs 2 and 3 while button 0 is held.
 *
 * Peripheral addresses (RVTop APB bus, apbAddrWidth=16):
 *   GPIOA (buttons) = 0x0201_1000  (APB offset 0x1000 within bridge at 0x0201_0000)
 *   GPIOB (LEDs)    = 0x0201_2000  (APB offset 0x2000)
 *
 * Apb3Gpio register map (from SpinalHDL generated Verilog):
 *   +0x0  val  : input value (read)
 *   +0x4  out  : output drive value (write)
 *   +0x8  oe   : output enable mask  (write, 1 = output)
 *
 * Button 0 = bit 0 of GPIOA.val (active high, no debounce needed for sim).
 * LED 2    = bit 2 of GPIOB.out
 * LED 3    = bit 3 of GPIOB.out
 *
 * Blink rate: LEDs toggle every (1 << BLINK_CLOG2) loop iterations.
 * Keep BLINK_CLOG2 low (e.g. 2) for fast toggling in simulation.
 * On real FPGA at 80 MHz: use BLINK_CLOG2=23 for ~0.5 Hz blink.
 */
#include <stdint.h>

#define GPIOA_BASE  0x02011000u
#define GPIOB_BASE  0x02012000u

typedef struct {
    volatile uint32_t val;   /* +0: input  value */
    volatile uint32_t out;   /* +4: output drive */
    volatile uint32_t oe;    /* +8: output enable */
} gpio_t;

#define mm_gpioa  ((gpio_t *)GPIOA_BASE)
#define mm_gpiob  ((gpio_t *)GPIOB_BASE)

/* Blink divider (power of 2): toggle period = (1<<BLINK_CLOG2) loop iters.
 * 2 → very fast (good for simulation); 23 → ~0.5 Hz at 80 MHz on FPGA.
 * Override at compile time with -DBLINK_CLOG2=2 for simulation. */
#ifndef BLINK_CLOG2
#define BLINK_CLOG2  20u
#endif 

int main(void)
{
    /* Configure LED[2] and LED[3] as outputs; leave LED[0] and LED[1] alone. */
    mm_gpiob->oe  = (1u << 2) | (1u << 3);
    mm_gpiob->out = 0u;

    uint32_t counter = 0u;

    while (1) {
        if (mm_gpioa->val & 1u) {
            /* Button 0 pressed: blink LED 2 and 3 */
            counter++;
            uint32_t blink = (counter >> BLINK_CLOG2) & 1u;
            mm_gpiob->out = blink ? ((1u << 2) | (1u << 3)) : 0u;
        } else {
            /* Button 0 released: LEDs off */
            counter = 0u;
            mm_gpiob->out = 0u;
        }
    }
    return 0;
}
