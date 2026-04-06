/* -------------------------------------------------------------------------
 * loader.S — Minimal boot stub for Linux on RV1 simulation
 *
 * Loaded at 0x80000040 (after 64‑byte boot‑pad).
 * Sets up  a0 = hartid = 0
 *          a1 = physical address of DTB
 * then jumps to the kernel entry point (immediately following this stub).
 *
 * Memory layout (assembled by Makefile):
 *   0x80000000 +0x000  64-byte boot-pad (zeros)
 *   0x80000040 +0x040  this loader  (~32 bytes)
 *   0x80000800 +0x800  Linux kernel Image
 *   DTB_ADDR           DTB blob (set by linker symbol)
 * -------------------------------------------------------------------------*/

.section .text.init
.global _start
_start:
    li   a0, 0                  /* hartid = 0                         */
    la   a1, _dtb_start         /* a1 = &DTB  (symbol injected by ld) */
    li   t0, 0x80000800         /* kernel entry (fixed offset)        */
    jr   t0                     /* jump to kernel                     */

/* Symbols _dtb_start is provided by the linker script or by
   objcopy --add-section / --set-section-flags at link time. */
