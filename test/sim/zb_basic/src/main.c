/* zb_basic: basic instruction-level tests for Zbs, Zba, Zbb, Zbkb extensions.
 * Each wrapper uses inline asm to force the exact instruction to be emitted.
 * Expected values computed by hand / cross-checked with the spec.
 */

#include <stdint.h>
#include "tb_cxxrtl_io.h"

static inline void fail(uint32_t code) {
    tb_puts("[zb_basic] FAIL code=");
    tb_put_u32(code);
    tb_exit(code);
}

#define CHECK(expr, code) do { if (!(expr)) fail(code); } while (0)

/* ---- helpers ---- */
#define U32(x) ((uint32_t)(x))
#define S32(x) ((int32_t)(x))

/* ======================================================================== */
/* Zbs – Single-bit instructions                                             */
/* ======================================================================== */

static inline uint32_t rv_bset (uint32_t a, uint32_t b) { uint32_t r; asm("bset  %0,%1,%2":"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_bclr (uint32_t a, uint32_t b) { uint32_t r; asm("bclr  %0,%1,%2":"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_binv (uint32_t a, uint32_t b) { uint32_t r; asm("binv  %0,%1,%2":"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_bext (uint32_t a, uint32_t b) { uint32_t r; asm("bext  %0,%1,%2":"=r"(r):"r"(a),"r"(b)); return r; }

/* immediate variants: shamt encoded as literal in asm string */
#define rv_bseti(a, shamt) ({ uint32_t _r; asm("bseti %0,%1," #shamt :"=r"(_r):"r"((uint32_t)(a))); _r; })
#define rv_bclri(a, shamt) ({ uint32_t _r; asm("bclri %0,%1," #shamt :"=r"(_r):"r"((uint32_t)(a))); _r; })
#define rv_binvi(a, shamt) ({ uint32_t _r; asm("binvi %0,%1," #shamt :"=r"(_r):"r"((uint32_t)(a))); _r; })
#define rv_bexti(a, shamt) ({ uint32_t _r; asm("bexti %0,%1," #shamt :"=r"(_r):"r"((uint32_t)(a))); _r; })

static void test_zbs(void) {
    /* bset: set bit at position given by rs2[4:0] */
    CHECK(rv_bset(0x00000000u,  3u) == 0x00000008u,  1);  /* set bit 3          */
    CHECK(rv_bset(0x12345670u,  3u) == 0x12345678u,  2);  /* set already-clear  */
    CHECK(rv_bset(0x12345678u,  3u) == 0x12345678u,  3);  /* set already-set    */
    CHECK(rv_bset(0x00000000u, 31u) == 0x80000000u,  4);  /* set MSB            */

    /* bseti: immediate shamt */
    CHECK(rv_bseti(0x00000000u,  0) == 0x00000001u,  5);
    CHECK(rv_bseti(0x00000000u, 31) == 0x80000000u,  6);
    CHECK(rv_bseti(0xFFFFFFFFu,  7) == 0xFFFFFFFFu,  7);  /* noop if already 1  */

    /* bclr: clear bit */
    CHECK(rv_bclr(0xFFFFFFFFu,  0u) == 0xFFFFFFFEu,  8);
    CHECK(rv_bclr(0x12345678u,  3u) == 0x12345670u,  9);  /* bit 3 was set      */
    CHECK(rv_bclr(0x12345670u,  3u) == 0x12345670u, 10);  /* already clear      */
    CHECK(rv_bclr(0x80000000u, 31u) == 0x00000000u, 11);

    /* bclri: immediate */
    CHECK(rv_bclri(0xFFFFFFFFu, 31) == 0x7FFFFFFFu, 12);
    CHECK(rv_bclri(0x00000001u,  0) == 0x00000000u, 13);

    /* binv: invert bit */
    CHECK(rv_binv(0x00000000u,  4u) == 0x00000010u, 14);
    CHECK(rv_binv(0xFFFFFFFFu,  4u) == 0xFFFFFFEFu, 15);
    CHECK(rv_binv(0x12345678u,  3u) == 0x12345670u, 16);  /* bit 3 = 1, flip → 0 */

    /* binvi: immediate */
    CHECK(rv_binvi(0x00000000u, 15) == 0x00008000u, 17);
    CHECK(rv_binvi(0x00008000u, 15) == 0x00000000u, 18);  /* double flip        */

    /* bext: extract single bit (unsigned, result ∈ {0, 1}) */
    CHECK(rv_bext(0x12345678u,  3u) == 1u, 19);  /* bit 3 of 0x78 = 1  */
    CHECK(rv_bext(0x12345678u,  2u) == 0u, 20);  /* bit 2 of 0x78 = 0  */
    CHECK(rv_bext(0x80000000u, 31u) == 1u, 21);
    CHECK(rv_bext(0x7FFFFFFFu, 31u) == 0u, 22);

    /* bexti: immediate */
    CHECK(rv_bexti(0x12345678u,  4) == 1u, 23);  /* bit 4 of 0x78 = 1  */
    CHECK(rv_bexti(0x12345678u,  2) == 0u, 24);

    tb_puts("[zb_basic] Zbs PASS\n");
}

/* ======================================================================== */
/* Zba – Address operations                                                  */
/* ======================================================================== */

static inline uint32_t rv_sh1add(uint32_t a, uint32_t b) { uint32_t r; asm("sh1add %0,%1,%2":"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_sh2add(uint32_t a, uint32_t b) { uint32_t r; asm("sh2add %0,%1,%2":"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_sh3add(uint32_t a, uint32_t b) { uint32_t r; asm("sh3add %0,%1,%2":"=r"(r):"r"(a),"r"(b)); return r; }

static void test_zba(void) {
    /* sh1add rd, rs1, rs2 = rs2 + (rs1 << 1) */
    CHECK(rv_sh1add(  3u, 100u) == 106u,        25);  /* 100 + 6             */
    CHECK(rv_sh1add(  0u, 100u) == 100u,        26);  /* 100 + 0             */
    CHECK(rv_sh1add(  1u,   0u) ==   2u,        27);
    /* wrap-around */
    CHECK(rv_sh1add(0xFFFFFFFFu, 1u) == 0xFFFFFFFFu, 28); /* 1 + 0xFFFFFFFE  */

    /* sh2add rd, rs1, rs2 = rs2 + (rs1 << 2) */
    CHECK(rv_sh2add(  3u, 100u) == 112u,        29);  /* 100 + 12            */
    CHECK(rv_sh2add(  4u,   0u) ==  16u,        30);
    CHECK(rv_sh2add(0x40000000u, 0u) == 0u,     31);  /* overflow to 0       */

    /* sh3add rd, rs1, rs2 = rs2 + (rs1 << 3) */
    CHECK(rv_sh3add(  3u, 100u) == 124u,        32);  /* 100 + 24            */
    CHECK(rv_sh3add(  1u,   0u) ==   8u,        33);
    CHECK(rv_sh3add(0x20000000u, 0u) == 0u,     34);  /* overflow to 0       */

    tb_puts("[zb_basic] Zba PASS\n");
}

/* ======================================================================== */
/* Zbb – Basic bit manipulation                                             */
/* ======================================================================== */

static inline uint32_t rv_clz  (uint32_t a)                { uint32_t r; asm("clz   %0,%1"      :"=r"(r):"r"(a)); return r; }
static inline uint32_t rv_ctz  (uint32_t a)                { uint32_t r; asm("ctz   %0,%1"      :"=r"(r):"r"(a)); return r; }
static inline uint32_t rv_cpop (uint32_t a)                { uint32_t r; asm("cpop  %0,%1"      :"=r"(r):"r"(a)); return r; }
static inline uint32_t rv_andn (uint32_t a, uint32_t b)    { uint32_t r; asm("andn  %0,%1,%2"   :"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_orn  (uint32_t a, uint32_t b)    { uint32_t r; asm("orn   %0,%1,%2"   :"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_xnor (uint32_t a, uint32_t b)    { uint32_t r; asm("xnor  %0,%1,%2"   :"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_min  (int32_t  a, int32_t  b)    { uint32_t r; asm("min   %0,%1,%2"   :"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_minu (uint32_t a, uint32_t b)    { uint32_t r; asm("minu  %0,%1,%2"   :"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_max  (int32_t  a, int32_t  b)    { uint32_t r; asm("max   %0,%1,%2"   :"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_maxu (uint32_t a, uint32_t b)    { uint32_t r; asm("maxu  %0,%1,%2"   :"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_sextb(uint32_t a)                { uint32_t r; asm("sext.b %0,%1"     :"=r"(r):"r"(a)); return r; }
static inline uint32_t rv_sexth(uint32_t a)                { uint32_t r; asm("sext.h %0,%1"     :"=r"(r):"r"(a)); return r; }
static inline uint32_t rv_orcb (uint32_t a)                { uint32_t r; asm("orc.b  %0,%1"     :"=r"(r):"r"(a)); return r; }
static inline uint32_t rv_rev8 (uint32_t a)                { uint32_t r; asm("rev8   %0,%1"     :"=r"(r):"r"(a)); return r; }
static inline uint32_t rv_ror  (uint32_t a, uint32_t b)    { uint32_t r; asm("ror    %0,%1,%2"  :"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_rol  (uint32_t a, uint32_t b)    { uint32_t r; asm("rol    %0,%1,%2"  :"=r"(r):"r"(a),"r"(b)); return r; }
#define rv_rori(a, shamt) ({ uint32_t _r; asm("rori %0,%1," #shamt :"=r"(_r):"r"((uint32_t)(a))); _r; })

static void test_zbb(void) {
    /* clz: count leading zeros */
    CHECK(rv_clz(0x80000000u) ==  0u, 35);
    CHECK(rv_clz(0x40000000u) ==  1u, 36);
    CHECK(rv_clz(0x00010000u) == 15u, 37);
    CHECK(rv_clz(0x00000001u) == 31u, 38);
    CHECK(rv_clz(0x00000000u) == 32u, 39);

    /* ctz: count trailing zeros */
    CHECK(rv_ctz(0x00000001u) ==  0u, 40);
    CHECK(rv_ctz(0x00000002u) ==  1u, 41);
    CHECK(rv_ctz(0x00010000u) == 16u, 42);
    CHECK(rv_ctz(0x80000000u) == 31u, 43);
    CHECK(rv_ctz(0x00000000u) == 32u, 44);

    /* cpop: count set bits (popcount) */
    CHECK(rv_cpop(0x00000000u) ==  0u, 45);
    CHECK(rv_cpop(0x00000001u) ==  1u, 46);
    CHECK(rv_cpop(0x55555555u) == 16u, 47);
    CHECK(rv_cpop(0xFFFFFFFFu) == 32u, 48);
    CHECK(rv_cpop(0xAAAAAAAAu) == 16u, 49);
    CHECK(rv_cpop(0x0F0F0F0Fu) == 16u, 50);

    /* andn: rd = rs1 & ~rs2 */
    CHECK(rv_andn(0xFF00FF00u, 0x0F0F0F0Fu) == 0xF000F000u, 51);
    CHECK(rv_andn(0xFFFFFFFFu, 0xFFFFFFFFu) == 0x00000000u, 52);
    CHECK(rv_andn(0xFFFFFFFFu, 0x00000000u) == 0xFFFFFFFFu, 53);

    /* orn: rd = rs1 | ~rs2 */
    CHECK(rv_orn(0xFF00FF00u, 0x0F0F0F0Fu) == 0xFFF0FFF0u, 54);
    CHECK(rv_orn(0x00000000u, 0x00000000u) == 0xFFFFFFFFu, 55);
    CHECK(rv_orn(0x00000000u, 0xFFFFFFFFu) == 0x00000000u, 56);

    /* xnor: rd = ~(rs1 ^ rs2) */
    CHECK(rv_xnor(0xF0F0F0F0u, 0x0F0F0F0Fu) == 0x00000000u, 57);
    CHECK(rv_xnor(0xAAAAAAAAu, 0xAAAAAAAAu)  == 0xFFFFFFFFu, 58);
    CHECK(rv_xnor(0x00000000u, 0xFFFFFFFFu)  == 0x00000000u, 59);

    /* min/max: signed compare */
    CHECK(rv_min (S32(-1),  S32(1))  == U32(-1), 60);  /* -1 < 1 → -1        */
    CHECK(rv_min (S32( 1),  S32(-1)) == U32(-1), 61);
    CHECK(rv_min (S32(-5),  S32(-3)) == U32(-5), 62);  /* -5 < -3             */
    CHECK(rv_max (S32(-1),  S32(1))  == U32( 1), 63);
    CHECK(rv_max (S32(-5),  S32(-3)) == U32(-3), 64);

    /* minu/maxu: unsigned compare */
    CHECK(rv_minu(0x80000000u, 0x7FFFFFFFu) == 0x7FFFFFFFu, 65);
    CHECK(rv_minu(0x00000001u, 0x00000002u) == 0x00000001u, 66);
    CHECK(rv_maxu(0x80000000u, 0x7FFFFFFFu) == 0x80000000u, 67);
    CHECK(rv_maxu(0x00000001u, 0x00000002u) == 0x00000002u, 68);

    /* sext.b: sign-extend byte */
    CHECK(rv_sextb(0x0000007Fu) == 0x0000007Fu, 69);  /* positive → unchanged */
    CHECK(rv_sextb(0x00000080u) == 0xFFFFFF80u, 70);  /* MSB of byte = 1      */
    CHECK(rv_sextb(0x000000FFu) == 0xFFFFFFFFu, 71);
    CHECK(rv_sextb(0xABCD1234u) == 0x00000034u, 72);  /* upper bits ignored   */

    /* sext.h: sign-extend halfword */
    CHECK(rv_sexth(0x00007FFFu) == 0x00007FFFu, 73);
    CHECK(rv_sexth(0x00008000u) == 0xFFFF8000u, 74);
    CHECK(rv_sexth(0x0000FFFFu) == 0xFFFFFFFFu, 75);
    CHECK(rv_sexth(0xABCD1234u) == 0x00001234u, 76);  /* upper bits ignored   */

    /* orc.b: OR-combine bytes — each byte → 0xFF if any bit set, else 0x00 */
    CHECK(rv_orcb(0x01000001u) == 0xFF0000FFu, 77);
    CHECK(rv_orcb(0x00FF0000u) == 0x00FF0000u, 78);
    CHECK(rv_orcb(0x00000000u) == 0x00000000u, 79);
    CHECK(rv_orcb(0xFFFFFFFFu) == 0xFFFFFFFFu, 80);
    CHECK(rv_orcb(0x01020304u) == 0xFFFFFFFFu, 81);

    /* rev8: byte-reverse the 32-bit word */
    CHECK(rv_rev8(0x12345678u) == 0x78563412u, 82);
    CHECK(rv_rev8(0x00000001u) == 0x01000000u, 83);
    CHECK(rv_rev8(0xAABBCCDDu) == 0xDDCCBBAAu, 84);

    /* ror: rotate right */
    CHECK(rv_ror(0x12345678u,  8u) == 0x78123456u, 85);
    CHECK(rv_ror(0x12345678u,  4u) == 0x81234567u, 86);
    CHECK(rv_ror(0x12345678u,  0u) == 0x12345678u, 87);
    CHECK(rv_ror(0x80000000u,  1u) == 0x40000000u, 88);
    CHECK(rv_ror(0x00000001u,  1u) == 0x80000000u, 89);  /* wrap-around bit    */

    /* rol: rotate left */
    CHECK(rv_rol(0x12345678u,  8u) == 0x34567812u, 90);
    CHECK(rv_rol(0x12345678u,  4u) == 0x23456781u, 91);
    CHECK(rv_rol(0x80000000u,  1u) == 0x00000001u, 92);  /* wrap-around bit    */

    /* rori: rotate right immediate */
    CHECK(rv_rori(0x12345678u,  8) == 0x78123456u, 93);
    CHECK(rv_rori(0x00000001u,  1) == 0x80000000u, 94);

    tb_puts("[zb_basic] Zbb PASS\n");
}

/* ======================================================================== */
/* Zbkb – Bit manipulation for cryptography                                  */
/* ======================================================================== */

static inline uint32_t rv_pack  (uint32_t a, uint32_t b) { uint32_t r; asm("pack   %0,%1,%2":"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_packh (uint32_t a, uint32_t b) { uint32_t r; asm("packh  %0,%1,%2":"=r"(r):"r"(a),"r"(b)); return r; }
static inline uint32_t rv_brev8 (uint32_t a)             { uint32_t r; asm("brev8  %0,%1"   :"=r"(r):"r"(a));        return r; }

static void test_zbkb(void) {
    /* pack: rd[15:0] = rs1[15:0], rd[31:16] = rs2[15:0] */
    CHECK(rv_pack (0x12345678u, 0xABCDABCDu) == 0xABCD5678u, 95);
    CHECK(rv_pack (0xFFFFFFFFu, 0x00000000u) == 0x0000FFFFu, 96);
    CHECK(rv_pack (0x00000000u, 0xFFFFFFFFu) == 0xFFFF0000u, 97);
    CHECK(rv_pack (0x00001234u, 0x00005678u) == 0x56781234u, 98);

    /* packh: rd[7:0] = rs1[7:0], rd[15:8] = rs2[7:0], rd[31:16] = 0 */
    CHECK(rv_packh(0x12345678u, 0xABCDABCDu) == 0x0000CD78u, 99);
    CHECK(rv_packh(0xFFFFFFFFu, 0x00000000u) == 0x000000FFu, 100);
    CHECK(rv_packh(0x00000000u, 0xFFFFFFFFu) == 0x0000FF00u, 101);
    CHECK(rv_packh(0x000000ABu, 0x000000CDu) == 0x0000CDABu, 102);

    /* brev8: bit-reverse each byte independently */
    /* 0x01=0000_0001 → 1000_0000=0x80 ; each byte independently */
    CHECK(rv_brev8(0x01020408u) == 0x80402010u, 103);
    /* 0xFF=1111_1111 → 1111_1111 ; 0x00=0→0 */
    CHECK(rv_brev8(0xFF0000FFu) == 0xFF0000FFu, 104);
    /* 0xAA=1010_1010 → 0101_0101=0x55 */
    CHECK(rv_brev8(0xAAAAAAAAu) == 0x55555555u, 105);

    tb_puts("[zb_basic] Zbkb PASS\n");
}

/* ======================================================================== */

int main(void) {
    tb_puts("[zb_basic] start\n");

    test_zbs();
    test_zba();
    test_zbb();
    test_zbkb();

    tb_puts("[zb_basic] PASS\n");
    return 0;
}
