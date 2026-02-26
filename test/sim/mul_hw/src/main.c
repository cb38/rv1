#include <stdint.h>
#include "tb_cxxrtl_io.h"

static inline void fail(uint32_t code) {
    tb_puts("[mul_hw] FAIL\n");
    tb_put_u32(code);
    tb_exit(code);
}

static inline uint32_t rv_mul(uint32_t a, uint32_t b) {
    uint32_t rd;
    asm volatile("mul %0, %1, %2" : "=r"(rd) : "r"(a), "r"(b));
    return rd;
}

static inline uint32_t rv_mulh(int32_t a, int32_t b) {
    uint32_t rd;
    asm volatile("mulh %0, %1, %2" : "=r"(rd) : "r"(a), "r"(b));
    return rd;
}

static inline uint32_t rv_mulhsu(int32_t a, uint32_t b) {
    uint32_t rd;
    asm volatile("mulhsu %0, %1, %2" : "=r"(rd) : "r"(a), "r"(b));
    return rd;
}

static inline uint32_t rv_mulhu(uint32_t a, uint32_t b) {
    uint32_t rd;
    asm volatile("mulhu %0, %1, %2" : "=r"(rd) : "r"(a), "r"(b));
    return rd;
}

static inline uint32_t rv_div(int32_t a, int32_t b) {
    uint32_t rd;
    asm volatile("div %0, %1, %2" : "=r"(rd) : "r"(a), "r"(b));
    return rd;
}

static inline uint32_t rv_divu(uint32_t a, uint32_t b) {
    uint32_t rd;
    asm volatile("divu %0, %1, %2" : "=r"(rd) : "r"(a), "r"(b));
    return rd;
}

static inline uint32_t rv_rem(int32_t a, int32_t b) {
    uint32_t rd;
    asm volatile("rem %0, %1, %2" : "=r"(rd) : "r"(a), "r"(b));
    return rd;
}

static inline uint32_t rv_remu(uint32_t a, uint32_t b) {
    uint32_t rd;
    asm volatile("remu %0, %1, %2" : "=r"(rd) : "r"(a), "r"(b));
    return rd;
}

int main(void) {
    tb_puts("[mul_hw] start\n");

    if (rv_mul(0x12345678u, 0x11111111u) != 0x652fb5f8u) fail(1u);
    if (rv_mul(0xffffffffu, 0x00000003u) != 0xfffffffdu) fail(2u);

    if (rv_mulh((int32_t)0x80000000u, (int32_t)0x80000000u) != 0x40000000u) fail(3u);
    if (rv_mulh((int32_t)0x80000003u, (int32_t)0x00010005u) != 0xffff7ffdu) fail(4u);

    if (rv_mulhsu((int32_t)0x80000000u, 0x80000000u) != 0xc0000000u) fail(5u);
    if (rv_mulhsu((int32_t)0x80000003u, 0x00010005u) != 0xffff7ffdu) fail(6u);

    if (rv_mulhu(0x80000000u, 0x80000000u) != 0x40000000u) fail(7u);
    if (rv_mulhu(0xffffffffu, 0x00000003u) != 0x00000002u) fail(8u);
    tb_puts("[mul_hw] PASS\n");
    if (rv_div((int32_t)20, (int32_t)3) != 6u) fail(9u);
    if (rv_div((int32_t)-20, (int32_t)3) != 0xfffffffau) fail(10u);
    if (rv_div((int32_t)0x80000000u, (int32_t)-1) != 0x80000000u) fail(11u);
    if (rv_div((int32_t)7, (int32_t)0) != 0xffffffffu) fail(12u);

    if (rv_divu(20u, 3u) != 6u) fail(13u);
    if (rv_divu(0xffffffffu, 3u) != 0x55555555u) fail(14u);
    if (rv_divu(7u, 0u) != 0xffffffffu) fail(15u);

    if (rv_rem((int32_t)20, (int32_t)3) != 2u) fail(16u);
    if (rv_rem((int32_t)-20, (int32_t)3) != 0xfffffffeu) fail(17u);
    if (rv_rem((int32_t)0x80000000u, (int32_t)-1) != 0u) fail(18u);
    if (rv_rem((int32_t)7, (int32_t)0) != 7u) fail(19u);

    if (rv_remu(20u, 3u) != 2u) fail(20u);
    if (rv_remu(0xffffffffu, 3u) != 0u) fail(21u);
    if (rv_remu(7u, 0u) != 7u) fail(22u);

    tb_puts("[div_hw] PASS\n");
    return 0;
}
