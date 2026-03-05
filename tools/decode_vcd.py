#!/usr/bin/env python3
insn1 = 0x41018273
opcode = insn1 & 0x7f
funct3 = (insn1 >> 12) & 0x7
rd = (insn1 >> 7) & 0x1f
rs1 = (insn1 >> 15) & 0x1f
csr = (insn1 >> 20) & 0xfff
print(f'Instruction 1: 0x{insn1:08x}')
print(f'  opcode=0x{opcode:02x} funct3={funct3} rd=x{rd} rs1=x{rs1} csr/funct12=0x{csr:03x}')
if opcode == 0x73 and funct3 == 0:
    print(f'  -> PRIV funct12=0x{csr:03x} — NOT a standard instruction, should be ILLEGAL')

insn2 = 0x17127013
opcode2 = insn2 & 0x7f
funct3_2 = (insn2 >> 12) & 0x7
rd2 = (insn2 >> 7) & 0x1f
rs1_2 = (insn2 >> 15) & 0x1f
imm = (insn2 >> 20)
print(f'\nInstruction 2: 0x{insn2:08x}')
print(f'  opcode=0x{opcode2:02x} funct3={funct3_2} rd=x{rd2} rs1=x{rs1_2} imm=0x{imm:03x}')
if opcode2 == 0x13:
    ops = {0:'ADDI', 1:'SLLI', 2:'SLTI', 3:'SLTIU', 4:'XORI', 5:'SRLI/SRAI', 6:'ORI', 7:'ANDI'}
    print(f'  -> {ops.get(funct3_2,"?")} x{rd2}, x{rs1_2}, 0x{imm:x}')
