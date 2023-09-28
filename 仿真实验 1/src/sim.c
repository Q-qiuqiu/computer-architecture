#include <stdio.h>
#include "shell.h"

    uint32_t extract_op(uint32_t inst) { return inst >> 26; }  //指令，右移 26 位,
    uint32_t extract_rs(uint32_t inst) { return (inst >> 21) & 0x1f; }  //rs，右移 21 位，rs 寄存器号将位于右数第 6-10 位，与0x1f做与操作获取这5位的值
    uint32_t extract_rt(uint32_t inst) { return (inst >> 16) & 0x1f; }  //rt，右移 16 位，与操作
    uint32_t extract_rd(uint32_t inst) { return (inst >> 11) & 0x1f; }  //rd,右移 11 位，与操作
    uint32_t extract_target(uint32_t inst) { return inst & 0x3ffffff; }  //目标地址26位，与操作
    uint32_t extract_imm(uint32_t inst) { return inst & 0xffff; }  //保留右数前 16 位的值，与操作，得立即数
    uint32_t extract_shamt(uint32_t inst) { return (inst >> 6) & 0x1f; }  //shamt，右移 6 位，与操作
    uint32_t extract_funct(uint32_t inst) { return inst & 0x3f; }  //funct，与操作
    
    /// Sign extend the 16 bits immediate
    uint32_t sign_ext(uint32_t imm) {
        int32_t signed_imm = *((int16_t*)&imm);
        uint32_t extended_imm = *((uint32_t*)&signed_imm);
        return extended_imm;
}

    /// Sign extend a byte to 32 bits
    uint32_t sign_ext_byte(uint8_t imm) {
        int32_t signed_imm = *((int8_t*)&imm);
        uint32_t extended_imm = *((uint32_t*)&signed_imm);
        return extended_imm;
}
    uint32_t sign_ext_half(uint16_t imm) {
        int32_t signed_imm = *((int16_t*)&imm);
        uint32_t extended_imm = *((uint32_t*)&signed_imm);
        return extended_imm;
}

    uint32_t zero_ext(uint32_t imm) { return imm; }

    uint32_t zero_ext_byte(uint8_t imm) { return imm; }

    uint32_t zero_ext_half(uint16_t imm) { return imm; }

    void process_instruction() {
    /* execute one instruction here. You should use CURRENT_STATE and modify
     * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
     * access memory. */
    uint32_t inst = mem_read_32(CURRENT_STATE.PC);

    printf("当前指令为: 0x%08x\n", inst);

    uint32_t op = extract_op(inst);
    uint32_t rs = extract_rs(inst);
    uint32_t rt = extract_rt(inst);
    uint32_t imm = extract_imm(inst);
    uint32_t rd = extract_rd(inst);
    uint32_t shamt = extract_shamt(inst);
    uint32_t funct = extract_funct(inst);

    switch (op) {
        case 0x0: {//R型指令
            switch (funct) {
                case 0x0: {//通过后六位的hex判断是什么指令
                    // SLL，sll rd,rs1,rs2 ：将寄存器rs1的值左移寄存器rs2的值这么多位，并写入寄存器rd。
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << shamt;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x2: {
                    // SRL，srl rd,rs1,rs2 ：将寄存器rs1的值逻辑右移寄存器rs2的值这么多位，并写入寄存器rd。
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> shamt;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x3: {
                    // SRA，sra rd,rs1,rs2 ：将寄存器rs1的值算数右移寄存器rs2的值这么多位，并写入寄存器rd
                    int32_t val = *((int32_t*)&CURRENT_STATE.REGS[rt]);//取地址转换为int32_t有符号整数类型
                    val = val >> shamt;
                    NEXT_STATE.REGS[rd] = val;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x4: {
                    // SLLV，将rt中的值逻辑左移，左移位数由rs中的0-4bit确定，空出来的用0填充，结果存在rd中
                    uint32_t shamt = CURRENT_STATE.REGS[rs] & 0x1f;
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << shamt;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x6: {
                    // SRLV，将rt中的值逻辑右移，右移位数由rs中的0-4bit确定，空出来的用0填充，结果存在rd中
                    uint32_t shamt = CURRENT_STATE.REGS[rs] & 0x1f;
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> shamt;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x7: {
                    // SRAV，将rt中的值算数右移，右移位数由rs中的0-4bit确定，空出来的用rt[31]填充，结果存在rd中
                    int32_t val = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                    uint32_t shamt = CURRENT_STATE.REGS[rs] & 0x1f;
                    val = val >> shamt;
                    NEXT_STATE.REGS[rd] = val;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x8: {
                    // JR，jr ra：jump &return ra记录返回地址 
                    NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
                    break;
                }
                case 0x9: {
                    // JALR，jalr rd,offset(rs) ：把下一条指令的地址存到rd中，然后跳转到rs+offset地址处的指令继续执行
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + 4;
                    NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
                    break;
                }
                case 0xc: {
                    // SYSCALL，在操作系统中触发系统调用
                    if (CURRENT_STATE.REGS[2] == 0x0a) {
                        RUN_BIT = FALSE;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x10: {//HI存放结果的高字节，LO存放结果的低字节。
                    // MFHI，将特殊寄存器HI的值赋给地址为rd的通用寄存器
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.HI;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x11: {
                    // MTHI，将地址为rs的通用寄存器的值赋给特殊寄存器HI
                    NEXT_STATE.HI = CURRENT_STATE.REGS[rs];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x12: {
                    // MFLO，将特殊寄存器LO的值赋给地址为rd的通用寄存器
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.LO;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x13: {
                    // MTLO，将地址为rs的通用寄存器的值赋给特殊寄存器LO
                    NEXT_STATE.LO = CURRENT_STATE.REGS[rs];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x18: {
                    // MULT，有符号整数的乘法
                    int64_t lhs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                    int64_t rhs = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                    int64_t product = lhs * rhs;
                    uint64_t uint_product = *((uint32_t*)&product);
                    NEXT_STATE.HI =
                        (uint32_t)((uint_product >> 32) & 0xffffffff);
                    NEXT_STATE.LO = (uint32_t)(uint_product & 0xffffffff);
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x19: {
                    // MULTU，用于将两个无符号整数寄存器中的值相乘
                    uint64_t lhs = CURRENT_STATE.REGS[rs];
                    uint64_t rhs = CURRENT_STATE.REGS[rt];
                    uint64_t product = lhs * rhs;

                    NEXT_STATE.HI = (uint32_t)((product >> 32) & 0xffffffff);
                    NEXT_STATE.LO = (uint32_t)(product & 0xffffffff);
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x1a: {
                    // DIV，有符号整数除法
                    int32_t lhs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                    int32_t rhs = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                    NEXT_STATE.LO = lhs / rhs;
                    NEXT_STATE.HI = lhs % rhs;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x1b: {
                    // DIVU，无符号整数除法
                    uint32_t lhs = CURRENT_STATE.REGS[rs];
                    uint32_t rhs = CURRENT_STATE.REGS[rt];
                    NEXT_STATE.LO = lhs / rhs;
                    NEXT_STATE.HI = lhs % rhs;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x20: {
                    // ADD，add rd,rs1,rs2 ：将寄存器rs1与rs2的值相加并写入寄存器rd
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x21: {
                    // ADDU，无符号数的加法
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x22: {
                    // SUB，sub rd,rs1,rs2 ：将寄存器rs1与rs2的值相减并写入寄存器rd。
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x23: {
                    // SUBU，无符号数的减法
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x24: {
                    // AND，and rd,rs1,rs2 ：将寄存器rs1与rs2的值按位与并写入寄存器rd。
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] & CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x25: {
                    // OR，or rd,rs1,rs2 ：将寄存器rs1与rs2的值按位或并写入寄存器rd。
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x26: {
                    // XOR，xor rd,rs1,rs2 ：将寄存器rs1与rs2的值按位异或并写入寄存器rd。
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] ^ CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x27: {
                    // NOR，异或
                    NEXT_STATE.REGS[rd] =
                        ~(CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt]);
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x2a: {
                    // SLT，有符号数比较小于时置位
                    int32_t lhs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                    int32_t rhs = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                    NEXT_STATE.REGS[rd] = (lhs < rhs) ? 1 : 0;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x2b: {
                    // SLTU，无符号数比较小于时置位
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] < CURRENT_STATE.REGS[rt] ? 1 : 0;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                default: {
                    printf("Unknown instruction: 0x%x\n", inst);
                    break;
                }
            }
            break;
        }
        //I型指令
        case 0x8: {
            // ADDI，addi rd,rs1,imm ：将寄存器rs1的值与立即数imm相加并存入寄存器rd。
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + sign_ext(imm);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x9: {
            // ADDIU，加立即数指令，不受溢出限制
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + sign_ext(imm);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0xc: {
            // ANDI，andi rd,rs1,imm ：将寄存器rs1的值与立即数imm的值按位与并写入寄存器rd。
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] & zero_ext(imm);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0xd: {
            // ORI，ori rd,rs1,imm ：将寄存器rs1的值与立即数imm的值按位或并写入寄存器rd。
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] | zero_ext(imm);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0xe: {
            // XORI，xori rd,rs1,imm ：将寄存器rs1的值与立即数imm的值按位异或并写入寄存器rd。
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] ^ zero_ext(imm);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x4: {
            // BEQ，beq rs1,rs2,lable ：若rs1的值等于rs2的值，程序跳转到lable处继续执行

            uint32_t offset = sign_ext(imm) << 2;

            if (CURRENT_STATE.REGS[rs] == CURRENT_STATE.REGS[rt]) {
                NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
            } else {
                NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            }
            break;
        }
        //I型指令
        case 0x1: {
            uint32_t offset = sign_ext(imm) << 2;

            switch (rt) {
                case 0x0: {
                    // BLTZ，即当寄存器的值小于零时进行分支
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x10: {
                    // BLTZAL，带条件的相对PC子程序调用 if(rs<0) lable()
                    NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x1: {
                    // BGEZ，大于等于0跳转 if (rs >= 0) goto lable
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) == 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x11: {
                    // BGEZAL，大于等于0跳转 if (rs >= 0) goto lable
                    NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) == 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
            }
            break;
        }
        case 0x5: {
            // BNE，不等则跳转 if (rs = rt) goto lable

            uint32_t offset = sign_ext(imm) << 2;

            printf("BNE: offset: %d, rs: %d, rt: %d\n", offset, rs, rt);

            printf("rs: 0x%08x\n", CURRENT_STATE.REGS[rs]);
            printf("rt: 0x%08x\n", CURRENT_STATE.REGS[rt]);

            if (CURRENT_STATE.REGS[rs] != CURRENT_STATE.REGS[rt]) {
                NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
            } else {
                NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            }
            break;
        }
        case 0x6: {
            // BLEZ，小于等于0跳转  if (rs <= 0) goto lable

            uint32_t offset = sign_ext(imm) << 2;

            if (rt == 0) {
                if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0 ||
                    CURRENT_STATE.REGS[rs] == 0) {
                    NEXT_STATE.PC = CURRENT_STATE.PC + offset + 4;
                } else {
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                }
            } else {
                // Illegal instruction
                printf("Illegal rt in BLEZ.\n");
            }
            break;
        }
        case 0x7: {
            // BGTZ，大于0跳转 if (rs > 0) goto lable
            uint32_t offset = sign_ext(imm) << 2;

            printf("BGTZ: offset: 0x%08x, rs: %d, rt: %d, pc: 0x%08x\n", offset,
                   rs, rt, CURRENT_STATE.PC);

            if (rt == 0) {
                if ((CURRENT_STATE.REGS[rs] & 0x80000000) == 0 &&
                    CURRENT_STATE.REGS[rs] != 0) {
                    NEXT_STATE.PC = CURRENT_STATE.PC + offset + (uint32_t)4;
                    printf("PC: 0x%08x\n", NEXT_STATE.PC);
                } else {
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                }
            } else {
                // Illegal instruction
                printf("Illegal rt in BGTZ.\n");
            }
            break;
        }
        case 0x2: {
            // J，绝对跳转
            uint32_t target = extract_target(inst);
            NEXT_STATE.PC = (CURRENT_STATE.PC & 0xf0000000) | (target << 2);
            break;
        }
        case 0x3: {
            // JAL，子程序调用
            uint32_t target = extract_target(inst);
            NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
            NEXT_STATE.PC = (CURRENT_STATE.PC & 0xf0000000) | (target << 2);
            break;
        }
        case 0xf: {
            // LUI，将一个立即数的高 16 位加载到一个寄存器中
            if (rs == 0) {
                NEXT_STATE.REGS[rt] = imm << 16;
                NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            } else {
                // Illegal instruction
                 printf("Illegal instruction\n");
            }
            break;
        }
        case 0x20: {
            // LB，8位加载
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint8_t byte = mem_read_32(addr) & 0xff;

            NEXT_STATE.REGS[rt] = sign_ext_byte(byte);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x24: {
            // LBU，8位加载，结果0扩展
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint8_t byte = mem_read_32(addr) & 0xff;

            NEXT_STATE.REGS[rt] = zero_ext_byte(byte);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x21: {
            // LH，加载16位
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint16_t half = mem_read_32(addr) & 0xffff;

            NEXT_STATE.REGS[rt] = sign_ext_half(half);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x25: {
            // LHU，加载16位，结果0扩展
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint16_t half = mem_read_32(addr) & 0xffff;

            NEXT_STATE.REGS[rt] = zero_ext_half(half);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x23: {
            // LW，加载32位
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            NEXT_STATE.REGS[rt] = mem_read_32(addr);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x28: {
            // SB，将一个寄存器中的字节数据存储到内存

            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint32_t val = (mem_read_32(addr) & 0xffffff00) |
                           (CURRENT_STATE.REGS[rt] & 0xff);

            mem_write_32(addr, val);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x29: {
            // SH，存16位

            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint32_t val = (mem_read_32(addr) & 0xffff0000) |
                           (CURRENT_STATE.REGS[rt] & 0xffff);
            mem_write_32(addr, val);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x2b: {
            // SW，存32位
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            mem_write_32(addr, CURRENT_STATE.REGS[rt]);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        default: {
            printf("unimplemented instruction: 0x%08x\n", inst);
            break;
        }
    }
}
