# generate branch and ALU ROMs
# exc rom depends on OS, so can't be generated here

branch_data = []

for address in range(2 ** 10):
    opdata = address % (2 ** 6)
    flags = address >> 6
    # flags are | ovrflw | sign | zero | carry |
    if opdata == 0 and flags & 2:
        # bz / beq
        branch_data.append(1)
    elif opdata == 1 and not flags & 2 and not flags & 4:
        # bp
        branch_data.append(1)
    elif opdata == 2 and flags & 4:
        # bn
        branch_data.append(1)
    elif opdata == 3 and flags & 1:
        # bc
        branch_data.append(1)
    elif opdata == 4 and flags & 8:
        # bo
        branch_data.append(1)
    elif opdata == 5 and not flags & 2:
        # bnz / bne
        branch_data.append(1)
    elif opdata == 6:
        # jmp
        branch_data.append(1)
    elif opdata == 7 and not flags & 1:
        # bnc
        branch_data.append(1)
    elif (opdata == 8 and not flags & 2
          and (bool(flags & 4) == bool(flags & 8))):
        # bg
        branch_data.append(1)
    elif (opdata == 9 and
          (bool(flags & 4) == bool(flags & 8))):
        # bge
        branch_data.append(1)
    elif (opdata == 10 and not flags & 2
          and (bool(flags & 4) != bool(flags & 8))):
        # bl
        branch_data.append(1)
    elif (opdata == 11 and
          (bool(flags & 4) != bool(flags & 8) or flags & 2)):
        # ble
        branch_data.append(1)
    elif (opdata == 12 and
          (not flags & 2 and flags & 1)):
        # ba
        branch_data.append(1)
    elif (opdata == 13 and
          (flags & 1)):
        # bae (same as bc), can optimize in the future
        branch_data.append(1)
    elif (opdata == 14 and
          (not flags & 1)):
        # bb
        branch_data.append(1)
    elif (opdata == 15 and
          (flags & 2 or not flags & 1)):
        # bbe
        branch_data.append(1)
    else:
        branch_data.append(0)

with open("branch_rom.bin", "wb") as branch_file:
    branch_file.write(bytearray(branch_data))

alu_data = []

for address in range(2 ** 7):
    op = address % 8
    opdata = address >> 3
    if op == 0:
        # alu operations
        if opdata == 0:
            # nand
            alu_data.append(0xC7)
            alu_data.append(0x0)
        elif opdata == 1:
            # add
            alu_data.append(0x8C)
            alu_data.append(0)
        elif opdata == 2:
            # addc
            alu_data.append(0x8C)
            alu_data.append(0x08)
        elif opdata == 3:
            # or
            alu_data.append(0xCE)
            alu_data.append(0)
        elif opdata == 4:
            # subc
            alu_data.append(0x83)
            alu_data.append(0xC)
        elif opdata == 5:
            # and
            alu_data.append(0xC8)
            alu_data.append(0)
        elif opdata == 6:
            # sub
            alu_data.append(0x83)
            alu_data.append(0x04)
        elif opdata == 7:
            # cmp
            # alu_data.append(0xC6) ????
            alu_data.append(0x83)
            alu_data.append(0x04)
        elif opdata == 8:
            # xor
            alu_data.append(0xC5)
            alu_data.append(0)
        elif opdata == 9:
            # not
            alu_data.append(0x60)
            alu_data.append(0)
        elif opdata == 10:
            # shl
            alu_data.append(0x00)
            alu_data.append(0x2)
        elif opdata == 11:
            # shr
            alu_data.append(0x50)
            alu_data.append(0)
        elif opdata == 12:
            # rotl
            alu_data.append(0x00)
            alu_data.append(0x3)
        elif opdata == 13:
            # rotr
            alu_data.append(0)
            alu_data.append(0x1)
        elif opdata == 14:
            # shlc
            alu_data.append(0)
            alu_data.append(0)
        elif opdata == 15:
            # shrc
            alu_data.append(0x40)
            alu_data.append(0)
        else:
            raise ValueError("Logical Error")
    elif op == 1:
        # addi
        alu_data.append(0x8C)
        alu_data.append(0x20)
    elif op == 2:
        # unused
        alu_data.append(0)
        alu_data.append(0x10)
    elif op == 3:
        # lui
        alu_data.append(0xCC)
        alu_data.append(0x10)
    elif op in [4, 5]:
        # sw, lw
        alu_data.append(0x8C)
        alu_data.append(0x30)
    elif op == 6:
        # branch
        alu_data.append(0)
        alu_data.append(0x10)
    elif op == 7:
        # jalr
        alu_data.append(0xCC)
        alu_data.append(0x10)
    else:
        raise ValueError("logical error")

with open("alu_rom.bin", "wb") as alu_file:
    alu_file.write(bytearray(alu_data))
