# generate branch and ALU ROMs
# exc rom depends on OS, so can't be generated here

branch_data = []

for address in range(2 ** 10):
    opdata = address % (2 ** 6)
    flags = address >> 6
    if opdata == 0 and flags & 2:
        branch_data.append(1)
    elif opdata == 1 and not flags & 2 and not flags & 4:
        branch_data.append(1)
    elif opdata == 2 and flags & 4:
        branch_data.append(1)
    elif opdata == 3 and flags & 1:
        branch_data.append(1)
    elif opdata == 4 and flags & 8:
        branch_data.append(1)
    elif opdata == 5 and not flags & 2:
        branch_data.append(1)
    elif opdata == 6:
        branch_data.append(1)
    elif opdata == 7 and not flags & 1:
        # bnc
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
            alu_data.append(0xC7)
            alu_data.append(0x0)
        elif opdata == 1:
            alu_data.append(0x8C)
            alu_data.append(0)
        elif opdata == 2:
            alu_data.append(0x8C)
            alu_data.append(0x08)
        elif opdata == 3:
            alu_data.append(0xCE)
            alu_data.append(0)
        elif opdata == 4:
            alu_data.append(0x83)
            alu_data.append(0xC)
        elif opdata == 5:
            alu_data.append(0xC8)
            alu_data.append(0)
        elif opdata == 6:
            alu_data.append(0x83)
            alu_data.append(0x04)
        elif opdata == 7:
            alu_data.append(0xC6)
            alu_data.append(0)
        elif opdata == 8:
            alu_data.append(0xC5)
            alu_data.append(0)
        elif opdata == 9:
            alu_data.append(0x60)
            alu_data.append(0)
        elif opdata == 10:
            alu_data.append(0x00)
            alu_data.append(0x2)
        elif opdata == 11:
            alu_data.append(0x50)
            alu_data.append(0)
        elif opdata == 12:
            alu_data.append(0x00)
            alu_data.append(0x3)
        elif opdata == 13:
            alu_data.append(0)
            alu_data.append(0x1)
        elif opdata == 14:
            alu_data.append(0)
            alu_data.append(0)
        elif opdata == 15:
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
