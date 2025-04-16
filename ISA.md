# ISA:

Mostly an extension of Dr. Bruce Jacob's RiSC-16 ISA:
https://user.eng.umd.edu/~blj/risc/RiSC-isa.pdf


3 Bit opcodes, 8 registers, 4 flags (Zero, Sign, Carry, Overflow)

## ALU instructions
opcode is 000

rA is destination, rB and rC are sources

000aaabbb0000ccc - nand rA, rB, rC  
000aaabbb0001ccc - add  rA, rB, rC  
000aaabbb0010ccc - addc rA, rB, rC  (add with carry)  
000aaabbb0011ccc - or   rA, rB, rC  
000aaabbb0100ccc - subc rA, rB, rC  (sub with carry)  
000aaabbb0101ccc - and  rA, rB, rC  
000aaabbb0110ccc - sub  rA, rB, rC  
000aaabbb0111ccc - xor  rA, rB, rC  
000aaabbb1000ccc - not  rA, rC  
000aaabbb1001ccc - shl  rA, rB, rC  (logical shift left)    
000aaabbb1010ccc - shr  rA, rB, rC  (logical shift right)  
000aaabbb1011ccc - rotl rA, rB, rC  (rotate left)  
000aaabbb1100ccc - rotr rA, rB, rC  (rotate right)  
000aaabbb1101ccc - sshr rA, rB, rC  (arithmetic shift right)  
000aaabbb1110ccc - shrc rA, rB, rC  (shift right with carry)  
000aaabbb1111ccc - shlc rA, rB, rC  (shift left with carry)  

## addi

(add immediate)

opcode is 001

i is a 7 bit immediate, sign extended to 16 bits

001aaabbbiiiiiii - addi rA, rB, i (rA = rB + extend(i))

## lui

(load upper immediate)

opcode is 011

i is a 10 bit immediate, left shifted by 6 to make a 16 bit value

rA = i << 6

011aaaiiiiiiiiii - lui  rA, i (rA = (i << 6))

## sw

(store word)

opcode is 100

i is a 7 bit immediate, sign extended to 16 bits

store rA at address rB + i

100aaabbbiiiiiii - sw   rA, rB

## lw

(load word)

opcode is 101

i is a 7 bit immediate, sign extended to 16 bits

load value at address rB + i into rA

101aaabbbiiiiiii - lw   rA, rB

## branch instructions

opcode is 110

i is a 7 bit immediate, sign extended to 16 bits

branches make decisions based on the state of the flags

branch target is (pc + 1 + i)

110000000iiiiiii - bz  i or beq i (branch if zero/branch if equal)  
110000001iiiiiii - bp  i  (brnach if positive)  
110000010iiiiiii - bn  i  (branch if negative)  
110000011iiiiiii - bc  i  (branch if carry)  
110000100iiiiiii - bo  i  (branch if overflow)  
110000101iiiiiii - bnz i or bne i   (branch if nonzero/branch if not equal)  
110000110iiiiiii - jmp i  (unconditional jump)  
110000111iiiiiii - bnc i  (branch if not carry)  
110001000iiiiiii - bg  i  (branch if greater (signed))  
110001001iiiiiii - bge i  (branch if greater or equal (signed))  
110001010iiiiiii - bl  i  (branch if less (signed))  
110001011iiiiiii - ble i  (branch if less or equal (signed))  
110001100iiiiiii - ba  i  (branch if above (unsigned))  
110001101iiiiiii - bae i  (branch if above or equal (unsigned))  
110001110iiiiiii - bb  i  (branch if below (unsigned))  
110001111iiiiiii - bbe i  (branch if below or equal (unsigned))  
110010000iiiiiii - bno i  (branch if not overflow)  

## jalr

(jump and link register)

opcode is 111, bottom 7 bits must be 0

store pc + 1 in rA, branch to address in rB

111aaabbb0000000 - jalr rA, rB

## halt

jalr with any nonzero immdiate in the bottom 7 bits is a halt

111xxxyyy0000001 - halt

## unused

opcode 010 is currently unused