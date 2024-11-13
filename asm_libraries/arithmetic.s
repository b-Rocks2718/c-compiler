
# add, div, and mod can probably be more efficient using shifts
# shl and shr should check if second parameter is >16 and return 0 if so
# but im lazy and this works for now
smul:
	sw r5 r1 -1 # push registers
	sw r6 r1 -2 
	# check sign of inputs, store results in r6
	# if inputs are negative, negate them
	lui r5 0x8000
	add r6 r0 r0
	and r0 r3 r5
	bz smul_check_r4
	addi r6 r6 1
	sub r3 r0 r3
smul_check_r4:
	and r0 r4 r5
	bz smul_pos
	addi r6 r6 1
	sub r4 r0 r4
smul_pos:
	addi r5 r0 0
smul_loop: # repeated addition
	cmp r3 r0
	bz smul_end
	addi r3 r3 -1
	add r5 r5 r4
	jmp smul_loop
smul_end:
	addi r6 r6 -1
	bnz smul_skip_negate
	sub r5 r0 r5 # fix sign of result
smul_skip_negate:
	mov r3 r5
	lw r5 r1 -1 # pop registers
	lw r6 r1 -2 
	jalr r0 r7

sdiv:
	sw r5 r1 -1 # push registers
	sw r6 r1 -2 
	# check sign of inputs, store results in r6
	lui r5 0x8000
	add r6 r0 r0
	and r0 r3 r5
	bz sdiv_check_r4
	addi r6 r6 1
	sub r3 r0 r3
sdiv_check_r4:
	and r0 r4 r5
	bz sdiv_pos
	addi r6 r6 1
	sub r4 r0 r4
sdiv_pos:
	add r5 r0 r0
sdiv_loop: # repeated subtraction
	cmp r3 r4
	bn sdiv_end
	addi r5 r5 1
	sub r3 r3 r4
	jmp sdiv_loop
sdiv_end:
	addi r6 r6 -1
	bnz sdiv_skip_negate
	sub r5 r0 r5
sdiv_skip_negate:
	mov r3 r5
	lw r5 r1 -1 # pop registers
	lw r6 r1 -2 
	jalr r0 r7

smod:
	sw r5 r1 -1 # push registers
	sw r6 r1 -2 
	# check sign of inputs, store results in r6
	lui r5 0x8000
	add r6 r0 r0
	and r0 r3 r5
	bz smod_check_r4
	addi r6 r6 1
	sub r3 r0 r3
smod_check_r4:
	and r0 r4 r5
	bz smod_pos
	addi r6 r6 1
	sub r4 r0 r4
smod_pos:
	add r5 r0 r0
smod_loop: # repeated subtraction
	cmp r3 r4
	bn smod_end
	sub r3 r3 r4
	jmp smod_loop
smod_end:
	addi r6 r6 -1
	bnz smod_skip_negate
	sub r3 r4 r3 # ensure result is between 0 and r4
smod_skip_negate:
	lw r5 r1 -1 # pop registers
	lw r6 r1 -2 
	jalr r0 r7

umul:
	sw r5 r1 -1 # push registers
	sw r6 r1 -2 
	addi r5 r0 0
umul_loop: # repeated addition
	cmp r3 r0
	bz umul_end
	addi r3 r3 -1
	add r5 r5 r4
	jmp umul_loop
umul_end:
	mov r3 r5
	lw r5 r1 -1 # pop registers
	lw r6 r1 -2 
	jalr r0 r7

udiv:
	sw r5 r1 -1 # push registers
	sw r6 r1 -2 
	add r5 r0 r0
udiv_loop: # repeated subtraction
	cmp r3 r4
	bb udiv_end
	addi r5 r5 1
	sub r3 r3 r4
	jmp udiv_loop
udiv_end:
	mov r3 r5
	lw r5 r1 -1 # pop registers
	lw r6 r1 -2 
	jalr r0 r7

umod:
	sw r5 r1 -1 # push registers
	sw r6 r1 -2 
	add r5 r0 r0
umod_loop: # repeated subtraction
	cmp r3 r4
	bb umod_end
	sub r3 r3 r4
	jmp umod_loop
umod_end:
	lw r5 r1 -1 # pop registers
	lw r6 r1 -2 
	jalr r0 r7
	
left_shift:
	sw r5 r1 -1 # push registers
	# check sign of r4
	# if negative, do right shift instead
	lui r5 0x8000
	and r0 r5 r4
	bz ls_loop
	sub r4 r0 r4
	jmp rs_loop
ls_loop: # repeated shift
	cmp r4 r0
	bz ls_end
	addi r4 r4 -1
	shl r3 r3
	jmp ls_loop
ls_end:
	lw r5 r1 -1 # pop registers
	jalr r0 r7
	
	
right_shift:
	sw r5 r1 -1 # push registers
	# check sign of r4
	# if negative, do left shift instead
	lui r5 0x8000
	and r0 r5 r4
	bz rs_loop
	sub r4 r0 r4
	jmp ls_loop
rs_loop: # repeated shift
	cmp r4 r0
	bz rs_end
	addi r4 r4 -1
	sshr r3 r3
	jmp rs_loop
rs_end:
	lw r5 r1 -1 # pop registers
	jalr r0 r7
	
# TODO: work out a more elegant way to do this
putchar:
  sys PUTCHAR
  jalr r0 r7