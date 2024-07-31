
# add, div, and mod can probably be more efficient using shifts
# but im lazy and this works for now
mul:
	sw r5 r1 -1 # push registers
	sw r6 r1 -2 
	# check sign of inputs, store results in r6
	# if inputs are negative, negate them
	lui r5 0x8000
	add r6 r0 r0
	and r0 r3 r5
	bz mul_check_r4
	addi r6 r6 1
	sub r3 r0 r3
mul_check_r4:
	and r0 r4 r5
	bz mul_pos
	addi r6 r6 1
	sub r4 r0 r4
mul_pos:
	addi r5 r0 0
mul_loop: # repeated addition
	cmp r3 r0
	bz mul_end
	addi r3 r3 -1
	add r5 r5 r4
	jmp mul_loop
mul_end:
	addi r6 r6 -1
	bnz mul_skip_negate
	sub r5 r0 r5 # fix sign of result
mul_skip_negate:
	mov r3 r5
	lw r5 r1 -1 # pop registers
	lw r6 r1 -2 
	jalr r0 r7


div:
	sw r5 r1 -1 # push registers
	sw r6 r1 -2 
	# check sign of inputs, store results in r6
	lui r5 0x8000
	add r6 r0 r0
	and r0 r3 r5
	bz div_check_r4
	addi r6 r6 1
	sub r3 r0 r3
div_check_r4:
	and r0 r4 r5
	bz div_pos
	addi r6 r6 1
	sub r4 r0 r4
div_pos:
	add r5 r0 r0
div_loop: # repeated subtraction
	cmp r3 r4
	bn div_end
	addi r5 r5 1
	sub r3 r3 r4
	jmp div_loop
div_end:
	addi r6 r6 -1
	bnz div_skip_negate
	sub r5 r0 r5
div_skip_negate:
	mov r3 r5
	lw r5 r1 -1 # pop registers
	lw r6 r1 -2 
	jalr r0 r7
	
	
mod:
	sw r5 r1 -1 # push registers
	sw r6 r1 -2 
	# check sign of inputs, store results in r6
	lui r5 0x8000
	add r6 r0 r0
	and r0 r3 r5
	bz mod_check_r4
	addi r6 r6 1
	sub r3 r0 r3
mod_check_r4:
	and r0 r4 r5
	bz mod_pos
	addi r6 r6 1
	sub r4 r0 r4
mod_pos:
	add r5 r0 r0
mod_loop: # repeated subtraction
	cmp r3 r4
	bn mod_end
	sub r3 r3 r4
	jmp mod_loop
mod_end:
	addi r6 r6 -1
	bnz mod_skip_negate
	sub r3 r4 r3 # ensure result is between 0 and r4
mod_skip_negate:
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
	shr r3 r3
	jmp rs_loop
rs_end:
	lw r5 r1 -1 # pop registers
	jalr r0 r7
	