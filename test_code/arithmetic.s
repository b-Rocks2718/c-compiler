
# all of these can be more efficient using shifts
mul:
	push r5
	addi r5 r0 0
mul_loop:
	cmp r3 r0
	bz mul_end
	addi r3 r3 -1
	add r5 r5 r4
	jmp mul_loop
mul_end:
	mov r3 r5
	pop r5
	jalr r0 r7
	
div:
	push r5
	addi r5 r0 0
div_loop:
	cmp r3 r4
	bn div_end
	addi r5 r5 1
	sub r3 r3 r4
	jmp div_loop
div_end:
	mov r3 r5
	pop r5
	jalr r0 r7
	
mod:
	push r5
	addi r5 r0 0
mod_loop:
	cmp r3 r4
	bn mod_end
	sub r3 r3 r4
	jmp mod_loop
mod_end:
	pop r5
	jalr r0 r7
	