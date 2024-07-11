main:
	push r7 
	push r1 
	addi r1  r1  0
	addi r1  r1  -2
	addi r3  r0  9
	sw r3  r1  -2
	lw r3  r1  -2
	sub r3  r0  r3 
	sw r3  r1  -2
	lw r3  r1  -2
	sw r3  r1  -1
	lw r3  r1  -1
	not r3  r3 
	sw r3  r1  -1
	lw r3  r1  -1
	addi r1  r1  0
	pop r1 
	pop r7 
	sys EXIT
