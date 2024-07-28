main:
	push r7 
	push r2 
	addi r2  r1  0
	addi r1  r1  -6
	movi r3  4
	movi r4  1
	sub r3  r3  r4 
	sw r3  r1  -6
	movi r3  3
	lw r4  r1  -6
	call mul
	sw r3  r1  -3
	movi r3  4
	movi r4  2
	call mul
	sw r3  r1  -5
	lw r3  r1  -5
	movi r4  2
	sub r3  r3  r4 
	sw r3  r1  -4
	movi r3  10
	lw r4  r1  -4
	call mul
	sw r3  r1  -2
	lw r3  r1  -3
	lw r4  r1  -2
	add r3  r3  r4 
	sw r3  r1  -1
	lw r3  r1  -1
	addi r1  r2  0
	pop r2 
	pop r7 
	sys EXIT
