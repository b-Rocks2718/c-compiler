main:
	push r7 
	push r2 
	addi r2  r1  0
	addi r1  r1  -7
	movi r3  4
	movi r4  1
	sub r3  r3  r4 
	sw r3  r1  -7
	movi r3  3
	lw r4  r1  -7
	call mul
	sw r3  r1  -3
	movi r3  5
	movi r4  3
	call mul
	sw r3  r1  -6
	lw r3  r1  -6
	movi r4  4
	sub r3  r3  r4 
	sw r3  r1  -5
	movi r3  11
	lw r4  r1  -5
	call mul
	sw r3  r1  -4
	lw r3  r1  -4
	movi r4  2
	call div
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
