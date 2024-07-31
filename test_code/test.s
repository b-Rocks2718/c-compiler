main:
	push r7 
	push r2 
	addi r2  r1  0
	addi r1  r1  -2
	movi r3  1
	movi r4  3
	add r3  r3  r4 
	sw r3  r1  -2
	movi r3  5
	lw r4  r1  -2
	call left_shift
	sw r3  r1  -1
	lw r3  r1  -1
	addi r1  r2  0
	pop r2 
	pop r7 
	sys EXIT
