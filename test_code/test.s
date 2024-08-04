main:
	sw r7  r1  -1
	sw r2  r1  -2
	addi r1  r1  -2
	addi r2  r1  0
	addi r1  r1  -4
	movi r3  7
	sw r3  r1  -1
	lw r3  r1  -1
	movi r4  1
	add r3  r3  r4 
	sw r3  r1  -4
	lw r3  r1  -4
	sw r3  r1  -1
	movi r3  9
	sw r3  r1  -3
	lw r3  r1  -1
	lw r4  r1  -3
	call mul
	sw r3  r1  -2
	lw r3  r1  -2
	sw r3  r1  -1
	lw r3  r1  -1
	addi r1  r2  0
	lw r2  r1  -2
	lw r7  r1  -1
	addi r1  r1  2
	sys EXIT
	movi r3  0
	addi r1  r2  0
	lw r2  r1  -2
	lw r7  r1  -1
	addi r1  r1  2
	sys EXIT
