main:
	addi r1  r0  0
	addi r2  r0  0
	addi r1  r1  -8
	movi r3  0
	sw r3  r2  -3
	movi r3  1
	sw r3  r2  -7
	lw r3  r2  -3
	movi r4  1
	add r3  r3  r4 
	sw r3  r2  -8
	lw r3  r2  -8
	sw r3  r2  -3
	lw r3  r2  -7
	sw r3  r2  -5
	lw r3  r2  -7
	movi r4  1
	add r3  r3  r4 
	sw r3  r2  -6
	lw r3  r2  -6
	sw r3  r2  -7
	lw r3  r2  -3
	lw r4  r2  -5
	add r3  r3  r4 
	sw r3  r2  -4
	lw r3  r2  -4
	sw r3  r2  -2
	lw r3  r2  -3
	lw r4  r2  -2
	add r3  r3  r4 
	sw r3  r2  -1
	lw r3  r2  -1
	sys EXIT
	movi r3  0
	sys EXIT
