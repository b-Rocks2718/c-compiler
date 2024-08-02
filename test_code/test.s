main:
	sw r7  r1  -1
	sw r2  r1  -2
	addi r1  r1  -2
	addi r2  r1  0
	addi r1  r1  -1
	movi r3  1
	sw r3  r1  -1
	movi r3  2
	movi r4  3
	cmp r3  r4 
	bl end.0
	movi r3  0
	sw r3  r1  -1
end.0:
	lw r3  r1  -1
	addi r1  r2  0
	lw r2  r1  -2
	lw r7  r1  -1
	addi r1  r1  2
	sys EXIT
