main:
	addi r1  r0  0
	addi r2  r0  0
	addi r1  r1  -3
	movi r3  0
	sw r3  r2  -3
	lw r3  r2  -3
	movi r4  2
	call mod
	sw r3  r2  -2
	lw r3  r2  -2
	movi r4  0
	cmp r3  r4 
	bz else.4
	movi r3  19
	sw r3  r2  -1
	jmp end.6
else.4:
	movi r3  21
	sw r3  r2  -1
end.6:
	lw r3  r2  -1
	sys EXIT
	movi r3  0
	sys EXIT
