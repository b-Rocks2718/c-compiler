main:
	addi r1  r0  0
	addi r2  r0  0
	addi r1  r1  -4
	movi r3  10
	sw r3  r2  -3
	movi r3  0
	sw r3  r2  -1
main.loop:
	movi r3  1
	sw r3  r2  -2
	lw r3  r2  -3
	movi r4  0
	cmp r3  r4 
	bz end.1
	movi r3  0
	sw r3  r2  -2
end.1:
	lw r3  r2  -2
	movi r4  0
	cmp r3  r4 
	bz end.3
	jmp main.done
end.3:
	lw r3  r2  -1
	lw r4  r2  -3
	add r3  r3  r4 
	sw r3  r2  -2
	lw r3  r2  -2
	sw r3  r2  -1
	lw r3  r2  -3
	sw r3  r2  -4
	lw r3  r2  -3
	movi r4  1
	sub r3  r3  r4 
	sw r3  r2  -2
	lw r3  r2  -2
	sw r3  r2  -3
	jmp main.loop
main.done:
	lw r3  r2  -1
	sys EXIT
	movi r3  0
	sys EXIT
