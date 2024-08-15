	movi r3  main
	jalr r0  r3 
next_collatz:
	sw r7  r1  -1
	sw r2  r1  -2
	addi r1  r1  -2
	addi r2  r1  0
	addi r1  r1  -5
	sw r3  r2  -1
	lw r3  r2  -1
	movi r4  1
	and r3  r3  r4 
	sw r3  r2  -5
	lw r3  r2  -5
	movi r4  0
	cmp r3  r4 
	bz else.3
	movi r3  3
	lw r4  r2  -1
	call mul
	sw r3  r2  -4
	lw r3  r2  -4
	movi r4  1
	add r3  r3  r4 
	sw r3  r2  -3
	lw r3  r2  -3
	sw r3  r2  -1
	movi r3  end.6
	jalr r0  r3 
else.3:
	lw r3  r2  -1
	movi r4  1
	call right_shift
	sw r3  r2  -2
	lw r3  r2  -2
	sw r3  r2  -1
end.6:
	lw r3  r2  -1
	mov r1  r2 
	lw r7  r2  1
	lw r2  r2  0
	addi r1  r1  2
	jalr r0  r7 
	movi r3  0
	mov r1  r2 
	lw r7  r2  1
	lw r2  r2  0
	addi r1  r1  2
	jalr r0  r7 
max:
	sw r7  r1  -1
	sw r2  r1  -2
	addi r1  r1  -2
	addi r2  r1  0
	addi r1  r1  -3
	sw r3  r2  -2
	sw r4  r2  -1
	movi r3  1
	sw r3  r2  -3
	lw r3  r2  -2
	lw r4  r2  -1
	cmp r3  r4 
	bg end.1
	movi r3  0
	sw r3  r2  -3
end.1:
	lw r3  r2  -3
	movi r4  0
	cmp r3  r4 
	bz else.2
	lw r3  r2  -2
	mov r1  r2 
	lw r7  r2  1
	lw r2  r2  0
	addi r1  r1  2
	jalr r0  r7 
	movi r3  end.4
	jalr r0  r3 
else.2:
	lw r3  r2  -1
	mov r1  r2 
	lw r7  r2  1
	lw r2  r2  0
	addi r1  r1  2
	jalr r0  r7 
end.4:
	movi r3  0
	mov r1  r2 
	lw r7  r2  1
	lw r2  r2  0
	addi r1  r1  2
	jalr r0  r7 
main:
	addi r1  r0  0
	addi r2  r0  0
	addi r1  r1  -5
	movi r3  121
	sw r3  r2  -3
	lw r3  r2  -3
	sw r3  r2  -1
while.0.continue:
	movi r3  1
	sw r3  r2  -5
	lw r3  r2  -3
	movi r4  1
	cmp r3  r4 
	bnz end.3
	movi r3  0
	sw r3  r2  -5
end.3:
	lw r3  r2  -5
	movi r4  0
	cmp r3  r4 
	bz while.0.break
	lw r3  r2  -3
	call next_collatz
	sw r3  r2  -4
	lw r3  r2  -4
	sw r3  r2  -3
	lw r3  r2  -1
	lw r4  r2  -3
	call max
	sw r3  r2  -2
	lw r3  r2  -2
	sw r3  r2  -1
	movi r3  while.0.continue
	jalr r0  r3 
while.0.break:
	lw r3  r2  -1
	sys EXIT
	movi r3  0
	sys EXIT
