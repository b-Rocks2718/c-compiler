main:
	addi r1  r0  0
	addi r2  r0  0
	addi r1  r1  -8
	movi r3  121
	sw r3  r2  -2
	lw r3  r2  -2
	sw r3  r2  -1
while.0.continue:
	movi r3  1
	sw r3  r2  -8
	lw r3  r2  -2
	movi r4  1
	cmp r3  r4 
	bnz end.12
	movi r3  0
	sw r3  r2  -8
end.12:
	lw r3  r2  -8
	movi r4  0
	cmp r3  r4 
	bz while.0.break
	lw r3  r2  -2
	movi r4  2
	call mod
	sw r3  r2  -7
	lw r3  r2  -7
	movi r4  0
	cmp r3  r4 
	bz else.3
	movi r3  3
	lw r4  r2  -2
	call mul
	sw r3  r2  -6
	lw r3  r2  -6
	movi r4  1
	add r3  r3  r4 
	sw r3  r2  -5
	lw r3  r2  -5
	sw r3  r2  -2
	movi r3  end.6
	jalr r0  r3 
else.3:
	lw r3  r2  -2
	movi r4  2
	call div
	sw r3  r2  -4
	lw r3  r2  -4
	sw r3  r2  -2
end.6:
	movi r3  1
	sw r3  r2  -3
	lw r3  r2  -2
	lw r4  r2  -1
	cmp r3  r4 
	bg end.8
	movi r3  0
	sw r3  r2  -3
end.8:
	lw r3  r2  -3
	movi r4  0
	cmp r3  r4 
	bz end.10
	lw r3  r2  -2
	sw r3  r2  -1
end.10:
	movi r3  while.0.continue
	jalr r0  r3 
while.0.break:
	lw r3  r2  -1
	sys EXIT
	movi r3  0
	sys EXIT
