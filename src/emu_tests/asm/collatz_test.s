	movi r3 main
	jalr r0  r3 

# Data Section:

# Code Section:
collatz:
	# Function Prologue
	sw r7  r1  -1
	sw r2  r1  -2
	addi r1  r1  -2
	addi r2  r1  0
	# Function Body
	addi r1  r1  -9
	sw r3  r2  -3
	movi r3 1
	sw r3  r2  -9
	lw r3  r2  -3
	lw r4  r2  -9
	and r3  r3  r4 
	sw r3  r2  -8
	lw r3  r2  -8
	movi r4 0
	cmp r3  r4 
	bz collatz.else.6
	lw r3  r2  -3
	lw r4  r2  -3
	add r3  r3  r4 
	sw r3  r2  -7
	lw r3  r2  -7
	lw r4  r2  -3
	add r3  r3  r4 
	sw r3  r2  -6
	movi r3 1
	sw r3  r2  -5
	lw r3  r2  -6
	lw r4  r2  -5
	add r3  r3  r4 
	sw r3  r2  -4
	lw r3  r2  -4
	# Function Epilogue
	mov r1  r2 
	lw r7  r2  1
	lw r2  r2  0
	addi r1  r1  2
	jalr r0  r7 
	movi r3 collatz.end.9
	jalr r0  r3 
collatz.else.6:
	movi r3 1
	sw r3  r2  -2
	lw r3  r2  -3
	lw r4  r2  -2
	call right_shift
	sw r3  r2  -1
	lw r3  r2  -1
	# Function Epilogue
	mov r1  r2 
	lw r7  r2  1
	lw r2  r2  0
	addi r1  r1  2
	jalr r0  r7 
collatz.end.9:
	movi r3 0
	# Function Epilogue
	mov r1  r2 
	lw r7  r2  1
	lw r2  r2  0
	addi r1  r1  2
	jalr r0  r7 
main:
	addi r1  r0  0
	addi r2  r0  0
	addi r1  r1  -8
	movi r3 121
	sw r3  r2  -8
	lw r3  r2  -8
	sw r3  r2  -3
	lw r3  r2  -3
	sw r3  r2  -2
main.while.0.continue:
	movi r3 1
	sw r3  r2  -6
	movi r3 1
	sw r3  r2  -7
	lw r3  r2  -3
	lw r4  r2  -7
	cmp r3  r4 
	bnz main.end.7
	movi r3 0
	sw r3  r2  -6
main.end.7:
	lw r3  r2  -6
	movi r4 0
	cmp r3  r4 
	bz main.while.0.break
	lw r3  r2  -3
	call collatz
	sw r3  r2  -5
	lw r3  r2  -5
	sw r3  r2  -3
	movi r3 1
	sw r3  r2  -4
	lw r3  r2  -3
	lw r4  r2  -2
	cmp r3  r4 
	ba main.end.3
	movi r3 0
	sw r3  r2  -4
main.end.3:
	lw r3  r2  -4
	movi r4 0
	cmp r3  r4 
	bz main.end.4
	lw r3  r2  -3
	sw r3  r2  -2
main.end.4:
	movi r3 main.while.0.continue
	jalr r0  r3 
main.while.0.break:
	lw r3  r2  -2
	sw r3  r2  -1
	lw r3  r2  -1
	# Function Epilogue
	sys EXIT
	movi r3 0
	# Function Epilogue
	sys EXIT
