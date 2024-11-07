	movi r3 main
	jalr r0  r3 

# Data Section:
ZERO_CHAR:
	.fill 48

# Code Section:
print_unisgned:
	# Function Prologue
	sw r7  r1  -1
	sw r2  r1  -2
	addi r1  r1  -2
	addi r2  r1  0
	# Function Body
	addi r1  r1  -11
	sw r3  r2  -5
	movi r3 10
	sw r3  r2  -11
	lw r3  r2  -5
	lw r4  r2  -11
	call umod
	sw r3  r2  -10
	lw r3  r2  -10
	sw r3  r2  -3
	movi r3 10
	sw r3  r2  -9
	lw r3  r2  -5
	lw r4  r2  -9
	call udiv
	sw r3  r2  -8
	lw r3  r2  -8
	sw r3  r2  -5
	movi r3 1
	sw r3  r2  -6
	movi r3 0
	sw r3  r2  -7
	lw r3  r2  -5
	lw r4  r2  -7
	cmp r3  r4 
	bnz print_unisgned.end.6
	movi r3 0
	sw r3  r2  -6
print_unisgned.end.6:
	lw r3  r2  -6
	movi r4 0
	cmp r3  r4 
	bz print_unisgned.end.8
	lw r3  r2  -5
	call print_unisgned
	sw r3  r2  -4
print_unisgned.end.8:
	movi r3 ZERO_CHAR
	lw r3  r3  0
	lw r4  r2  -3
	add r3  r3  r4 
	sw r3  r2  -2
	lw r3  r2  -2
	call putchar
	sw r3  r2  -1
	movi r3 0
	# Function Epilogue
	mov r1  r2 
	lw r7  r2  1
	lw r2  r2  0
	addi r1  r1  2
	jalr r0  r7 
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
	addi r1  r1  -47
	movi r3 121
	sw r3  r2  -47
	lw r3  r2  -47
	sw r3  r2  -18
	lw r3  r2  -18
	sw r3  r2  -2
	movi r3 0
	sw r3  r2  -46
	lw r3  r2  -46
	sw r3  r2  -20
	movi r3 67
	sw r3  r2  -45
	lw r3  r2  -45
	call putchar
	sw r3  r2  -44
	movi r3 111
	sw r3  r2  -43
	lw r3  r2  -43
	call putchar
	sw r3  r2  -42
	movi r3 108
	sw r3  r2  -41
	lw r3  r2  -41
	call putchar
	sw r3  r2  -40
	movi r3 108
	sw r3  r2  -39
	lw r3  r2  -39
	call putchar
	sw r3  r2  -38
	movi r3 97
	sw r3  r2  -37
	lw r3  r2  -37
	call putchar
	sw r3  r2  -36
	movi r3 116
	sw r3  r2  -35
	lw r3  r2  -35
	call putchar
	sw r3  r2  -34
	movi r3 122
	sw r3  r2  -33
	lw r3  r2  -33
	call putchar
	sw r3  r2  -32
	movi r3 58
	sw r3  r2  -31
	lw r3  r2  -31
	call putchar
	sw r3  r2  -30
	movi r3 10
	sw r3  r2  -29
	lw r3  r2  -29
	call putchar
	sw r3  r2  -28
main.while.0.continue:
	movi r3 1
	sw r3  r2  -26
	movi r3 1
	sw r3  r2  -27
	lw r3  r2  -18
	lw r4  r2  -27
	cmp r3  r4 
	bnz main.end.30
	movi r3 0
	sw r3  r2  -26
main.end.30:
	lw r3  r2  -26
	movi r4 0
	cmp r3  r4 
	bz main.while.0.break
	lw r3  r2  -18
	call print_unisgned
	sw r3  r2  -25
	movi r3 44
	sw r3  r2  -24
	lw r3  r2  -24
	call putchar
	sw r3  r2  -23
	lw r3  r2  -18
	call collatz
	sw r3  r2  -22
	lw r3  r2  -22
	sw r3  r2  -18
	movi r3 1
	sw r3  r2  -21
	lw r3  r2  -18
	lw r4  r2  -2
	cmp r3  r4 
	ba main.end.25
	movi r3 0
	sw r3  r2  -21
main.end.25:
	lw r3  r2  -21
	movi r4 0
	cmp r3  r4 
	bz main.end.26
	lw r3  r2  -18
	sw r3  r2  -2
main.end.26:
	movi r3 1
	sw r3  r2  -19
	lw r3  r2  -20
	lw r4  r2  -19
	add r3  r3  r4 
	sw r3  r2  -20
	movi r3 main.while.0.continue
	jalr r0  r3 
main.while.0.break:
	lw r3  r2  -18
	call print_unisgned
	sw r3  r2  -17
	movi r3 10
	sw r3  r2  -16
	lw r3  r2  -16
	call putchar
	sw r3  r2  -15
	movi r3 10
	sw r3  r2  -14
	lw r3  r2  -14
	call putchar
	sw r3  r2  -13
	movi r3 77
	sw r3  r2  -12
	lw r3  r2  -12
	call putchar
	sw r3  r2  -11
	movi r3 97
	sw r3  r2  -10
	lw r3  r2  -10
	call putchar
	sw r3  r2  -9
	movi r3 120
	sw r3  r2  -8
	lw r3  r2  -8
	call putchar
	sw r3  r2  -7
	movi r3 58
	sw r3  r2  -6
	lw r3  r2  -6
	call putchar
	sw r3  r2  -5
	movi r3 32
	sw r3  r2  -4
	lw r3  r2  -4
	call putchar
	sw r3  r2  -3
	lw r3  r2  -2
	call print_unisgned
	sw r3  r2  -1
	movi r3 0
	# Function Epilogue
	sys EXIT
