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
main:
	addi r1  r0  0
	addi r2  r0  0
	addi r1  r1  -30
	movi r3 120
	sw r3  r2  -30
	lw r3  r2  -30
	call putchar
	sw r3  r2  -29
	movi r3 58
	sw r3  r2  -28
	lw r3  r2  -28
	call putchar
	sw r3  r2  -27
	movi r3 1
	sub r3  r0  r3 
	sw r3  r2  -26
	lw r3  r2  -26
	sw r3  r2  -20
	movi r3 1
	sw r3  r2  -25
	lw r3  r2  -20
	movi r4 0
	cmp r3  r4 
	bg main.end.6
	movi r3 0
	sw r3  r2  -25
main.end.6:
	lw r3  r2  -25
	movi r4 0
	cmp r3  r4 
	bz main.else.9
	lw r3  r2  -20
	sw r3  r2  -24
	lw r3  r2  -24
	call print_unisgned
	sw r3  r2  -23
	movi r3 main.end.15
	jalr r0  r3 
main.else.9:
	movi r3 45
	sw r3  r2  -22
	lw r3  r2  -22
	call putchar
	sw r3  r2  -21
	lw r3  r2  -20
	sub r3  r0  r3 
	sw r3  r2  -19
	lw r3  r2  -19
	sw r3  r2  -18
	lw r3  r2  -18
	call print_unisgned
	sw r3  r2  -17
main.end.15:
	movi r3 10
	sw r3  r2  -16
	lw r3  r2  -16
	call putchar
	sw r3  r2  -15
	movi r3 121
	sw r3  r2  -14
	lw r3  r2  -14
	call putchar
	sw r3  r2  -13
	movi r3 58
	sw r3  r2  -12
	lw r3  r2  -12
	call putchar
	sw r3  r2  -11
	movi r3 1
	sub r3  r0  r3 
	sw r3  r2  -10
	lw r3  r2  -10
	sw r3  r2  -9
	lw r3  r2  -9
	sw r3  r2  -3
	movi r3 1
	sw r3  r2  -7
	movi r3 0
	sw r3  r2  -8
	lw r3  r2  -3
	lw r4  r2  -8
	cmp r3  r4 
	ba main.end.26
	movi r3 0
	sw r3  r2  -7
main.end.26:
	lw r3  r2  -7
	movi r4 0
	cmp r3  r4 
	bz main.else.28
	lw r3  r2  -3
	call print_unisgned
	sw r3  r2  -6
	movi r3 main.end.33
	jalr r0  r3 
main.else.28:
	movi r3 45
	sw r3  r2  -5
	lw r3  r2  -5
	call putchar
	sw r3  r2  -4
	lw r3  r2  -3
	sub r3  r0  r3 
	sw r3  r2  -2
	lw r3  r2  -2
	call print_unisgned
	sw r3  r2  -1
main.end.33:
	movi r3 0
	# Function Epilogue
	sys EXIT
