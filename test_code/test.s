	movi r3  main
	jalr r0  r3 

# Data Section:

# Code Section:
main:
	addi r1  r0  0
	addi r2  r0  0
	addi r1  r1  -12
	movi r3  1
	call putchar
	sw r3  r2  -12
	movi r3  2
	call putchar
	sw r3  r2  -11
	movi r3  3
	call putchar
	sw r3  r2  -10
	movi r3  3
	call putchar
	sw r3  r2  -9
	movi r3  4
	call putchar
	sw r3  r2  -8
	movi r3  0
	call putchar
	sw r3  r2  -7
	movi r3  5
	call putchar
	sw r3  r2  -6
	movi r3  4
	call putchar
	sw r3  r2  -5
	movi r3  6
	call putchar
	sw r3  r2  -4
	movi r3  3
	call putchar
	sw r3  r2  -3
	movi r3  7
	call putchar
	sw r3  r2  -2
	movi r3  8
	call putchar
	sw r3  r2  -1
	movi r3  0
	# Function Epilogue
	sys EXIT
