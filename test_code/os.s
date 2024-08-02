	# os = true

	# the cpu starts in kernel mode
	# this code will copy code appended after 'start' 
	# to user space and run it in user mode
	
	# for large programs, you may need to 
	# change the value in line 15
	
	# set up mem map
	movi r6, 0x0020
	
	# load program into user space
	movi r2, start
	lui  r3, 1000 # size of program (in words)
	lui  r4, 0x8000
	movi r7, move_to_user_space
	jalr r7, r7	
	
	# enable interupts
	movi r7, 0xFF00
	or   r6, r7, r6
	
	# run user program
	rfe  r0

EXC_PRIV:
	movi r1, 0xEEEE
	sys MODE_HALT

EXIT:
	# user programs return result in r3
	# put it in cr1 for testing
	tocr r1, r3
	sys MODE_HALT

# not worrying about these for now
INT0:
INT1:
INT2:
INT3:
INT4:
INT5:
INT6:
INT7:
	# log that an interrupt happened
	kpsh r2
	kpsh r3
	lui	 r2, 0x100
	lw	 r3, r2, 0
	addi r3, r3, 1
	sw   r3, r2, 0
	kpop r3
	kpop r2
	rfi  r7, r5

move_to_user_space:
	# address in r2
	# length of program in r3
	# target address in r4
	# return address in r7
	kpsh r7
move_to_user_space_loop:
	lw   r7, r2, 0
	addi r2, r2, 1
	sw   r7, r4, 0		
	addi r4, r4, 1
	addi r3, r3, -1
	bnz move_to_user_space_loop
	kpop r7
	jalr r0, r7

start:
