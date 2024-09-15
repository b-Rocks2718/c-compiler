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

  movi r7, write_tilemap
  jalr r7, r7
	
	# enable interupts
	movi r7, 0xFF00
	or   r6, r7, r6
	
	# run user program
	rfe  r0

terminal_index:
  .fill 0x8000

PUTCHAR:
  # could be more efficient - 
  # need add more support for relative addressing in assembler

  # writes the character in r3
  kpsh r6
  addi r6 r0 0x0030 # set mem map to framebuffer
  movi r4 terminal_index
  lw r5 r4 0
  tocr r3, r3
  sw r3 r5 0
  addi r5 r5 1
  sw r5 r4 0
  kpop r6
  rfe r7

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

write_tilemap:
  kpsh r6
  movi r6 0x0038
  movi r2 tilemap
  movi r4 0x0F00
  movi r5 tilemap_end
write_tilemap_loop:
  lw   r3 r2 0
  sw   r4 r3 0
  addi r2 r2 1
  cmp  r2 r5
  bnz  write_tilemap_loop
  kpop r6
  jalr r0 r7

tilemap:
  # H
  .fill 0x8049
  .fill 0x804E
	.fill 0x8051
	.fill 0x8056
	.fill 0x8059
	.fill 0x805A
	.fill 0x805B
	.fill 0x805C
	.fill 0x805D
	.fill 0x805E
	.fill 0x8061
	.fill 0x8066
	.fill 0x8069
	.fill 0x806E
	.fill 0x8071
	.fill 0x8076

  # E
	.fill 0x8089
	.fill 0x808A
	.fill 0x808B
	.fill 0x8091
	.fill 0x8099
	.fill 0x809A
	.fill 0x80A1
	.fill 0x80A9
	.fill 0x80B1
	.fill 0x80B2
	.fill 0x80B3
	
	# L
	.fill 0x80C9
	.fill 0x80D1
	.fill 0x80D9
	.fill 0x80E1
	.fill 0x80E9
	.fill 0x80F1
	.fill 0x80F2
	.fill 0x80F3
	
	# O
	.fill 0x810A
	.fill 0x810B
	.fill 0x810C
	.fill 0x8111
	.fill 0x8115
	.fill 0x8119
	.fill 0x811D
	.fill 0x8121
	.fill 0x8125
	.fill 0x8129
	.fill 0x812D
	.fill 0x8132
	.fill 0x8133
	.fill 0x8134
	
	# W
	.fill 0x8149
	.fill 0x814D
	.fill 0x8151
	.fill 0x8155
	.fill 0x8159
	.fill 0x815B
	.fill 0x815D
	.fill 0x8161
	.fill 0x8163
	.fill 0x8165
	.fill 0x8169
	.fill 0x816B
	.fill 0x816D
	.fill 0x8172
	.fill 0x8174
	
	# R
	.fill 0x8189
	.fill 0x818A
	.fill 0x818B
	.fill 0x8191
	.fill 0x8194
	.fill 0x8199
	.fill 0x819C
	.fill 0x81A1
	.fill 0x81A2
	.fill 0x81A3
	.fill 0x81A9
	.fill 0x81AC
	.fill 0x81B1
	.fill 0x81B4
	
	# D
	.fill 0x81C9
	.fill 0x81CA
	.fill 0x81CB
	.fill 0x81D1
	.fill 0x81D4
	.fill 0x81D9
	.fill 0x81DC
	.fill 0x81E1
	.fill 0x81E4
	.fill 0x81E9
	.fill 0x81EC
	.fill 0x81F1
	.fill 0x81F2
	.fill 0x81F3
	
	# !
	.fill 0x820B
	.fill 0x8213
	.fill 0x821B
	.fill 0x8223
	.fill 0x8233
tilemap_end:

start:
