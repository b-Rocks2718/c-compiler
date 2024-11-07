  mov  r4 r0
  mov  r5 r0 # 32 bit integer 0x00000000 stored in r4 + r5
  addi r6 r0 1
  mov  r7 r0 # 32 bit integer 0x00000001 stored in r6 + r7
  # add and store result in r2 + r3
  sub  r2 r4 r6
  subc r3 r5 r7
  # result should be 0xFFFFFFFF
  # r3 should have 0xFFFF
  sys  EXIT