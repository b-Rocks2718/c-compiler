  movi r4 0xFFFF
  movi r5 0xAAAA # 32 bit integer 0xAAAAFFFF stored in r4 + r5
  movi r6 0xFFFF
  movi r7 0x0001 # 32 bit integer 0x0001FFFF stored in r6 + r7
  # add and store result in r2 + r3
  add  r2 r4 r6
  addc r3 r5 r7
  # result should be 0xAAACFFFE
  # r3 should have 0xAAAC
  sys  EXIT