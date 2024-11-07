  addi r1 r0 10 # 0b1010
  addi r2 r0 6  # 0b0110
  nand r3 r1 r2 # 0b1111 1111 1111 1101
  sys  EXIT # should return -3