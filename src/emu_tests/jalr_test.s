  movi r7 far_label
  jalr r7 r7
  sys  EXIT # should return 42

  .fill 0
  .fill 0
  .fill 0

far_label:
  addi r3 r0 21
  addi r3 r3 21
  jalr r0 r7 
