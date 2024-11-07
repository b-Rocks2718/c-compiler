  addi r1 r0 10
  addi r2 r0 11
  addi r3 r0 9
  cmp  r3 r1
  bn   label # this should be taken
  movi r3 0xE
  sys  EXIT
label:
  cmp  r2 r1
  bn   label2 # this branch should not be taken
  cmp  r1 r1
  bn   label3 # this branch should not be taken
  movi r3 0
  sys  EXIT
label2:
  movi r3 0xF
  sys  EXIT
label3:
  movi r3 0xD
  sys  EXIT