  addi r1 r0 10
  addi r2 r0 11
  addi r3 r0 10
  cmp  r1 r2
  jmp  label # this should be taken
  movi r3 0xE
  sys  EXIT
label:
  cmp  r1 r3
  jmp  label2 # this branch should be taken
  movi r3 0xA
  sys  EXIT
label2:
  movi r3 0
  sys  EXIT