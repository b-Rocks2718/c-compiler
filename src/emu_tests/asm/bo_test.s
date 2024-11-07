  movi r1 0x7FFF
  movi r2 3
  add  r0 r1 r2
  bo   label # this should be taken
  movi r3 0xE
  sys  EXIT
label:
  add  r0 r2 r2
  bc   label2 # this branch should not be taken
  add  r0 r1 r1
  bc   label3 # this branch should not be taken
  movi r3 0
  sys  EXIT
label2:
  movi r3 0xF
  sys  EXIT
label3:
  movi r3 0xD
  sys  EXIT