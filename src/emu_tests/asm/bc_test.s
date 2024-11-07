  movi r1 0x8000
  add  r0 r1 r1
  bc   label # this should be taken
  movi r3 0xE
  sys  EXIT
label:
  add  r0 r0 r0
  bc   label2 # this branch should not be taken
  movi r3 0
  sys  EXIT
label2:
  movi r3 0xF
  sys  EXIT