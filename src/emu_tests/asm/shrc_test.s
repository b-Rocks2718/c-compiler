  movi r4 0x5555
  movi r3 0x00A0
  shr  r4 r4
  shrc r3 r3
  sys  EXIT # should return 0x8050