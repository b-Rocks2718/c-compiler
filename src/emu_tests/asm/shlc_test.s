  movi r4 0xAAAA
  movi r3 0x0050
  shl  r4 r4
  shlc r3 r3
  sys  EXIT # should return 0x00A1