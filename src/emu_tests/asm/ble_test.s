  movi r1 0x8FFF
  movi r2 3
  cmp  r1 r2
  ble  label # this should be taken
  movi r3 0xE
  sys  EXIT
label:
  cmp  r2 r1
  ble  label2 # this branch should not be taken
  cmp  r0 r0
  ble  label3 # this branch should be taken
  movi r3 0xD
  sys  EXIT
label2:
  movi r3 0xF
  sys  EXIT
label3:
  movi r3 0
  sys  EXIT