  addi r4 r0 10
  addi r5 r0 42
  sw   r5 r4 15 # store at address 25
  lw   r3 r0 25
  sys  EXIT     # should return 42