#[cfg(test)]

use super::*;
#[test]
fn addi_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/addi_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 14);
}

#[test]
fn sw_lw_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/sw_lw_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 42);
}

#[test]
fn lui_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/lui_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 512);
}

#[test]
fn movi_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/movi_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 513);
}

#[test]
fn jalr_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/jalr_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 42);
}

#[test]
fn nand_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/nand_test.bin"));
  let result = cpu.run();
  assert_eq!(result as i16, -3);
}

#[test]
fn add_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/add_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 42);
}

#[test]
fn addc_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/addc_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0xAAAC);
}

#[test]
fn or_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/or_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 14);
}

#[test]
fn subc_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/subc_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0xFFFF);
}

#[test]
fn and_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/and_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 2);
}

#[test]
fn sub_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/sub_test.bin"));
  let result = cpu.run();
  assert_eq!(result as i16, -7);
}

#[test]
fn xor_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/xor_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 29);
}
#[test]
fn not_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/not_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 2);
}

#[test]
fn shl_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/shl_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0x5554);
}
#[test]
fn shr_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/shr_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0x2AAA);
}

#[test]
fn rotl_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/rotl_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0x5555);
}

#[test]
fn rotr_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/rotr_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0xAAAA);
}

#[test]
fn sshr_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/sshr_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0xD555);
}

#[test]
fn shrc_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/shrc_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0x8050);
}

#[test]
fn shlc_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/shlc_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0x00A1);
}

#[test]
fn beq_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/beq_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bp_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bp_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bn_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bn_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bc_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bc_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bo_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bo_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bne_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bne_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn jmp_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/jmp_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bnc_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bnc_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bg_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bg_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bge_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bge_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bl_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bl_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn ble_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/ble_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn ba_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/ba_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bae_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bae_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bb_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bb_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn bbe_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/bbe_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 0);
}

#[test]
fn collatz_test() {
  let mut cpu = Emulator::new(String::from("src/emu_tests/bin/collatz_test.bin"));
  let result = cpu.run();
  assert_eq!(result, 9232);
}