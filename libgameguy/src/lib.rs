use std::fmt::UpperHex;

#[derive(Debug, Default, Clone)]
pub struct Emulator {
    cpu: Cpu,
    memory: Memory,
}

impl Emulator {
    pub fn from_rom_00(rom_00: [u8; ROM_00_SIZE]) -> Self {
        Emulator {
            cpu: Cpu::default(),
            memory: Memory::with_rom_00(rom_00),
        }
    }

    pub fn cpu(&self) -> &Cpu {
        &self.cpu
    }

    pub fn memory(&self) -> &Memory {
        &self.memory
    }

    pub fn step(&mut self) {
        self.cpu.step(&mut self.memory);
    }
}

#[derive(Debug, Clone)]
pub struct Cpu {
    pub af: FlagRegister,
    pub bc: Register,
    pub de: Register,
    pub hl: Register,
    pub sp: u16,
    pub pc: u16,
}

impl Default for Cpu {
    fn default() -> Self {
        Cpu {
            af: FlagRegister::default(),
            bc: Register::default(),
            de: Register::default(),
            hl: Register::default(),
            sp: 0,
            pc: 0,
        }
    }
}

impl Cpu {
    pub fn step<M>(&mut self, mut memory: &mut M)
    where
        M: std::ops::Index<u16, Output = u8> + std::ops::IndexMut<u16, Output = u8>,
    {
        let opcode_raw = memory[self.pc];
        let opcode = Opcode::from(opcode_raw);
        self.pc += 1;

        // Define closures for getting and setting u16 values in memory
        let get_u16 = |memory: &M, addr: u16| -> u16 {
            let lo = memory[addr] as u16;
            let hi = memory[addr + 1] as u16;
            (hi << 8) | lo
        };

        let set_u16 = |memory: &mut M, addr: u16, value: u16| {
            memory[addr] = (value & 0xFF) as u8;
            memory[addr + 1] = (value >> 8) as u8;
        };

        match opcode {
            Opcode::Noop => {}

            Opcode::LdCU8 => {
                let operand = memory[self.pc];
                self.pc += 1;

                *self.bc().lo_mut() = operand;
            }

            Opcode::LdHldA => {
                let hl = self.hl.combined();
                memory[hl] = self.af.hi();
                self.hl.set_u16(hl - 1);
            }

            Opcode::Prefix => {
                let opcode_raw = memory[self.pc];
                let opcode = PrefixeOpcode::from(opcode_raw);
                self.pc += 1;
                match opcode {
                    PrefixeOpcode::Bit7H => {
                        let h = self.hl().lo();

                        self.af.set_flag(Flag::Zero, 0b1000_0000 & h == 0);
                        self.af.set_flag(Flag::Subtraction, false);
                        self.af.set_flag(Flag::HalfCarry, true);
                    }
                }
            }

            Opcode::LdRHlA => {
                let ptr = self.hl.combined();
                memory[ptr] = self.af.hi();
            }

            Opcode::JrNzE8 => {
                let offset = memory[self.pc] as i8;
                self.pc += 1;

                if !self.af.get_flag(Flag::Zero) {
                    self.pc = if offset >= 0 {
                        self.pc + offset as u16
                    } else {
                        self.pc - (-offset as u16)
                    };
                }
            }

            Opcode::LdhCA => {
                let ptr = 0x00FF + self.bc.lo() as u16;
                memory[ptr] = self.af.hi();
            }
            Opcode::IncC => {
                let x = self.bc.lo_mut();
                let (new_x, overflowed) = x.overflowing_add(1);
                *x = new_x;
                if overflowed {
                    self.af.set_flag(Flag::Zero, true);
                    self.af.set_flag(Flag::HalfCarry, true);
                }
                self.af.set_flag(Flag::Subtraction, false);
            }

            Opcode::LdAU8 => {
                let val = memory[self.pc];
                self.pc += 1;
                let a = self.af.hi_mut();
                *a = val;
            }

            Opcode::DecB => {
                let b = self.bc.hi_mut();
                let (new, overflowed) = b.overflowing_sub(1);
                *b = new;
                if overflowed {
                    // Set HalfCarry flag if there was a borrow from bit 4 (i.e., if lower nibble was 0)
                    self.af.set_flag(Flag::HalfCarry, (*b & 0x0F) == 0x0F);
                }
                self.af.set_flag(Flag::Zero, *b == 0);
                self.af.set_flag(Flag::Subtraction, true);
            }

            Opcode::LdSpU16 => {
                self.sp = get_u16(memory, self.pc);
                self.pc += 2;
            }

            // INC A
            Opcode::IncA => {
                let a = self.af.hi_mut();
                let (new_a, overflowed) = a.overflowing_add(1);
                *a = new_a;
                if overflowed {
                    self.af.set_flag(Flag::Zero, true);
                    self.af.set_flag(Flag::HalfCarry, true);
                }
                self.af.set_flag(Flag::Subtraction, false);
            }
            // RET
            Opcode::Ret => {
                let value = get_u16(memory, self.sp);
                self.pc += 2;
                self.sp -= 2;
                self.pc = value;
            }
            Opcode::JpU16 => {
                let addr = get_u16(memory, self.pc);
                self.pc += 2;
                self.pc = addr;
            }
            Opcode::LdU16Sp => {
                let ptr = get_u16(memory, self.pc);
                self.pc += 2;
                let val = get_u16(memory, ptr);
                self.sp = val;
            }
            Opcode::LdDeU16 => {
                let value = get_u16(memory, self.pc);
                self.pc += 2;
                self.de.set_u16(value);
            }
            Opcode::CpAI8 => {
                let a = self.af.hi_mut();

                let v = memory[self.pc] as i8;
                self.pc += 1;

                let (new_a, overflowed) = if v >= 0 {
                    a.overflowing_sub(v as u8)
                } else {
                    a.overflowing_add(-v as u8)
                };

                *a = new_a;

                self.af.set_flag(Flag::Zero, new_a == 0);
                self.af.set_flag(Flag::Subtraction, true);
                self.af.set_flag(Flag::HalfCarry, overflowed);
                self.af.set_flag(Flag::Carry, overflowed);
            }

            Opcode::RetNz => {
                let value = get_u16(memory, self.sp);
                self.pc += 2;
                if !self.af.get_flag(Flag::Zero) {
                    self.sp += 2;
                    self.pc = value;
                }
            }
            Opcode::LdHlU16 => {
                let value = get_u16(memory, self.pc);
                self.pc += 2;
                self.hl.set_u16(value);
            }
            Opcode::LdBB => {
                *self.bc.hi_mut() = self.bc.hi();
            }
            Opcode::XorAA => {
                *self.af.hi_mut() = 0;
            }
            Opcode::LdBA => {
                *self.bc.hi_mut() = self.af.hi();
            }
            Opcode::LdhRU8A => {
                let operand = memory[self.pc];
                self.pc += 1;

                let ptr = 0xFF00 + operand as u16;
                memory[ptr] = self.af.hi();
            }
            Opcode::Rst38 => {
                self.sp -= 2;
                set_u16(memory, self.sp, self.pc);
                self.pc = 0x0038;
            }
            o => unimplemented!("{:?}", o),
        }
    }

    pub fn af(&self) -> FlagRegister {
        self.af
    }

    pub fn bc(&self) -> Register {
        self.bc
    }

    pub fn de(&self) -> Register {
        self.de
    }

    pub fn hl(&self) -> Register {
        self.hl
    }

    pub fn sp(&self) -> u16 {
        self.sp
    }

    pub fn pc(&self) -> u16 {
        self.pc
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct FlagRegister {
    inner: Register,
}

impl UpperHex for FlagRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02X}", self.inner)
    }
}

pub enum Flag {
    Zero,
    Subtraction,
    HalfCarry,
    Carry,
}

impl FlagRegister {
    pub fn hi(self) -> u8 {
        self.inner.hi()
    }

    pub fn hi_mut(&mut self) -> &mut u8 {
        self.inner.hi_mut()
    }

    pub fn get_flag(self, flag: Flag) -> bool {
        match flag {
            Flag::Zero => (self.inner.lo() & 0b1000_0000) > 0,
            Flag::Subtraction => (self.inner.lo() & 0b0100_0000) > 0,
            Flag::HalfCarry => (self.inner.lo() & 0b0010_0000) > 0,
            Flag::Carry => (self.inner.lo() & 0b0001_0000) > 0,
        }
    }

    pub fn set_flag(&mut self, flag: Flag, value: bool) {
        match flag {
            Flag::Zero => self.inner.lo = (self.inner.lo & !0b1000_0000) | (value as u8) << 7,
            Flag::Subtraction => {
                self.inner.lo = (self.inner.lo & !0b0100_0000) | ((value as u8) << 6)
            }
            Flag::HalfCarry => {
                self.inner.lo = (self.inner.lo & !0b0010_0000) | ((value as u8) << 5)
            }
            Flag::Carry => self.inner.lo = (self.inner.lo & !0b0001_0000) | ((value as u8) << 4),
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Register {
    lo: u8,
    hi: u8,
}

impl Register {
    pub fn hi(self) -> u8 {
        self.hi
    }

    pub fn hi_mut(&mut self) -> &mut u8 {
        &mut self.hi
    }

    pub fn lo_mut(&mut self) -> &mut u8 {
        &mut self.lo
    }

    pub fn lo(self) -> u8 {
        self.lo
    }

    pub fn set_u16(&mut self, value: u16) {
        let (hi, low) = split_u16(value);
        self.hi = hi;
        self.lo = low;
    }

    pub fn combined(self) -> u16 {
        ((self.hi as u16) << 8) | (self.lo as u16)
    }
}

impl UpperHex for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "hi {:02X} lo{:02X}", self.hi(), self.lo())
    }
}

pub const ROM_00_ADDRESS_START: u16 = 0x0000;
pub const ROM_00_SIZE: usize = ROM_NN_ADDRESS_START as usize - ROM_00_ADDRESS_START as usize;

pub const ROM_NN_ADDRESS_START: u16 = 0x4000;
pub const ROM_NN_SIZE: usize = VRAM_ADDRESS_START as usize - ROM_NN_ADDRESS_START as usize;

pub const VRAM_ADDRESS_START: u16 = 0x8000;
pub const VRAM_SIZE: usize = ERAM_ADDRESS_START as usize - VRAM_ADDRESS_START as usize;

pub const ERAM_ADDRESS_START: u16 = 0xA000;

pub const WRAM_00_ADDRESS_START: u16 = 0xC000;
pub const WRAM_00_SIZE: usize = WRAM_NN_ADDRESS_START as usize - WRAM_00_ADDRESS_START as usize;

pub const WRAM_NN_ADDRESS_START: u16 = 0xD000;

pub const IO_ADDRESS_START: u16 = 0xFF00;
pub const IO_SIZE: usize = HRAM_ADDRESS_START as usize - IO_ADDRESS_START as usize;

pub const HRAM_ADDRESS_START: u16 = 0xFF80;
pub const HRAM_SIZE: usize = IE_ADDRESS_START as usize - HRAM_ADDRESS_START as usize;
pub const IE_ADDRESS_START: u16 = 0xFFFF;

#[derive(Debug, Clone)]
pub struct Memory {
    rom_00: [u8; ROM_00_SIZE],
    rom_nn: [u8; ROM_NN_SIZE],
    vram: [u8; VRAM_SIZE],
    wram: [u8; WRAM_00_SIZE],
    io: [u8; IO_SIZE],
    hram: [u8; HRAM_SIZE],
    ie: u8,
}

impl Index<u16> for Memory {
    type Output = u8;
    fn index(&self, index: u16) -> &Self::Output {
        self.get_u8(index)
    }
}

impl IndexMut<u16> for Memory {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        self.get_u8_mut(index)
    }
}

impl Default for Memory {
    fn default() -> Self {
        Memory {
            rom_00: [0; ROM_00_SIZE],
            rom_nn: [0; ROM_NN_SIZE],
            vram: [0; VRAM_SIZE],
            wram: [0; WRAM_00_SIZE],
            io: [0; IO_SIZE],
            hram: [0; HRAM_SIZE],
            ie: 0,
        }
    }
}
/// Splits a u16 value into its high and low u8 bytes.
/// Returns (hi, lo), where hi is the high byte and lo is the low byte.
pub fn split_u16(value: u16) -> (u8, u8) {
    let hi = (value >> 8) as u8;
    let lo = (value & 0xFF) as u8;
    (hi, lo)
}

impl Memory {
    pub fn set_u8(&mut self, address: u16, value: u8) {
        match address {
            ROM_00_ADDRESS_START..ROM_NN_ADDRESS_START => {
                self.rom_00[(address - ROM_00_ADDRESS_START) as usize] = value;
            }
            ROM_NN_ADDRESS_START..VRAM_ADDRESS_START => {
                self.rom_nn[(address - ROM_NN_ADDRESS_START) as usize] = value;
            }
            VRAM_ADDRESS_START..ERAM_ADDRESS_START => {
                self.vram[(address - VRAM_ADDRESS_START) as usize] = value;
            }
            WRAM_00_ADDRESS_START..WRAM_NN_ADDRESS_START => {
                self.wram[(address - WRAM_00_ADDRESS_START) as usize] = value;
            }
            IO_ADDRESS_START..HRAM_ADDRESS_START => {
                self.io[(address - IO_ADDRESS_START) as usize] = value;
            }
            HRAM_ADDRESS_START..IE_ADDRESS_START => {
                self.hram[(address - HRAM_ADDRESS_START) as usize] = value;
            }
            IE_ADDRESS_START => self.ie = value,
            _ => panic!("Invalid address"),
        }
    }

    pub fn set_u16(&mut self, address: u16, value: u16) {
        let (hi, lo) = split_u16(value);
        self.set_u8(address + 1, hi);
        self.set_u8(address, lo);
    }

    pub fn get_u8(&self, address: u16) -> &u8 {
        match address {
            ROM_00_ADDRESS_START..ROM_NN_ADDRESS_START => {
                &self.rom_00[(address - ROM_00_ADDRESS_START) as usize]
            }
            ROM_NN_ADDRESS_START..VRAM_ADDRESS_START => {
                &self.rom_nn[(address - ROM_NN_ADDRESS_START) as usize]
            }
            VRAM_ADDRESS_START..ERAM_ADDRESS_START => {
                &self.vram[(address - VRAM_ADDRESS_START) as usize]
            }
            WRAM_00_ADDRESS_START..WRAM_NN_ADDRESS_START => {
                &self.wram[(address - WRAM_00_ADDRESS_START) as usize]
            }
            IO_ADDRESS_START..HRAM_ADDRESS_START => &self.io[(address - IO_ADDRESS_START) as usize],
            HRAM_ADDRESS_START..IE_ADDRESS_START => {
                &self.hram[(address - HRAM_ADDRESS_START) as usize]
            }
            IE_ADDRESS_START => &self.ie,
            a => panic!("Invalid address {:04X}", a),
        }
    }

    pub fn get_u8_mut(&mut self, address: u16) -> &mut u8 {
        match address {
            ROM_00_ADDRESS_START..ROM_NN_ADDRESS_START => {
                &mut self.rom_00[(address - ROM_00_ADDRESS_START) as usize]
            }
            ROM_NN_ADDRESS_START..VRAM_ADDRESS_START => {
                &mut self.rom_nn[(address - ROM_NN_ADDRESS_START) as usize]
            }
            VRAM_ADDRESS_START..ERAM_ADDRESS_START => {
                &mut self.vram[(address - VRAM_ADDRESS_START) as usize]
            }
            WRAM_00_ADDRESS_START..WRAM_NN_ADDRESS_START => {
                &mut self.wram[(address - WRAM_00_ADDRESS_START) as usize]
            }
            IO_ADDRESS_START..HRAM_ADDRESS_START => {
                &mut self.io[(address - IO_ADDRESS_START) as usize]
            }
            HRAM_ADDRESS_START..IE_ADDRESS_START => {
                &mut self.wram[(address - HRAM_ADDRESS_START) as usize]
            }
            IE_ADDRESS_START => &mut self.ie,
            a => panic!("Invalid address {:04X}", a),
        }
    }

    pub fn with_rom_00(rom_00: [u8; ROM_00_SIZE]) -> Self {
        Memory {
            rom_00,
            ..Default::default()
        }
    }
}

/// Packs two u8 values into a u16, with `hi` as the high byte and `lo` as the low byte.
pub fn pack_u8s(hi: u8, lo: u8) -> u16 {
    ((hi as u16) << 8) | (lo as u16)
}

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Noop = 0x00,
    LdhRU8A = 0xE0,
    IncC = 0x0C,
    LdCU8 = 0x0E,
    LdSpU16 = 0x31,
    IncA = 0x3c,
    LdBA = 0x47,
    Ret = 0xC9,
    JpU16 = 0xC3,
    LdU16Sp = 0x08,
    LdDeU16 = 0x11,
    LdHlU16 = 0x21,
    LdBB = 0x40,
    RetNz = 0xC0,
    CpAI8 = 0xFE,
    Rst38 = 0xFF,
    DecB = 0x05,
    JrNzE8 = 0x20,
    XorAA = 0xAF,
    LdAU8 = 0x3E,
    LdhCA = 0xE2,
    LdHldA = 0x32,
    Prefix = 0xCB,
    LdRHlA = 0x77,
}

pub enum PrefixeOpcode {
    Bit7H = 0x7C,
}

impl From<u8> for PrefixeOpcode {
    fn from(value: u8) -> Self {
        match value {
            0x7C => PrefixeOpcode::Bit7H,
            x => unimplemented!("{:04X}", x),
        }
    }
}

// The preferred trait to display an enum as a string in Rust is the `std::fmt::Display` trait.
// Here is an implementation for the Opcode enum:

use std::fmt;
use std::ops::{Index, IndexMut};

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Opcode::LdAU8 => "LD A,u8",
            Opcode::LdhRU8A => "LDH [u8], A",
            Opcode::LdRHlA => "LD [HL], A",
            Opcode::IncC => "INC C",
            Opcode::LdCU8 => "LD C,u8",
            Opcode::XorAA => "XOR A,A",
            Opcode::DecB => "DEC B",
            Opcode::JrNzE8 => "JR NZ, e8",
            Opcode::LdSpU16 => "LD SP,u16",
            Opcode::LdBB => "LD B,B",
            Opcode::LdBA => "LD B,A",
            Opcode::Noop => "Noop",
            Opcode::IncA => "IncA",
            Opcode::Ret => "Ret",
            Opcode::JpU16 => "Jp(U16)",
            Opcode::LdU16Sp => "Ld (u16) SP",
            Opcode::LdDeU16 => "LD DE,u16",
            Opcode::LdHlU16 => "LD HL,u16",
            Opcode::LdHldA => "LD [HL-],A",
            Opcode::RetNz => "RET NZ",
            Opcode::CpAI8 => "CP A,i8",
            Opcode::Rst38 => "RST $38",
            Opcode::LdhCA => "LDH [C], A",
            Opcode::Prefix => "Prefix",
        };
        write!(f, "{}", s)
    }
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        match value {
            0x00 => Opcode::Noop,
            0x77 => Opcode::LdRHlA,
            0x0C => Opcode::IncC,
            0x05 => Opcode::DecB,
            0x20 => Opcode::JrNzE8,
            0x31 => Opcode::LdSpU16,
            0x47 => Opcode::LdBA,
            0x3C => Opcode::IncA,
            0xC3 => Opcode::JpU16,
            0xC9 => Opcode::Ret,
            0x08 => Opcode::LdU16Sp,
            0x11 => Opcode::LdDeU16,
            0xE0 => Opcode::LdhRU8A,
            0x21 => Opcode::LdHlU16,
            0x40 => Opcode::LdBB,
            0xC0 => Opcode::RetNz,
            0xAF => Opcode::XorAA,
            0xFE => Opcode::CpAI8,
            0x32 => Opcode::LdHldA,
            0xFF => Opcode::Rst38,
            0x3E => Opcode::LdAU8,
            0xE2 => Opcode::LdhCA,
            0xCB => Opcode::Prefix,
            0x0E => Opcode::LdCU8,
            x => unimplemented!("{:02X}", x),
        }
    }
}

#[cfg(test)]
mod cpu_tests {
    use crate::Cpu;
    // A simple fake memory type that can be indexed by u16 for testing.
    #[derive(Debug, Clone)]
    struct FakeMemory {
        data: Vec<u8>,
    }

    impl From<Vec<u8>> for FakeMemory {
        fn from(value: Vec<u8>) -> Self {
            Self { data: value }
        }
    }

    impl std::ops::Index<u16> for FakeMemory {
        type Output = u8;

        fn index(&self, index: u16) -> &Self::Output {
            &self.data[index as usize]
        }
    }

    impl std::ops::IndexMut<u16> for FakeMemory {
        fn index_mut(&mut self, index: u16) -> &mut Self::Output {
            &mut self.data[index as usize]
        }
    }

    #[test]
    fn test_noop() {
        let mut cpu = Cpu {
            pc: 0,
            sp: 0,
            ..Default::default()
        };

        let mut memory = FakeMemory::from(vec![0x00, 0x00]);

        cpu.step(&mut memory);

        assert_eq!(cpu.pc(), 1);
    }

    #[test]
    fn test_jp_u16() {
        let mut cpu = Cpu {
            pc: 0,
            sp: 0,
            ..Default::default()
        };
        let mut memory = FakeMemory::from(vec![
            0x00, // Noop
            0xC3, // JR
            0x02, 0x01,
        ]);

        cpu.step(&mut memory); // skip noop to make sure it directly writes memory adress
        cpu.step(&mut memory);

        assert_eq!(cpu.pc(), 0x0102);
    }

    #[test]
    fn test_ld_hl_u16() {
        let mut cpu = Cpu {
            pc: 0,
            sp: 0,
            ..Default::default()
        };

        let mut memory = FakeMemory::from(vec![
            0x21, // LD HL,u16
            0x02, 0x01,
        ]);

        cpu.step(&mut memory);
        assert_eq!(cpu.hl().combined(), 0x0102);
        assert_eq!(cpu.hl().lo(), 0x02);
        assert_eq!(cpu.hl().hi(), 0x01);
    }

    #[test]
    fn test_ld_de_u16() {
        let mut cpu = Cpu {
            pc: 0,
            sp: 0,
            ..Default::default()
        };

        let mut memory = FakeMemory::from(vec![
            0x11, // LD DE,u16
            0x02, 0x01,
        ]);

        cpu.step(&mut memory);

        assert_eq!(cpu.de().combined(), 0x0102);
    }
}
