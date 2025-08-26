use std::fmt::{format, UpperHex};
use std::ops::{AddAssign, Index, IndexMut, SubAssign};
use std::{fmt, mem};

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
    pub sp: Register,
    pub pc: Register,
}

impl Default for Cpu {
    fn default() -> Self {
        Cpu {
            af: FlagRegister::default(),
            bc: Register::default(),
            de: Register::default(),
            hl: Register::default(),
            sp: Register::default(),
            pc: Register::default(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Bit(u8, R8),
    RlR8(R8),
}

impl TryFrom<u8> for PrefixOp {
    type Error = Box<dyn std::error::Error>;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use PrefixOp::*;
        use R8::*;
        match value {
            0x11 => Ok(RlR8(C)),
            0x17 => Ok(RlR8(A)),
            0x7C => Ok(Bit(7, H)),
            c => Err(format!("unknown prefix opcode {:02X}", c).into()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    SubAR8(R8),
    CallU16,
    Ret,
    LdIU16A,
    LdIHliA,
    IncR16(R16),
    PushR16(R16),
    LdR8R8(R8, R8),
    CpAU8,
    LdIHlA,
    LdAIR16(R16),
    IncR8(R8),
    LDHldA,
    LdR8U8(R8),
    JrI8,
    LdhAU8,
    PopR16(R16),
    LdR16U16(R16),
    LdIHlR8(R8),
    Rla,
    LdhU8A,
    LdhCA,
    LdIHlU8,
    DecR8(R8),
    Prefix,
    JrCcI8(Condition),
    XorR8R8(R8, R8),
}

#[derive(Debug, Clone, Copy)]
pub enum Condition {
    Nz, // Not Zero
    Z,  // Zero
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Condition::*;
        let st = match self {
            Nz => "NZ",
            Z => "Z",
        };

        write!(f, "{}", st)
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Op::*;
        let st = match self {
            SubAR8(rhs) => format!("SUB A,{rhs}"),
            LdhAU8 => format!("LD A,(FF00+u8)"),
            LdIU16A => format!("LD (u16),A"),
            JrI8 => format!("JR i8"),
            CpAU8 => format!("CP A,u8"),
            Ret => format!("RET"),
            IncR16(r) => format!("INC {r}"),
            LdIHliA => format!("LD (HL+),A"),
            LdIHlA => format!("LD (HL),A"),
            DecR8(r) => format!("DEC {r}"),
            PushR16(r) => format!("PUSH {r}"),
            PopR16(r) => format!("POP {r}"),
            LdR8R8(dst, src) => format!("LD {dst},{src}"),
            LdR8U8(dst) => format!("LD {dst},u8"),
            LdhU8A => format!("LD (FF00 + u8),A"),
            IncR8(dst) => format!("INC {}", dst),
            LdhCA => format!("LD (FF00+C),A"),
            LdR16U16(dst) => format!("LD {dst},u16"),
            LdIHlR8(src) => format!("LD [HL],{src}"),
            LdIHlU8 => format!("LD [HL],u8"),
            Prefix => format!("Prefix"),
            Rla => format!("RLA"),
            XorR8R8(dst, src) => format!("XOR {dst},{src}"),
            JrCcI8(c) => format!("JR {c},i8"),
            LDHldA => format!("LD [HL-],A"),
            LdAIR16(src) => format!("LD A,({src})"),
            CallU16 => format!("CALL u16"),
        };
        write!(f, "{}", st)
    }
}

impl TryFrom<u8> for Op {
    type Error = Box<dyn std::error::Error>;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use Condition::*;
        use Op::*;
        use R16::*;
        use R8::*;
        match value {
            0x22 => Ok(LdIHlA),
            0x04 => Ok(IncR8(B)),
            0x1E => Ok(LdR8U8(E)),
            0xF0 => Ok(LdhAU8),
            0x23 => Ok(IncR16(HL)),
            0x13 => Ok(IncR16(DE)),
            0x7B => Ok(LdR8R8(A, E)),
            0xFE => Ok(CpAU8),
            0xC9 => Ok(Ret),
            0x28 => Ok(JrCcI8(Z)),
            0xEA => Ok(LdIU16A),
            0x3D => Ok(DecR8(A)),
            0x0D => Ok(DecR8(C)),
            0x15 => Ok(DecR8(D)),
            0x16 => Ok(LdR8U8(D)),
            0x18 => Ok(JrI8),
            0x0C => Ok(IncR8(C)),
            0x24 => Ok(IncR8(H)),
            0x7c => Ok(LdR8R8(A, H)),
            0x90 => Ok(SubAR8(B)),
            0x1D => Ok(DecR8(E)),
            0x67 => Ok(LdR8R8(H, A)),
            0x57 => Ok(LdR8R8(D, A)),
            0x05 => Ok(DecR8(B)),
            0xC1 => Ok(PopR16(BC)),
            0x06 => Ok(LdR8U8(B)),
            0x17 => Ok(Rla),
            0xC5 => Ok(PushR16(BC)),
            0xCD => Ok(CallU16),
            0x4F => Ok(LdR8R8(C, A)),
            0x11 => Ok(LdR16U16(DE)),
            0x1A => Ok(LdAIR16(DE)),
            0xE0 => Ok(LdhU8A),
            0x31 => Ok(LdR16U16(SP)),
            0x32 => Ok(LDHldA),
            0x3E => Ok(LdR8U8(A)),
            0x0E => Ok(LdR8U8(C)),
            0x20 => Ok(JrCcI8(Nz)),
            0x21 => Ok(LdR16U16(HL)),
            0x77 => Ok(LdIHlA),
            0xAF => Ok(XorR8R8(A, A)),
            0xCB => Ok(Prefix),
            0xE2 => Ok(LdhCA),
            _ => Err(Box::from(format!("unknown opcode {:02X}", value))),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum R8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

impl fmt::Display for R8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use R8::*;
        let st = match self {
            A => "A",
            B => "B",
            C => "C",
            D => "D",
            E => "E",
            H => "H",
            L => "L",
        };
        write!(f, "{}", st)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum R16 {
    AF,
    BC,
    DE,
    HL,
    SP,
}

impl fmt::Display for R16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use R16::*;
        let st = match self {
            AF => "AF",
            BC => "BC",
            DE => "DE",
            HL => "HL",
            SP => "SP",
        };
        write!(f, "{}", st)
    }
}

impl Cpu {
    pub fn step<M>(&mut self, mut memory: &mut M)
    where
        M: std::ops::Index<u16, Output = u8> + std::ops::IndexMut<u16, Output = u8>,
    {
        let opcode_raw = memory[self.pc.into()];
        let opcode = Op::try_from(opcode_raw).expect("unknown opcode");
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

        use Op::*;
        use R8::*;
        match opcode {
            JrI8 => {
                let offset = memory[self.pc.into()] as i8;
                self.pc += 1;

                *self.pc.u16_mut() = self.pc.u16().wrapping_add_signed(offset as i16);
            }
            SubAR8(rhs) => {
                let old = self.af.hi();
                let rhs = self.r8(rhs);
                let new = old.wrapping_sub(rhs);
                *self.af.hi_mut() = new;

                // Set flags for SUB A, r8
                self.af.set_flag(Flag::Zero, new == 0);
                self.af.set_flag(Flag::Subtraction, true);
                self.af
                    .set_flag(Flag::HalfCarry, (old & 0x0F) < (rhs & 0x0F));
                self.af.set_flag(Flag::Carry, old < rhs);
            }
            LdhAU8 => {
                let op = memory[self.pc.into()];
                self.pc += 1;
                let ptr = 0xFF00 + op as u16;

                *self.af.hi_mut() = memory[ptr];
            }
            LdIU16A => {
                let ptr = get_u16(memory, self.pc.into());
                self.pc += 2;

                memory[ptr] = self.af.hi();
            }
            CpAU8 => {
                let op = memory[self.pc.into()];
                self.pc += 1;

                let value = self.af().hi().wrapping_sub(op);

                self.af.set_flag(Flag::Zero, value == 0);
                self.af.set_flag(Flag::Subtraction, true);
                self.af
                    .set_flag(Flag::HalfCarry, (self.af().hi() & 0x0F) < (op & 0x0F));
                self.af.set_flag(Flag::Carry, self.af().hi() < op);
            }
            Ret => {
                *self.pc.u16_mut() = get_u16(&mut memory, self.sp.into());
                self.sp += 2;
            }
            IncR16(r) => {
                let old = self.r16(r);
                let new = old.wrapping_add(1);
                *self.r16_mut(r) = new;

                self.af.set_flag(Flag::Zero, new == 0);
                self.af.set_flag(Flag::Subtraction, true);
                self.af.set_flag(Flag::HalfCarry, (old & 0x0F) == 0);
            }
            LdIHliA => {
                let ptr = self.hl.u16();
                *self.hl.u16_mut() += 1;

                memory[ptr] = self.af.hi();
            }
            Rla => {
                // Rotate left through carry for 8-bit register r
                let old_val = self.r8(A);
                let carry_in = if self.af.get_flag(Flag::Carry) { 1 } else { 0 };
                let new_val = (old_val << 1) | carry_in;
                let carry_out = (old_val & 0x80) != 0;

                *self.r8_mut(A) = new_val;

                self.af.set_flag(Flag::Zero, new_val == 0);
                self.af.set_flag(Flag::Subtraction, false);
                self.af.set_flag(Flag::HalfCarry, false);
                self.af.set_flag(Flag::Carry, carry_out);
            }
            DecR8(r) => {
                let old = self.r8(r);
                let new = old.wrapping_sub(1);
                *self.r8_mut(r) = new;

                self.af.set_flag(Flag::Zero, new == 0);
                self.af.set_flag(Flag::Subtraction, true);
                self.af.set_flag(Flag::HalfCarry, (old & 0x0F) == 0);
            }
            PopR16(r) => {
                *self.r16_mut(r) = get_u16(&memory, self.sp.into());
                self.sp += 2;
            }
            PushR16(r) => {
                let val = self.r16(r);
                self.sp -= 2;
                set_u16(&mut memory, self.sp.into(), val);
            }
            CallU16 => {
                let destination_addr = get_u16(memory, self.pc.into());
                self.pc += 2;

                self.sp -= 2;
                set_u16(&mut memory, self.sp.into(), self.pc.into());

                *self.pc.u16_mut() = destination_addr;
            }
            IncR8(r) => {
                let old = self.r8(r);
                let new = old.wrapping_add(1);
                *self.r8_mut(r) = new;

                self.af.set_flag(Flag::Zero, new == 0);
                self.af.set_flag(Flag::Subtraction, false);
                self.af.set_flag(Flag::HalfCarry, old & 0x0F == 0b0000_1111); // check if we overflowed the lower nibble
            }
            LdR8U8(r) => {
                let val = memory[self.pc.into()];
                self.pc += 1;
                let r_mut = self.r8_mut(r);
                *r_mut = val;
            }
            LdIHlA => {
                let ptr = self.hl.u16();
                memory[ptr] = self.af.hi();
            }
            LdR16U16(dst) => {
                let val = get_u16(memory, self.pc.into());
                self.pc += 2;
                *self.r16_mut(dst) = val;
            }
            XorR8R8(dst, src) => {
                *self.r8_mut(dst) ^= self.r8(src);
            }
            LdhCA => {
                let ptr = 0xFF00 + self.bc.hi() as u16;
                memory[ptr] = self.af.hi();
            }
            LdhU8A => {
                let op = memory[self.pc.into()];
                self.pc += 1;

                let ptr = 0xFF00 + op as u16;
                memory[ptr] = self.af.hi();
            }
            LDHldA => {
                let ptr = self.hl.u16();
                memory[ptr] = self.af.hi();
                *self.hl.u16_mut() -= 1;
            }
            JrCcI8(cond) => {
                let op = memory[self.pc.into()] as i8;
                self.pc += 1;

                use Condition::*;
                let should_jump = match cond {
                    Nz => self.af.get_flag(Flag::Zero),
                    Z => !self.af.get_flag(Flag::Zero),
                };

                if should_jump {
                    *self.pc.u16_mut() = self.pc.u16().wrapping_add_signed(op as i16)
                }
            }
            Prefix => {
                let opcode_raw = memory[self.pc.into()];
                self.pc += 1;
                let opcode = PrefixOp::try_from(opcode_raw).expect("unknown opcode");
                use PrefixOp::*;
                match opcode {
                    RlR8(r) => {
                        // Rotate left through carry for 8-bit register r
                        let old_val = self.r8(r);
                        let carry_in = if self.af.get_flag(Flag::Carry) { 1 } else { 0 };
                        let new_val = (old_val << 1) | carry_in;
                        let carry_out = (old_val & 0x80) != 0;

                        *self.r8_mut(r) = new_val;

                        self.af.set_flag(Flag::Zero, new_val == 0);
                        self.af.set_flag(Flag::Subtraction, false);
                        self.af.set_flag(Flag::HalfCarry, false);
                        self.af.set_flag(Flag::Carry, carry_out);
                    }
                    Bit(n, r) => {
                        let mask = 1u8 << n;
                        let r = self.r8(r);

                        self.af.set_flag(Flag::Zero, r & mask > 0);
                        self.af.set_flag(Flag::Subtraction, false);
                        self.af.set_flag(Flag::HalfCarry, true);
                    }
                }
            }
            LdR8R8(dst, src) => {
                *self.r8_mut(dst) = self.r8(src);
            }
            LdAIR16(src) => {
                let ptr = self.r16(src);
                *self.af.hi_mut() = memory[ptr];
            }
            LdIHlR8(src) => todo!(),
            LdIHlU8 => todo!(),
        }
    }
    fn r16(&mut self, r: R16) -> u16 {
        use R16::*;
        match r {
            HL => self.hl.u16(),
            AF => unreachable!(),
            BC => self.bc.u16(),
            DE => self.de.u16(),
            SP => self.sp.u16(),
        }
    }

    fn r16_mut(&mut self, r: R16) -> U16Ref {
        use R16::*;
        match r {
            HL => self.hl.u16_mut(),
            AF => unreachable!(),
            BC => self.bc.u16_mut(),
            DE => self.de.u16_mut(),
            SP => self.sp.u16_mut(),
        }
    }

    fn r8(&self, r: R8) -> u8 {
        use R8::*;
        match r {
            A => self.af.hi(),
            B => self.bc.hi(),
            C => self.bc.lo(),
            D => self.de.hi(),
            E => self.de.lo(),
            H => self.hl.hi(),
            L => self.hl.lo(),
        }
    }

    fn r8_mut(&mut self, r: R8) -> &mut u8 {
        use R8::*;
        match r {
            A => self.af.hi_mut(),
            B => self.bc.hi_mut(),
            C => self.bc.lo_mut(),
            D => self.de.hi_mut(),
            E => self.de.lo_mut(),
            H => self.hl.hi_mut(),
            L => self.hl.lo_mut(),
        }
    }

    pub fn af(&self) -> &FlagRegister {
        &self.af
    }

    pub fn bc(&self) -> &Register {
        &self.bc
    }

    pub fn de(&self) -> &Register {
        &self.de
    }

    pub fn hl(&self) -> &Register {
        &self.hl
    }

    pub fn sp(&self) -> &Register {
        &self.sp
    }

    pub fn pc(&self) -> &Register {
        &self.pc
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

impl Into<u16> for Register {
    fn into(self) -> u16 {
        self.u16()
    }
}

impl SubAssign<u16> for Register {
    fn sub_assign(&mut self, rhs: u16) {
        self.set_u16(self.u16() - rhs);
    }
}

impl AddAssign<u16> for Register {
    fn add_assign(&mut self, rhs: u16) {
        self.set_u16(self.u16() + rhs);
    }
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

    pub fn u16(self) -> u16 {
        ((self.hi as u16) << 8) | (self.lo as u16)
    }

    pub fn u16_mut(&mut self) -> U16Ref<'_> {
        let val = self.u16();
        U16Ref {
            register: self,
            cached_value: val,
        }
    }
}

pub struct U16Ref<'a> {
    register: &'a mut Register,
    cached_value: u16,
}

impl<'a> std::ops::Deref for U16Ref<'a> {
    type Target = u16;
    fn deref(&self) -> &Self::Target {
        &self.cached_value
    }
}

impl<'a> std::ops::DerefMut for U16Ref<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cached_value
    }
}

impl<'a> Drop for U16Ref<'a> {
    fn drop(&mut self) {
        self.register.set_u16(self.cached_value);
    }
}

impl UpperHex for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "hi {:02X} lo{:02X} combined {:04X}",
            self.hi(),
            self.lo(),
            self.u16()
        )
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
                &mut self.hram[(address - HRAM_ADDRESS_START) as usize]
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

// The preferred trait to display an enum as a string in Rust is the `std::fmt::Display` trait.
// Here is an implementation for the Opcode enum:

// #[cfg(test)]
// mod cpu_tests {
//     use crate::Cpu;
//     // A simple fake memory type that can be indexed by u16 for testing.
//     #[derive(Debug, Clone)]
//     struct FakeMemory {
//         data: Vec<u8>,
//     }

//     impl From<Vec<u8>> for FakeMemory {
//         fn from(value: Vec<u8>) -> Self {
//             Self { data: value }
//         }
//     }

//     impl std::ops::Index<u16> for FakeMemory {
//         type Output = u8;

//         fn index(&self, index: u16) -> &Self::Output {
//             &self.data[index as usize]
//         }
//     }

//     impl std::ops::IndexMut<u16> for FakeMemory {
//         fn index_mut(&mut self, index: u16) -> &mut Self::Output {
//             &mut self.data[index as usize]
//         }
//     }

//     #[test]
//     fn test_noop() {
//         let mut cpu = Cpu {
//             ..Default::default()
//         };

//         let mut memory = FakeMemory::from(vec![0x00, 0x00]);

//         cpu.step(&mut memory);

//         assert_eq!(cpu.pc().into(), 1);
//     }

//     #[test]
//     fn test_jp_u16() {
//         let mut cpu = Cpu {
//             ..Default::default()
//         };
//         let mut memory = FakeMemory::from(vec![
//             0x00, // Noop
//             0xC3, // JR
//             0x02, 0x01,
//         ]);

//         cpu.step(&mut memory); // skip noop to make sure it directly writes memory adress
//         cpu.step(&mut memory);

//         assert_eq!(cpu.pc().into(), 0x0102);
//     }

//     #[test]
//     fn test_ld_hl_u16() {
//         let mut cpu = Cpu {
//             ..Default::default()
//         };

//         let mut memory = FakeMemory::from(vec![
//             0x21, // LD HL,u16
//             0x02, 0x01,
//         ]);

//         cpu.step(&mut memory);
//         assert_eq!(cpu.hl().u16(), 0x0102);
//         assert_eq!(cpu.hl().lo(), 0x02);
//         assert_eq!(cpu.hl().hi(), 0x01);
//     }

//     #[test]
//     fn test_ld_de_u16() {
//         let mut cpu = Cpu {
//             ..Default::default()
//         };

//         let mut memory = FakeMemory::from(vec![
//             0x11, // LD DE,u16
//             0x02, 0x01,
//         ]);

//         cpu.step(&mut memory);

//         assert_eq!(cpu.de().u16(), 0x0102);
//     }
// }
