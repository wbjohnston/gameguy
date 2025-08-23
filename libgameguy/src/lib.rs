use std::fmt::UpperHex;

#[derive(Debug, Default, Clone)]
pub struct Emulator {
    cpu: Cpu,
}

impl Emulator {
    pub fn cpu(&self) -> &Cpu {
        &self.cpu
    }
}

#[derive(Debug, Default, Clone)]
pub struct Cpu {
    pub af: FlagRegister,
    pub bc: Register,
    pub de: Register,
    pub hl: Register,
    pub sp: u16,
    pub pc: u16,
}

impl Cpu {
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
    inner: u16,
}

impl UpperHex for FlagRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04X}", self.inner)
    }
}

impl From<u16> for FlagRegister {
    fn from(value: u16) -> Self {
        FlagRegister { inner: value }
    }
}

impl FlagRegister {
    pub fn hi(self) -> u8 {
        (self.inner >> 8) as u8
    }

    pub fn z(self) -> bool {
        (self.inner & 0b0000_0000_1000_0000) > 0
    }

    pub fn n(self) -> bool {
        (self.inner & 0b0000_0000_0100_0000) > 0
    }

    pub fn h(self) -> bool {
        (self.inner & 0b0000_0000_0010_0000) > 0
    }

    pub fn c(self) -> bool {
        (self.inner & 0b0000_0000_0001_0000) > 0
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Register {
    inner: u16,
}

impl Register {
    pub fn hi(self) -> u8 {
        (self.inner >> 8) as u8
    }

    pub fn lo(self) -> u8 {
        (self.inner & 0x00FF) as u8
    }
}

impl UpperHex for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04X}", self.inner)
    }
}

impl From<u16> for Register {
    fn from(value: u16) -> Self {
        Register { inner: value }
    }
}

#[cfg(test)]
mod test_flag_register {
    use crate::FlagRegister;

    fn setup() -> FlagRegister {
        //                  |hi-------|znhc ????
        FlagRegister::from(0b1111_1111_1010_1010)
    }

    #[test]
    fn test_z() {
        let r = setup();
        assert_eq!(r.z(), true);
    }

    #[test]
    fn test_n() {
        let r = setup();
        assert_eq!(r.n(), false);
    }

    #[test]
    fn test_h() {
        let r = setup();
        assert_eq!(r.h(), true);
    }

    #[test]
    fn test_c() {
        let r = setup();
        assert_eq!(r.c(), false);
    }

    #[test]
    fn test_hi() {
        let r = setup();
        assert_eq!(r.hi(), 0xFF);
    }
}

#[cfg(test)]
mod test_regster {
    use super::*;

    fn setup() -> Register {
        Register::from(0x1122)
    }

    #[test]
    pub fn test_register_hi() {
        let r = setup();
        assert_eq!(r.hi(), 0x11);
    }

    #[test]
    pub fn test_register_lo() {
        let r = setup();
        assert_eq!(r.lo(), 0x22);
    }
}
