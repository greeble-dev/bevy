//! Utilities for reading and writing binary data.

use std::io::Write;

// Helper for reading things out of a byte slice.
//
// XXX TODO: Surely this exists somewhere as a library? Or are there better
// approaches? Contrast with `MeshletMeshLoader` - fairly similar, but uses
// `Reader` directly plus a few helpers. Also see `byteorder` crate
// (https://docs.rs/byteorder/latest/byteorder/trait.ReadBytesExt.html)
pub struct BlobReader<'a> {
    data: &'a [u8],
    cursor: usize,
}

impl<'a> BlobReader<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, cursor: 0 }
    }

    pub fn bytes(&mut self, num: usize) -> Option<&'a [u8]> {
        let range = self.cursor..(self.cursor + num);
        self.cursor = range.end;
        self.data.get(range)
    }

    pub fn bytes_const<const NUM: usize>(&mut self) -> Option<&'a [u8; NUM]> {
        <&[u8; NUM]>::try_from(self.bytes(NUM)?).ok()
    }

    pub fn bytes_sized(&mut self) -> Option<&'a [u8]> {
        let len = self.u64()? as usize;
        self.bytes(len)
    }

    pub fn u64(&mut self) -> Option<u64> {
        let bytes = *self.bytes_const::<{ size_of::<u64>() }>()?;
        Some(u64::from_le_bytes(bytes))
    }

    pub fn u16(&mut self) -> Option<u16> {
        let bytes = *self.bytes_const::<{ size_of::<u16>() }>()?;
        Some(u16::from_le_bytes(bytes))
    }

    pub fn string(&mut self) -> Option<&'a str> {
        let len = self.u64()? as usize;
        str::from_utf8(self.bytes(len)?).ok()
    }
}

pub struct BlobWriter<'a> {
    writer: &'a mut dyn Write,
}

impl<'a> BlobWriter<'a> {
    pub fn new(writer: &'a mut dyn Write) -> Self {
        Self { writer }
    }

    pub fn bytes(&mut self, bytes: &[u8]) {
        self.writer.write_all(bytes).expect("TODO");
    }

    pub fn bytes_sized(&mut self, bytes: &[u8]) {
        self.u64(bytes.len() as u64);
        self.bytes(bytes);
    }

    pub fn u64(&mut self, value: u64) {
        self.writer.write_all(&value.to_le_bytes()).expect("TODO");
    }

    pub fn u16(&mut self, value: u16) {
        self.writer.write_all(&value.to_le_bytes()).expect("TODO");
    }

    pub fn string(&mut self, string: &str) {
        self.bytes_sized(string.as_bytes());
    }
}
