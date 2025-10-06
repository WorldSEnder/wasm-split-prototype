//! Customized symbol parsing due to bytecodealliance/wasm-tools#2332
//! This code replaces some calls to read_string with read_string_unlimited compared to the original implementation

use eyre::{bail, Result};
use wasmparser::{
    BinaryReader, FromReader, SectionLimited, Segment, Subsection, Subsections, SymbolFlags,
    SymbolInfo as OrigSymbolInfo,
};

pub struct SymbolInfo<'a>(OrigSymbolInfo<'a>);

impl<'a> SymbolInfo<'a> {
    pub fn into_inner(self) -> OrigSymbolInfo<'a> {
        self.0
    }
}

impl<'a> FromReader<'a> for SymbolInfo<'a> {
    fn from_reader(reader: &mut BinaryReader<'a>) -> wasmparser::Result<Self> {
        const SYMTAB_FUNCTION: u8 = 0;
        const SYMTAB_DATA: u8 = 1;
        const SYMTAB_GLOBAL: u8 = 2;
        //const SYMTAB_SECTION: u8 = 3;
        const SYMTAB_EVENT: u8 = 4;
        const SYMTAB_TABLE: u8 = 5;

        let backup = reader.clone();
        let kind = reader.read_u8()?;
        let flags: SymbolFlags = reader.read()?;
        let defined = !flags.contains(SymbolFlags::UNDEFINED);
        let explicit_name = flags.contains(SymbolFlags::EXPLICIT_NAME);

        // https://github.com/WebAssembly/wabt/blob/1.0.34/src/binary-writer.cc#L1226
        match kind {
            SYMTAB_FUNCTION | SYMTAB_GLOBAL | SYMTAB_EVENT | SYMTAB_TABLE => {
                let index = reader.read_var_u32()?;
                let name = match defined || explicit_name {
                    true => Some(reader.read_unlimited_string()?),
                    false => None,
                };
                Ok(match kind {
                    SYMTAB_FUNCTION => Self(OrigSymbolInfo::Func { flags, index, name }),
                    SYMTAB_GLOBAL => Self(OrigSymbolInfo::Global { flags, index, name }),
                    SYMTAB_EVENT => Self(OrigSymbolInfo::Event { flags, index, name }),
                    SYMTAB_TABLE => Self(OrigSymbolInfo::Table { flags, index, name }),
                    _ => unreachable!(),
                })
            }
            SYMTAB_DATA => {
                let name = reader.read_unlimited_string()?;
                let data = match defined {
                    true => Some(reader.read()?),
                    false => None,
                };
                Ok(Self(OrigSymbolInfo::Data {
                    flags,
                    name,
                    symbol: data,
                }))
            }
            _ => {
                *reader = backup;
                Ok(Self(OrigSymbolInfo::from_reader(reader)?))
            }
        }
    }
}

pub enum Linking<'a> {
    SegmentInfo(SectionLimited<'a, Segment<'a>>),
    SymbolTable(SectionLimited<'a, SymbolInfo<'a>>),
    Unimportant { _ty: u8, _reader: BinaryReader<'a> },
}

impl<'a> Subsection<'a> for Linking<'a> {
    fn from_reader(id: u8, reader: BinaryReader<'a>) -> wasmparser::Result<Self> {
        Ok(match id {
            5 => Self::SegmentInfo(SectionLimited::new(reader)?),
            //6 => Self::InitFuncs(InitFuncMap::new(reader)?),
            //7 => Self::ComdatInfo(ComdatMap::new(reader)?),
            8 => Self::SymbolTable(SectionLimited::new(reader)?),
            ty => Self::Unimportant {
                _ty: ty,
                _reader: reader,
            },
        })
    }
}

pub struct LinkingSectionReader<'a> {
    sections: Subsections<'a, Linking<'a>>,
}

impl<'a> LinkingSectionReader<'a> {
    pub fn new(mut reader: BinaryReader<'a>) -> Result<Self> {
        let offset = reader.original_position();
        let version = reader.read_var_u32()?;
        if version != 2 {
            bail!("unsupported linking section version: {version} at {offset:x}");
        }
        Ok(Self {
            sections: Subsections::new(reader),
        })
    }
    pub fn subsections(&self) -> Subsections<'a, Linking<'a>> {
        self.sections.clone()
    }
}
