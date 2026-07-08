use eyre::bail;
use gimli::Section;
use wasm_encoder::CustomSection;

use crate::dep_graph::DepNode;
use crate::read::DwarfReader;
use crate::reloc;
use crate::reloc::DataDetails;
use crate::reloc::RelocDetails;
use crate::reloc::RelocInfo;
use crate::reloc::RelocTarget;

use super::ModuleEmitState;
use super::Result;

struct DwarfRelocTarget<'m, 'a> {
    module: &'m ModuleEmitState<'a>,
}

const RELOC_TO_TOMBSTONE_ADDRESS: Option<usize> = Some(reloc::SENTINEL_UNDEF);

impl RelocTarget for DwarfRelocTarget<'_, '_> {
    const SENTINEL_UNDEF: bool = true;
    fn reloc_value(&self, reloc: RelocDetails<'_>) -> Result<Option<usize>> {
        let reloc = match reloc {
            RelocDetails::GlobalIndex(_) => return self.module.reloc_value(reloc),
            RelocDetails::FunctionOffset(details) => {
                let local_def = self
                    .module
                    .dep_to_local_index
                    .get(&DepNode::Function(details.index));
                let local_offset = local_def
                    .and_then(|local_def| self.module.function_offset_hint.get(&local_def));
                match local_offset {
                    Some(offset) => Some(self.module.function_header_len + offset),
                    None => RELOC_TO_TOMBSTONE_ADDRESS,
                }
            }
            RelocDetails::MemoryAddr(DataDetails {
                definition: None, ..
            }) => None, // undefined symbols don't get relocated
            RelocDetails::MemoryAddr(DataDetails {
                definition: Some(symbol),
                symbol_index,
                ..
            }) => match self
                .module
                .emit_state
                .data_relocations
                .find_relocated_address(symbol_index, symbol)
            {
                Ok(address) => address,
                _ => RELOC_TO_TOMBSTONE_ADDRESS,
            },
            // We do not move data in custom sections around.
            // TODO: assert that the addressed section is indeed one of our debug sections?
            RelocDetails::SectionOffset(details) => {
                let _ = details;
                None
            }
            _ => bail!("unexpected reloc in debug section: {:?}", reloc),
        };
        Ok(reloc)
    }
}

fn write_relocate_dwarf_section<'a, S: Section<DwarfReader<'a>>>(
    module: &mut ModuleEmitState<'a>,
    section: &S,
) -> Result<Vec<u8>> {
    let reader = section.reader();
    let input_range = reader.range();
    if input_range.is_empty() {
        // We emit empty sections
        return Ok(vec![]);
    }
    let target = DwarfRelocTarget { module };
    let reloc_data = RelocInfo::get_relocated_data(&module.input_module, input_range, &target)?;
    module.output_module.section(&CustomSection {
        name: S::section_name().into(),
        data: (&reloc_data).into(),
    });
    Ok(reloc_data)
}

pub fn emit_debug_info(module: &mut ModuleEmitState<'_>) -> Result<()> {
    let crate::read::DwarfState::Inline(input_dwarf) = &module.input_module.dwarf else {
        return Ok(());
    };
    let mut error_writer = ErrorWriter::new(std::io::BufWriter::new(std::io::stderr()));
    if module.emit_state.input_options.strict_tests && module.is_main() {
        validate_info(&mut error_writer, input_dwarf.borrow(|v| v.clone()));
        validate_line_progs(&mut error_writer, input_dwarf.borrow(|v| v.clone()));
        if !error_writer.check_valid_and_reset() {
            tracing::warn!("original debug info didn't pass validation!");
        }
    }

    let mut validate = gimli::DwarfSections::default();
    // There is no strict order defined for these, but let's try to go from common to less useful
    // Further, some sections are needed to successfully parse other ones, define those in order of dependency
    // All section names (included commented out ones) have been taken from the table of section version numbers
    // of the current DWARF 6 draft.
    macro_rules! write_relocatable_section {
        ($name:ident in $input:expr) => {
            validate.$name = write_relocate_dwarf_section(module, &$input.$name)?.into()
        };
    }
    write_relocatable_section!(debug_abbrev in input_dwarf);
    write_relocatable_section!(debug_info   in input_dwarf);

    write_relocatable_section!(debug_line     in input_dwarf);
    write_relocatable_section!(debug_loc      in input_dwarf);
    write_relocatable_section!(debug_ranges   in input_dwarf);
    write_relocatable_section!(debug_str      in input_dwarf);

    write_relocatable_section!(debug_addr        in input_dwarf);
    write_relocatable_section!(debug_aranges     in input_dwarf);
    // write_relocatable_section!(debug_frame     in input_dwarf); // as of now not supported, no exception handling in wasm
    write_relocatable_section!(debug_line_str    in input_dwarf);
    write_relocatable_section!(debug_loclists    in input_dwarf);
    write_relocatable_section!(debug_macinfo     in input_dwarf);
    write_relocatable_section!(debug_macro       in input_dwarf);
    write_relocatable_section!(debug_names       in input_dwarf);
    // write_relocatable_section!(debug_pubnames    in input_dwarf); // old, currently unused and not present in dwarf 5+
    // write_relocatable_section!(debug_pubtypes    in input_dwarf); // same as above
    write_relocatable_section!(debug_rnglists    in input_dwarf);
    write_relocatable_section!(debug_str_offsets in input_dwarf);
    // write_relocatable_section!(debug_sup         in input_dwarf); // supplemental files not supported at the moment
    write_relocatable_section!(debug_types       in input_dwarf);

    // You can dump the contained dwarf sections with `llvm-dwarfdump` which can read wasm object files
    // TODO: the debugging information is currently NOT stripped.
    // Downstream tools are required to understand tombstone markers in lineprogs and we also copy all bytes into all modules
    // With more processing, we could garbage collect the output and strip it further.
    // This would require more work though, and seems only worth for release builds with debugging information, or misbehaving
    // tools. At least one issue https://github.com/emscripten-core/emscripten/issues/23710 points out that support might
    // not be universal and we could put in more effort to clean up the relocated information.

    if module.emit_state.input_options.strict_tests {
        validate_info(
            &mut error_writer,
            validate.borrow(|v| gimli::EndianSlice::new(&v[..], gimli::LittleEndian)),
        );
        validate_line_progs(
            &mut error_writer,
            validate.borrow(|v| gimli::EndianSlice::new(&v[..], gimli::LittleEndian)),
        );
        if !error_writer.check_valid_and_reset() {
            bail!("transformed debug info didn't pass validation!");
        }
    }
    Ok(())
}

struct UnitSummary {
    // True if we successfully parsed all the DIEs and attributes in the compilation unit
    internally_valid: bool,
    offset: gimli::DebugInfoOffset,
    die_offsets: Vec<gimli::UnitOffset>,
    global_die_references: Vec<(gimli::UnitOffset, gimli::DebugInfoOffset)>,
}

struct ErrorWriter<W> {
    inner: std::sync::Mutex<(W, usize)>,
}
impl<W> ErrorWriter<W> {
    fn new(w: W) -> Self {
        Self {
            inner: std::sync::Mutex::new((w, 0)),
        }
    }
    fn check_valid_and_reset(&mut self) -> bool {
        let mut lock = self.inner.lock().unwrap();
        std::mem::take(&mut lock.1) == 0
    }
}

impl<W: std::io::Write + Send> std::io::Write for ErrorWriter<W> {
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::io::Result<()> {
        let mut lock = self.inner.lock().unwrap();
        writeln!(&mut lock.0, "DWARF error: {}", args)?;
        lock.1 += 1;
        Ok(())
    }

    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut lock = self.inner.lock().unwrap();
        let len = lock.0.write(buf)?;
        lock.1 += 1;
        Ok(len)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        let mut lock = self.inner.lock().unwrap();
        lock.0.flush()
    }
}

fn validate_line_progs<W, R>(w: &mut ErrorWriter<W>, dwarf: gimli::Dwarf<R>)
where
    W: std::io::Write + Send,
    R: gimli::Reader<Offset = usize>,
{
    use std::io::Write;
    let mut line_prog_offsets = vec![];
    for unit in dwarf.units() {
        let unit = unit.and_then(|unit| dwarf.unit(unit));
        let unit = match unit {
            Ok(unit) => unit,
            Err(err) => {
                let _ = writeln!(w, "error when reading unit: {err}");
                continue;
            }
        };
        let address_size = unit.address_size();
        let comp_dir = &unit.comp_dir;
        let comp_name = &unit.name;
        let mut entries = unit.entries();
        while let Some(die) = entries.next_dfs().transpose() {
            let die = match die {
                Ok(die) => die,
                Err(err) => {
                    let _ = writeln!(w, "error reading die: {err}");
                    continue;
                }
            };
            for attr in die.attrs() {
                if let gimli::AttributeValue::DebugLineRef(dlp_offset) = attr.value() {
                    line_prog_offsets.push((
                        dlp_offset,
                        address_size,
                        comp_dir.clone(),
                        comp_name.clone(),
                    ));
                }
            }
        }
    }
    for (offset, address_size, comp_dir, comp_name) in line_prog_offsets {
        let program = dwarf
            .debug_line
            .program(offset, address_size, comp_dir, comp_name);
        let program = match program {
            Ok(program) => program,
            Err(err) => {
                let _ = writeln!(w, "error reading program 0x{:x}: {err}", offset.0);
                continue;
            }
        };
        let mut rows = program.rows();
        while let Some(row) = rows.next_row().transpose() {
            if let Err(err) = row {
                let _ = writeln!(w, "invalid line in program 0x{:x}: {err}", offset.0);
            }
        }
    }
}

// This is almost verbatim from gimli examples, copied with slight modifications
// under Apache License. Their MIT license quotes "Copyright (c) 2015 The Rust Project Developers"
// though that is a bit questionable attribution. The Apache license has not been filled in.
fn validate_info<W, R>(w: &mut ErrorWriter<W>, dwarf: gimli::Dwarf<R>)
where
    W: std::io::Write + Send,
    R: gimli::Reader<Offset = usize>,
{
    use std::io::Write;
    let debug_info = &dwarf.debug_info;
    let debug_abbrev = &dwarf.debug_abbrev;

    let mut units = Vec::new();
    let mut units_iter = debug_info.units();
    let mut last_offset = 0;
    loop {
        let u = match units_iter.next() {
            Err(err) => {
                let _ = writeln!(
                    w,
                    "Can't read unit header at offset {:#x}, stopping reading units: {}",
                    last_offset, err
                );
                break;
            }
            Ok(None) => break,
            Ok(Some(u)) => u,
        };
        last_offset = u.offset().0 + u.length_including_self();
        units.push(u);
    }
    let process_unit = |unit: gimli::UnitHeader<R>| -> UnitSummary {
        let unit_offset = unit.debug_info_offset().unwrap();
        let mut ret = UnitSummary {
            internally_valid: false,
            offset: unit_offset,
            die_offsets: Vec::new(),
            global_die_references: Vec::new(),
        };
        let abbrevs = match unit.abbreviations(debug_abbrev) {
            Ok(abbrevs) => abbrevs,
            Err(err) => {
                let _ = writeln!(w, "Invalid abbrevs for unit {:#x}: {}", unit_offset.0, &err);
                return ret;
            }
        };
        let mut entries = unit.entries_raw(&abbrevs, None).unwrap();
        let mut unit_refs = Vec::new();
        while !entries.is_empty() {
            let entry_offset = entries.next_offset();
            let abbrev = match entries.read_abbreviation() {
                Err(err) => {
                    let _ = writeln!(
                        w,
                        "Invalid DIE for unit {:#x} at DIE {:#x}: {}",
                        unit_offset.0, entry_offset.0, &err
                    );
                    return ret;
                }
                Ok(None) => continue,
                Ok(Some(abbrev)) => abbrev,
            };
            ret.die_offsets.push(entry_offset);

            for spec in abbrev.attributes() {
                let attr = match entries.read_attribute(*spec) {
                    Err(err) => {
                        let _ = writeln!(
                            w,
                            "Invalid attribute for unit {:#x} at DIE {:#x}: {}",
                            unit_offset.0, entry_offset.0, &err
                        );
                        return ret;
                    }
                    Ok(attr) => attr,
                };
                match attr.value() {
                    gimli::AttributeValue::UnitRef(offset) => {
                        unit_refs.push((entry_offset, offset));
                    }
                    gimli::AttributeValue::DebugInfoRef(offset) => {
                        ret.global_die_references.push((entry_offset, offset));
                    }
                    _ => (),
                }
            }
        }
        ret.internally_valid = true;
        ret.die_offsets.shrink_to_fit();
        ret.global_die_references.shrink_to_fit();

        // Check intra-unit references
        for (from, to) in unit_refs {
            if ret.die_offsets.binary_search(&to).is_err() {
                let _ = writeln!(
                    w,
                    "Invalid intra-unit reference in unit {:#x} from DIE {:#x} to {:#x}",
                    unit_offset.0, from.0, to.0
                );
            }
        }

        ret
    };
    let processed_units = units.into_iter().map(process_unit).collect::<Vec<_>>();

    let check_unit = |summary: &UnitSummary| {
        if !summary.internally_valid {
            return;
        }
        for &(from, to) in summary.global_die_references.iter() {
            let u = match processed_units.binary_search_by_key(&to, |v| v.offset) {
                Ok(i) => &processed_units[i],
                Err(i) => {
                    if i > 0 {
                        &processed_units[i - 1]
                    } else {
                        let _ = writeln!(w, "Invalid cross-unit reference in unit {:#x} from DIE {:#x} to global DIE {:#x}: no unit found",
                                        summary.offset.0, from.0, to.0);
                        continue;
                    }
                }
            };
            if !u.internally_valid {
                continue;
            }
            let to_offset = gimli::UnitOffset(to.0 - u.offset.0);
            if u.die_offsets.binary_search(&to_offset).is_err() {
                let _ = writeln!(w, "Invalid cross-unit reference in unit {:#x} from DIE {:#x} to global DIE {:#x}: unit at {:#x} contains no DIE {:#x}",
                                summary.offset.0, from.0, to.0, u.offset.0, to_offset.0);
            }
        }
    };
    processed_units.iter().for_each(check_unit);
}
