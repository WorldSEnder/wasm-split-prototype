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
    enable_tombstone_hack: bool,
}

const RELOC_TO_UNDEF_ADDRESS: Option<usize> = Some(reloc::SENTINEL_UNDEF);

impl RelocTarget for DwarfRelocTarget<'_, '_> {
    const SENTINEL_UNDEF: bool = true;
    fn reloc_value(&self, reloc: RelocDetails<'_>) -> Result<Option<usize>> {
        let reloc = match reloc {
            RelocDetails::FunctionOffset(details)
                if let Some(&local_def) = self
                    .module
                    .dep_to_local_index
                    .get(&DepNode::Function(details.index)) =>
            {
                match self.module.function_offset_hint.get(&local_def) {
                    Some(&offset) => Some(self.module.function_header_len + offset),
                    None => RELOC_TO_UNDEF_ADDRESS,
                }
            }
            RelocDetails::FunctionOffset(_) => RELOC_TO_UNDEF_ADDRESS,
            RelocDetails::MemoryAddr(DataDetails {
                definition: None, ..
            }) => return Ok(None),
            RelocDetails::MemoryAddr(
                details @ DataDetails {
                    definition: Some(symbol),
                    ..
                },
            ) if let Ok(address) = self
                .module
                .emit_state
                .data_relocations
                .find_relocated_address(details.symbol_index, symbol) =>
            {
                address
            }
            RelocDetails::MemoryAddr(_) => RELOC_TO_UNDEF_ADDRESS,
            reloc @ RelocDetails::GlobalIndex(_) => return self.module.reloc_value(reloc),
            _ => bail!("unexpected reloc in debug section: {:?}", reloc),
        };
        if self.enable_tombstone_hack && self.module.is_main() {
            return Ok(Some(0));
        }
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
    let target = DwarfRelocTarget {
        module,
        enable_tombstone_hack: S::id() == gimli::SectionId::DebugLine,
    };
    let reloc_data = RelocInfo::get_relocated_data(&module.input_module, input_range, &target)?;
    module.output_module.section(&CustomSection {
        name: S::section_name().into(),
        data: reloc_data.clone().into(),
    });
    Ok(reloc_data)
}

pub fn emit_debug_info(module: &mut ModuleEmitState<'_>) -> Result<()> {
    let crate::read::DwarfState::Inline(input_dwarf) = &module.input_module.dwarf else {
        return Ok(());
    };
    // if !module.is_main() {
    //     return Ok(());
    // }
    let mut w: std::io::BufWriter<std::io::Stdout> = std::io::BufWriter::new(std::io::stdout());
    let mut error_writer = ErrorWriter {
        inner: std::sync::Mutex::new((&mut w, 0)),
    };
    validate_info(&mut error_writer, input_dwarf.borrow(|v| v.clone()));
    tracing::trace!("original debug info passed validation!");

    let mut validate = gimli::DwarfSections::default();
    let debug_info = write_relocate_dwarf_section(module, &input_dwarf.debug_info)?;
    validate.debug_info = debug_info.into();
    let debug_abbrev = write_relocate_dwarf_section(module, &input_dwarf.debug_abbrev)?;
    validate.debug_abbrev = debug_abbrev.into();

    validate.debug_addr = write_relocate_dwarf_section(module, &input_dwarf.debug_addr)?.into();
    let debug_line = write_relocate_dwarf_section(module, &input_dwarf.debug_line)?;
    validate.debug_line = debug_line.into();
    validate.debug_loc = write_relocate_dwarf_section(module, &input_dwarf.debug_loc)?.into();
    validate.debug_loclists =
        write_relocate_dwarf_section(module, &input_dwarf.debug_loclists)?.into();
    validate.debug_ranges = write_relocate_dwarf_section(module, &input_dwarf.debug_ranges)?.into();
    validate.debug_rnglists =
        write_relocate_dwarf_section(module, &input_dwarf.debug_rnglists)?.into();
    validate.debug_str = write_relocate_dwarf_section(module, &input_dwarf.debug_str)?.into();
    validate.debug_str_offsets =
        write_relocate_dwarf_section(module, &input_dwarf.debug_str_offsets)?.into();
    validate.debug_line_str =
        write_relocate_dwarf_section(module, &input_dwarf.debug_line_str)?.into();
    validate.debug_macro = write_relocate_dwarf_section(module, &input_dwarf.debug_macro)?.into();

    validate.debug_aranges =
        write_relocate_dwarf_section(module, &input_dwarf.debug_aranges)?.into();
    validate.debug_names = write_relocate_dwarf_section(module, &input_dwarf.debug_names)?.into();
    validate_info(
        &mut error_writer,
        validate.borrow(|v| gimli::EndianSlice::new(&v[..], gimli::LittleEndian)),
    );
    tracing::trace!("transformed debug info passed validation!");
    Ok(())
}

struct UnitSummary {
    // True if we successfully parsed all the DIEs and attributes in the compilation unit
    internally_valid: bool,
    offset: gimli::DebugInfoOffset,
    die_offsets: Vec<gimli::UnitOffset>,
    global_die_references: Vec<(gimli::UnitOffset, gimli::DebugInfoOffset)>,
}

struct ErrorWriter<W: std::io::Write + Send> {
    inner: std::sync::Mutex<(W, usize)>,
}

impl<W: std::io::Write + Send> ErrorWriter<W> {
    #[allow(clippy::needless_pass_by_value)]
    fn error(&self, s: String) {
        let mut lock = self.inner.lock().unwrap();
        writeln!(&mut lock.0, "DWARF error: {}", s).unwrap();
        lock.1 += 1;
    }
}

fn validate_info<W, R>(w: &mut ErrorWriter<W>, dwarf: gimli::Dwarf<R>)
where
    W: std::io::Write + Send,
    R: gimli::Reader<Offset = usize>,
{
    let debug_info = &dwarf.debug_info;
    let debug_abbrev = &dwarf.debug_abbrev;

    let mut units = Vec::new();
    let mut units_iter = debug_info.units();
    let mut last_offset = 0;
    loop {
        let u = match units_iter.next() {
            Err(err) => {
                w.error(format!(
                    "Can't read unit header at offset {:#x}, stopping reading units: {}",
                    last_offset, err
                ));
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
                w.error(format!(
                    "Invalid abbrevs for unit {:#x}: {}",
                    unit_offset.0, &err
                ));
                return ret;
            }
        };
        let mut entries = unit.entries_raw(&abbrevs, None).unwrap();
        let mut unit_refs = Vec::new();
        while !entries.is_empty() {
            let entry_offset = entries.next_offset();
            let abbrev = match entries.read_abbreviation() {
                Err(err) => {
                    w.error(format!(
                        "Invalid DIE for unit {:#x} at DIE {:#x}: {}",
                        unit_offset.0, entry_offset.0, &err
                    ));
                    return ret;
                }
                Ok(None) => continue,
                Ok(Some(abbrev)) => abbrev,
            };
            ret.die_offsets.push(entry_offset);

            for spec in abbrev.attributes() {
                let attr = match entries.read_attribute(*spec) {
                    Err(err) => {
                        w.error(format!(
                            "Invalid attribute for unit {:#x} at DIE {:#x}: {}",
                            unit_offset.0, entry_offset.0, &err
                        ));
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
                w.error(format!(
                    "Invalid intra-unit reference in unit {:#x} from DIE {:#x} to {:#x}",
                    unit_offset.0, from.0, to.0
                ));
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
                        w.error(format!("Invalid cross-unit reference in unit {:#x} from DIE {:#x} to global DIE {:#x}: no unit found",
                                        summary.offset.0, from.0, to.0));
                        continue;
                    }
                }
            };
            if !u.internally_valid {
                continue;
            }
            let to_offset = gimli::UnitOffset(to.0 - u.offset.0);
            if u.die_offsets.binary_search(&to_offset).is_err() {
                w.error(format!("Invalid cross-unit reference in unit {:#x} from DIE {:#x} to global DIE {:#x}: unit at {:#x} contains no DIE {:#x}",
                                summary.offset.0, from.0, to.0, u.offset.0, to_offset.0));
            }
        }
    };
    processed_units.iter().for_each(check_unit);
}
