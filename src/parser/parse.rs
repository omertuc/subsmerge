extern crate nom;

use crate::parser::types::*;

use self::nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{flat_map, map, value},
    error::{context, ErrorKind, ParseError},
    multi::{count, many1},
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult, InputTake,
};

#[inline]
fn parse_timestamp<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], Timestamp, E> {
    map(be_u32, Timestamp::from)(input)
}

#[inline]
fn bool_byte<'a, E: ParseError<&'a [u8]>>(f_val: u8, t_val: u8) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], bool, E> {
    alt((value(false, tag([f_val])), value(true, tag([t_val]))))
}

fn parse_segment<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], Segment, E> {
    let (rest, (seg_type, size)) = tuple((be_u8, be_u16))(input)?;

    match seg_type {
        0x14 => context("pds", parse_segment_pallete_definition(size))(rest),
        0x15 => context("ods", parse_segment_object_definition(size))(rest),
        0x16 => context("pcs", parse_segment_presentation_composition(size))(rest),
        0x17 => context("wds", parse_segment_window_definition(size))(rest),
        0x80 => Ok((rest, Segment::End)),
        _ => Err(nom::Err::Error(nom::error::make_error(input, ErrorKind::Eof))),
    }
}

fn parse_segment_pallete_definition<'a, E: ParseError<&'a [u8]>>(size: u16) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Segment, E> {
    map(
        tuple((
            context("id", be_u8),
            context("version", be_u8),
            context("entries", count(parse_pallete_entry, usize::from((size - 2) / 5))),
        )),
        |(id, version, entries)| Segment::PaletteDefinition(PaletteDefinition { id, version, entries }),
    )
}

fn parse_pallete_entry<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], PaletteEntry, E> {
    map(
        tuple((
            context("id", be_u8),
            context("y", be_u8),
            context("Cr", be_u8),
            context("Cb", be_u8),
            context("a", be_u8),
        )),
        |(id, y, cr, cb, a)| PaletteEntry {
            id,
            color: YCrCbAColor { y, cr, cb, a },
        },
    )(input)
}

fn parse_segment_object_definition<'a, E: ParseError<&'a [u8]>>(_size: u16) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Segment, E> {
    move |i: &'a [u8]| {
        let (after_info, (id, version, flag_raw, data_size, width, height)) = tuple((
            context("id", be_u16),
            context("version", be_u8),
            context("last_in_sequence_flag", be_u8),
            context("data_size", be_u24),
            context("width", be_u16),
            context("height", be_u16),
        ))(i)?;

        // - 2 - 2 because data_size includes width & height
        let (rest, data_raw) = after_info.take_split((data_size - 2 - 2) as usize);
        let (incomplete, rle_data) = parse_rle_data(data_raw)?;

        if !incomplete.is_empty() {
            return Err(nom::Err::Error(nom::error::make_error(i, ErrorKind::Eof)));
        }

        Ok((
            rest,
            Segment::ObjectDefinition(ObjectDefinition {
                id,
                version,
                is_last_in_sequence: (flag_raw & 0x40) != 0,
                is_first_in_sequence: (flag_raw & 0x80) != 0,
                width,
                height,
                data_raw: rle_data,
            }),
        ))
    }
}

fn serialize_object_definition(obj: &ObjectDefinition) -> Vec<u8> {
    let mut seg_buf = vec![];
    seg_buf.extend_from_slice(&obj.id.to_be_bytes());
    seg_buf.push(obj.version);
    seg_buf.push(get_object_flag(obj));
    let mut rle_buf = vec![];
    rle_buf.extend_from_slice(&obj.width.to_be_bytes());
    rle_buf.extend_from_slice(&obj.height.to_be_bytes());
    for entry in &obj.data_raw {
        rle_buf.extend_from_slice(&serialize_entry(entry));
    }
    seg_buf.extend_from_slice(&(rle_buf.len() as u32).to_be_bytes()[1..]);
    seg_buf.extend_from_slice(&rle_buf);
    seg_buf
}

#[inline]
fn parse_rle_data<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], Vec<RLEEntry>, E> {
    many1(parse_rle_entry)(input)
}

fn parse_rle_entry<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], RLEEntry, E> {
    if input.is_empty() {
        return Err(nom::Err::Error(nom::error::make_error(input, ErrorKind::Eof)));
    }

    let color = input[0];
    if color != 0 {
        return Ok((&input[1..], RLEEntry::Single(color)));
    }

    if input.len() < 2 {
        return Err(nom::Err::Error(nom::error::make_error(input, ErrorKind::Eof)));
    }

    let metadata = input[1];
    if metadata == 0 {
        return Ok((&input[2..], RLEEntry::EndOfLine));
    }

    let (consumed_length_so_far, repeat_count) = {
        let initial_repeat_count = (metadata & 0b0011_1111) as u16;

        let length_is_14_bits = metadata & 0b0100_0000 != 0;
        if length_is_14_bits {
            if input.len() < 3 {
                return Err(nom::Err::Error(nom::error::make_error(input, ErrorKind::Eof)));
            }

            (2, initial_repeat_count << 8 | input[2] as u16)
        } else {
            // length is just the 6 bits we already read
            (1, initial_repeat_count)
        }
    };

    let color_is_zero = metadata & 0b1000_0000 == 0;
    let (consumed_length_so_far, color) = if color_is_zero {
        // When the flag is not set, the color is implicitly assumed to be 0
        (consumed_length_so_far, 0)
    } else {
        // When the flag is set, we must read the color from the byte following the metadata
        (consumed_length_so_far + 1, input[consumed_length_so_far + 1])
    };

    Ok((
        &input[(1 + consumed_length_so_far)..],
        RLEEntry::Repeated {
            color,
            count: repeat_count,
        },
    ))
}

fn parse_segment_presentation_composition<'a, E: ParseError<&'a [u8]>>(_: u16) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Segment, E> {
    map(
        tuple((
            context("width", be_u16),
            context("height", be_u16),
            context("frame_rate", be_u8),
            context("composition_number", be_u16),
            context("state", parse_presentation_composition_state),
            context("palette_update", bool_byte(0x00, 0x80)),
            context("palette_id", be_u8),
            flat_map(be_u8, |objects_count| {
                count(context("composition_object", parse_composition_object), objects_count as usize)
            }),
        )),
        |(width, height, frame_rate, number, state, palette_update, palette_id, objects)| {
            Segment::PresentationComposition(PresentationComposition {
                width,
                height,
                frame_rate,
                number,
                state,
                palette_update,
                palette_id,
                objects,
            })
        },
    )
}

fn parse_composition_object<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], CompositionObject, E> {
    let (rest, (object_id, window_id, is_crop, x, y)) = tuple((
        context("object_id", be_u16),
        context("window_id", be_u8),
        context("is_crop", bool_byte(0x00, 0x40)),
        context("x", be_u16),
        context("y", be_u16),
    ))(input)?;

    let (rest, crop) = if is_crop {
        map(
            tuple((
                context("crop_x", be_u16),
                context("crop_y", be_u16),
                context("crop_w", be_u16),
                context("crop_h", be_u16),
            )),
            |(x, y, width, height)| CompositionObjectCrop::Cropped { x, y, width, height },
        )(rest)
    } else {
        Ok((rest, CompositionObjectCrop::NotCropped))
    }?;

    Ok((
        rest,
        CompositionObject {
            object_id,
            window_id,
            x,
            y,
            crop,
        },
    ))
}

fn serialize_presentation_composition(pcs: &PresentationComposition) -> Vec<u8> {
    let mut seg_buf = vec![];
    seg_buf.extend_from_slice(&pcs.width.to_be_bytes());
    seg_buf.extend_from_slice(&pcs.height.to_be_bytes());
    seg_buf.push(pcs.frame_rate);
    seg_buf.extend_from_slice(&pcs.number.to_be_bytes());
    seg_buf.push(match pcs.state {
        CompositionState::Normal => 0x00,
        CompositionState::AcquisitionPoint => 0x40,
        CompositionState::EpochStart => 0x80,
    });
    seg_buf.push(if pcs.palette_update { 0x80 } else { 0x00 });
    seg_buf.push(pcs.palette_id);
    seg_buf.push(pcs.objects.len() as u8);
    for obj in &pcs.objects {
        seg_buf.extend_from_slice(&serialize_composition_object(obj));
    }
    seg_buf
}

fn serialize_composition_object(obj: &CompositionObject) -> Vec<u8> {
    let mut obj_buf = vec![];
    obj_buf.extend_from_slice(&obj.object_id.to_be_bytes());
    obj_buf.push(obj.window_id);

    match &obj.crop {
        CompositionObjectCrop::NotCropped => {
            obj_buf.push(0x00);
        }
        CompositionObjectCrop::Cropped { .. } => {
            obj_buf.push(0x40);
        }
    }
    obj_buf.extend_from_slice(&obj.x.to_be_bytes());
    obj_buf.extend_from_slice(&obj.y.to_be_bytes());
    match &obj.crop {
        CompositionObjectCrop::NotCropped => {}
        CompositionObjectCrop::Cropped { x, y, width, height } => {
            obj_buf.extend_from_slice(&x.to_be_bytes());
            obj_buf.extend_from_slice(&y.to_be_bytes());
            obj_buf.extend_from_slice(&width.to_be_bytes());
            obj_buf.extend_from_slice(&height.to_be_bytes());
        }
    }
    obj_buf
}

#[inline]
fn parse_presentation_composition_state<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], CompositionState, E> {
    alt((
        value(CompositionState::Normal, tag([0x00])),
        value(CompositionState::AcquisitionPoint, tag([0x40])),
        value(CompositionState::EpochStart, tag([0x80])),
    ))(input)
}

fn parse_segment_window_definition<'a, E: ParseError<&'a [u8]>>(size: u16) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Segment, E> {
    map(
        preceded(
            context("num_windows", be_u8),
            context(
                "windows",
                count(context("def", parse_window_definition), usize::from((size - 1) / 9)),
            ),
        ),
        Segment::WindowDefinition,
    )
}

fn parse_window_definition<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], WindowDefinition, E> {
    map(
        tuple((
            context("id", be_u8),
            context("x", be_u16),
            context("y", be_u16),
            context("width", be_u16),
            context("height", be_u16),
        )),
        |(id, x, y, width, height)| WindowDefinition { id, x, y, width, height },
    )(input)
}

pub fn get_packet<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], Packet, E> {
    context(
        "packet",
        map(
            preceded(
                tag(b"PG"),
                tuple((
                    context("pts", parse_timestamp),
                    context("dts", parse_timestamp),
                    context("segment", parse_segment),
                )),
            ),
            |(pts, dts, segment)| Packet {
                pts,
                dts,
                segment,
                raw: None,
            },
        ),
    )(input)
}

pub fn serialize_packet(packet: &Packet) -> Vec<u8> {
    let mut buf = Vec::new();
    buf.extend_from_slice(b"PG");
    buf.extend_from_slice(&packet.pts.to_be_bytes());
    buf.extend_from_slice(&packet.dts.to_be_bytes());
    buf.extend_from_slice(&serialize_segment(&packet.segment));
    buf
}

pub fn serialize_segment(segment: &Segment) -> Vec<u8> {
    let mut buf = Vec::new();
    let seg_buf = match segment {
        Segment::PaletteDefinition(pallete) => serialize_pallete(pallete),
        Segment::ObjectDefinition(obj) => serialize_object_definition(obj),
        Segment::PresentationComposition(pcs) => serialize_presentation_composition(pcs),
        Segment::WindowDefinition(wds) => serialize_window_definition(wds),
        Segment::End => {
            vec![]
        }
    };

    buf.push(match segment {
        Segment::PaletteDefinition(_) => 0x14,
        Segment::ObjectDefinition(_) => 0x15,
        Segment::PresentationComposition(_) => 0x16,
        Segment::WindowDefinition(_) => 0x17,
        Segment::End => 0x80,
    });

    buf.extend_from_slice(&(seg_buf.len() as u16).to_be_bytes());
    buf.extend_from_slice(&seg_buf);

    buf
}

fn serialize_window_definition(wds: &Vec<WindowDefinition>) -> Vec<u8> {
    let mut seg_buf = vec![];
    seg_buf.push(wds.len() as u8);
    for win in wds {
        seg_buf.push(win.id);
        seg_buf.extend_from_slice(&win.x.to_be_bytes());
        seg_buf.extend_from_slice(&win.y.to_be_bytes());
        seg_buf.extend_from_slice(&win.width.to_be_bytes());
        seg_buf.extend_from_slice(&win.height.to_be_bytes());
    }
    seg_buf
}

fn get_object_flag(obj: &ObjectDefinition) -> u8 {
    match (obj.is_last_in_sequence, obj.is_first_in_sequence) {
        (true, true) => 0x40 | 0x80,
        (true, false) => 0x40,
        (false, true) => 0x80,
        (false, false) => 0x00,
    }
}

fn serialize_entry(entry: &RLEEntry) -> Vec<u8> {
    let mut entry_buf = vec![];
    match *entry {
        RLEEntry::Single(color) => {
            entry_buf.push(color);
        }
        RLEEntry::Repeated { .. } | RLEEntry::EndOfLine => {
            entry_buf.push(0x00);
        }
    };
    match *entry {
        RLEEntry::Repeated { count, color } => {
            push_repeated_entry(count, color, &mut entry_buf);
        }
        RLEEntry::EndOfLine => {
            entry_buf.push(0x00);
        }
        RLEEntry::Single { .. } => (),
    }
    entry_buf
}

fn push_repeated_entry(count: u16, color: u8, entry_buf: &mut Vec<u8>) {
    if count < 0b0100_0000 {
        if color == 0 {
            entry_buf.push(count as u8);
        } else {
            entry_buf.push(0b1000_0000 | count as u8);
            entry_buf.push(color);
        }
    } else if count < 0b0100_0000_0000_0000 {
        let metadata = 0b0100_0000 | (count >> 8) as u8;

        if color != 0 {
            entry_buf.push(metadata | 0b1000_0000);
        } else {
            entry_buf.push(metadata);
        }

        entry_buf.push((count & 0b1111_1111) as u8);

        if color != 0 {
            entry_buf.push(color);
        }
    } else {
        panic!("unreachable");
    }
}

fn serialize_pallete(pallete: &PaletteDefinition) -> Vec<u8> {
    let mut seg_buf = vec![];
    seg_buf.push(pallete.id);
    seg_buf.push(pallete.version);
    for entry in &pallete.entries {
        seg_buf.push(entry.id);
        seg_buf.push(entry.color.y);
        seg_buf.push(entry.color.cr);
        seg_buf.push(entry.color.cb);
        seg_buf.push(entry.color.a);
    }
    seg_buf
}
