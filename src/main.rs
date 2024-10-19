use crate::parser::parse::{get_packet, serialize_packet};
use crate::parser::types::Packet;
use anyhow::{bail, ensure, Context, Result};
use clap::Parser;
use derivative::Derivative;
use fs::File;
use nom::error::VerboseError;
use parser::types::Segment;
use std::fs;
use std::io::{Read, Write};

mod parser;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    /// The first PGS subtitles file path
    pgs_a: String,

    /// The second PGS subtitles file path
    pgs_b: String,

    /// The path to output the merged PGS subtitles file to
    output: String,
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
struct DisplaySet {
    presentation_composition: Packet,
    window_definitions: Vec<Packet>,
    palette_definitions: Vec<Packet>,
    object_definitions: Vec<Packet>,
    end_of_display_set: Packet,
}

impl DisplaySet {
    fn presentation_timestamp(&self) -> u32 {
        self.presentation_composition.presentation_timestamp
    }

    fn is_clearing_display(&self) -> bool {
        match &self.presentation_composition.segment {
            Segment::PresentationComposition(presentation_composition) => presentation_composition.objects.len() == 0,
            _ => panic!("unexpected segment"),
        }
    }
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
struct DisplaySetPair {
    display_set_a: DisplaySet,
    display_set_b: DisplaySet,
}

impl DisplaySetPair {
    fn new(display_set_a: DisplaySet, display_set_b: DisplaySet) -> Result<Self> {
        ensure!(
            !display_set_a.is_clearing_display() && display_set_b.is_clearing_display(),
            "display set A must not clear the display and display set B must clear the display"
        );
        Ok(DisplaySetPair {
            display_set_a,
            display_set_b,
        })
    }

    fn is_inside(&self, packet: &Packet) -> bool {
        packet.presentation_timestamp >= self.display_set_a.presentation_timestamp()
            && packet.presentation_timestamp <= self.display_set_b.presentation_timestamp()
    }

    fn insert(&mut self, packet: &Packet) -> Result<()> {
        Ok(match packet.segment {
            Segment::ObjectDefinition { .. } => {
                self.display_set_a.object_definitions.push(packet.clone());
            }
            _ => bail!("unexpected segment"),
        })
    }
}

impl IntoIterator for DisplaySetPair {
    type Item = Packet;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        let mut packets = Vec::new();
        packets.push(self.display_set_a.presentation_composition.clone());
        packets.extend(self.display_set_a.window_definitions.clone());
        packets.extend(self.display_set_a.palette_definitions.clone());
        packets.extend(self.display_set_a.object_definitions.clone());
        packets.push(self.display_set_a.end_of_display_set.clone());
        packets.push(self.display_set_b.presentation_composition.clone());
        packets.extend(self.display_set_b.window_definitions.clone());
        packets.extend(self.display_set_b.palette_definitions.clone());
        packets.extend(self.display_set_b.object_definitions.clone());
        packets.push(self.display_set_b.end_of_display_set.clone());
        packets.into_iter()
    }
}

struct DisplaySetPairCollector {
    display_sets: Vec<DisplaySet>,
}

impl DisplaySetPairCollector {
    fn new() -> Self {
        DisplaySetPairCollector { display_sets: Vec::new() }
    }

    fn add_display_set(&mut self, display_set: DisplaySet) -> Result<Option<DisplaySetPair>> {
        self.display_sets.push(display_set.clone());

        Ok(if self.display_sets.len() == 2 {
            let display_set_a = self.display_sets.remove(0);
            let display_set_b = self.display_sets.remove(0);
            Some(DisplaySetPair::new(display_set_a, display_set_b)?)
        } else {
            None
        })
    }
}

struct DisplaySetCollector {
    presentation_timestamp: Option<u32>,
    packets: Vec<Packet>,
}

impl DisplaySetCollector {
    fn new() -> Self {
        DisplaySetCollector {
            packets: Vec::new(),
            presentation_timestamp: None,
        }
    }

    fn reset(&mut self) {
        self.presentation_timestamp = None;
        self.packets.clear();
    }

    fn add_packet(&mut self, packet: Packet) -> Result<Option<DisplaySet>> {
        if self.presentation_timestamp.is_none() {
            self.presentation_timestamp = Some(packet.presentation_timestamp);
        } else if packet.presentation_timestamp != self.presentation_timestamp.unwrap() {
            bail!("presentation timestamp mismatch");
        }

        self.packets.push(packet.clone());

        Ok(if packet.segment == Segment::End {
            let mut presentation_composition = None;
            let mut window_definitions = Vec::new();
            let mut palette_definitions = Vec::new();
            let mut object_definitions = Vec::new();
            let mut end_of_display_set = None;

            for packet in self.packets.iter() {
                match packet.segment {
                    Segment::PresentationComposition { .. } => {
                        presentation_composition = Some(packet.clone());
                    }
                    Segment::WindowDefinition { .. } => {
                        window_definitions.push(packet.clone());
                    }
                    Segment::PaletteDefinition { .. } => {
                        palette_definitions.push(packet.clone());
                    }
                    Segment::ObjectDefinition { .. } => {
                        object_definitions.push(packet.clone());
                    }
                    Segment::End => {
                        end_of_display_set = Some(packet.clone());
                    }
                }
            }

            self.reset();

            Some(DisplaySet {
                presentation_composition: presentation_composition.unwrap(),
                window_definitions,
                palette_definitions,
                object_definitions,
                end_of_display_set: end_of_display_set.unwrap(),
            })
        } else {
            None
        })
    }
}

fn main() -> Result<()> {
    let args = Args::parse();

    let packets1 = parse_file(args.pgs_a).context("parsing first PGS file")?;
    let packets1 = packets1
        .into_iter()
        .map(|packet| packet.accept())
        .collect::<Result<Vec<_>>>()
        .context("accepting packets")?;

    let packets2 = parse_file(args.pgs_b).context("parsing second PGS file")?;

    let mut collector = DisplaySetCollector::new();
    let mut pair_collector = DisplaySetPairCollector::new();
    let mut display_set_pairs = Vec::new();
    for packet in packets1.into_iter() {
        if let Some(display_set) = collector.add_packet(packet).context("adding packet")? {
            if let Some(display_set_pair) = pair_collector.add_display_set(display_set).context("adding display set")? {
                display_set_pairs.push(display_set_pair);
            }
        }
    }

    for packet in packets2.into_iter().filter_map(|packet| packet.force()).collect::<Vec<_>>() {
        for display_set_pair in display_set_pairs.iter_mut() {
            if display_set_pair.is_inside(&packet) {
                display_set_pair.insert(&packet).context("inserting packet")?;
            }
        }
    }

    let mut out = fs::File::create(args.output)?;
    for display_set_pair in display_set_pairs.into_iter() {
        for packet in display_set_pair.into_iter() {
            let reserialized = &serialize_packet(&packet);
            out.write_all(reserialized).context("writing packet")?;
        }
    }

    Ok(())
}

fn parse_file(path: String) -> Result<Vec<Packet>> {
    let mut opened = File::open(&path)?;

    let full_buffer = {
        let mut vec = Vec::with_capacity(opened.metadata()?.len() as usize);
        opened.read_to_end(&mut vec).context(format!("reading file {}", path))?;
        vec
    };

    let mut buffer_left_to_parse: &[u8] = full_buffer.as_slice();
    let mut collector = DisplaySetCollector::new();
    let mut pair_collector = DisplaySetPairCollector::new();

    let mut packets: Vec<Packet> = Vec::new();

    while !buffer_left_to_parse.is_empty() {
        let (rest, packet) = match get_packet::<VerboseError<&[u8]>>(buffer_left_to_parse) {
            Ok((rest, packet)) => (rest, packet),
            Err(_) => {
                bail!("failed to parse packet");
            }
        };

        packets.push(packet.clone());

        // let remaining_after_packet = rest.len();
        // let packet_size = buffer_left_to_parse.len() - remaining_after_packet;
        // packet.raw = Some(buffer_left_to_parse[..packet_size].to_vec());
        buffer_left_to_parse = rest;

        // if let Some(display_set) = collector.add_packet(packet).context("adding packet")? {
        //     if let Some(display_set_pair) = pair_collector.add_display_set(display_set).context("adding display set")? {
        //         println!("{:?}", display_set_pair);
        //     }
        // }
    }

    Ok(packets)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_file() {
        let path = "english.sup".to_string();
        assert!(parse_file(path).is_ok());
    }
}
