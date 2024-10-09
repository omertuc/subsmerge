use crate::parser::parse::{get_packet, serialize_packet};
use anyhow::{bail, Context, Result};
use clap::Parser;
use fs::File;
use nom::error::VerboseError;
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

fn main() -> Result<()> {
    let args = Args::parse();

    parse_file(args.pgs_a).context("parsing first PGS file")?;
    parse_file(args.pgs_b).context("parsing second PGS file")?;

    Ok(())
}

fn parse_file(path: String) -> Result<()> {
    let mut opened = File::open(&path)?;

    let full_buffer = {
        let mut vec = Vec::with_capacity(opened.metadata()?.len() as usize);
        opened.read_to_end(&mut vec).context(format!("reading file {}", path))?;
        vec
    };

    let mut buffer_left_to_parse: &[u8] = full_buffer.as_slice();
    let mut out = fs::File::create("output.sup")?;
    while !buffer_left_to_parse.is_empty() {
        let (rest, mut packet) = match get_packet::<VerboseError<&[u8]>>(buffer_left_to_parse) {
            Ok((rest, packet)) => (rest, packet),
            Err(_) => {
                bail!("failed to parse packet");
            }
        };
        let remaining_after_packet = rest.len();
        let packet_size = buffer_left_to_parse.len() - remaining_after_packet;
        packet.raw = Some(buffer_left_to_parse[..packet_size].to_vec());
        let reserialized = &serialize_packet(&packet);

        out.write_all(reserialized).context("writing packet")?;
        buffer_left_to_parse = rest;
    }

    Ok(())
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
