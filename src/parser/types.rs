use anyhow::{bail, ensure, Result};
use derivative::Derivative;

pub type Timestamp = u32;

#[derive(Derivative, PartialEq, Clone)]
#[derivative(Debug)]
pub struct Packet {
    pub presentation_timestamp: Timestamp,
    pub decoding_timestamp_unused: Timestamp,
    pub segment: Segment,
    pub raw: Option<Vec<u8>>,
}

const OFFSET: u16 = 200;

impl Packet {
    pub fn accept(&self) -> Result<Self> {
        Ok(Packet {
            presentation_timestamp: self.presentation_timestamp,
            decoding_timestamp_unused: self.decoding_timestamp_unused,
            segment: match &self.segment {
                Segment::PresentationComposition(presentation_composition) => {
                    let mut old_objects = presentation_composition.objects.clone();
                    match old_objects.len() {
                        0 => Segment::PresentationComposition(presentation_composition.clone()),
                        1 => {
                            ensure!(old_objects.len() == 1, "expected one object");
                            let old_object = old_objects.remove(0);
                            let mut new_object = old_object.clone();
                            new_object.y = if old_object.y > OFFSET {
                                old_object.y - OFFSET
                            } else {
                                old_object.y + OFFSET
                            };
                            new_object.object_id = 1;
                            let new_presenation_composition = presentation_composition.clone();
                            Segment::PresentationComposition(PresentationComposition {
                                objects: vec![old_object, new_object],
                                ..new_presenation_composition
                            })
                        }
                        _ => bail!("expected one or zero objects"),
                    }
                }

                others => others.clone(),
            },
            raw: self.raw.clone(),
        })
    }
    pub fn force(&self) -> Option<Self> {
        Some(Packet {
            presentation_timestamp: self.presentation_timestamp,
            decoding_timestamp_unused: self.decoding_timestamp_unused,
            segment: match &self.segment {
                Segment::PresentationComposition { .. }
                | Segment::WindowDefinition { .. }
                | Segment::End { .. }
                | Segment::PaletteDefinition { .. } => {
                    return None;
                }
                Segment::ObjectDefinition(object_definition) => {
                    let mut new_object_definition = object_definition.clone();
                    new_object_definition.object_id = 1;
                    Segment::ObjectDefinition(new_object_definition)
                }
            },
            raw: None,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompositionState {
    Normal,
    AcquisitionPoint,
    EpochStart,
}

#[derive(Derivative, PartialEq, Clone)]
#[derivative(Debug)]
pub enum Segment {
    PresentationComposition(PresentationComposition),
    WindowDefinition(Vec<WindowDefinition>),
    PaletteDefinition(PaletteDefinition),
    ObjectDefinition(ObjectDefinition),
    End,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PresentationComposition {
    pub width: u16,
    pub height: u16,
    pub frame_rate: u8,
    pub composition_number: u16,
    pub state: CompositionState,
    pub palette_update: bool,
    pub palette_id: u8,
    pub objects: Vec<CompositionObject>,
}

#[derive(Derivative, PartialEq, Clone)]
#[derivative(Debug)]
pub struct PaletteDefinition {
    pub id: u8,
    pub version: u8,
    #[derivative(Debug = "ignore")]
    pub entries: Vec<PaletteEntry>,
}

#[derive(Derivative, PartialEq, Clone)]
#[derivative(Debug)]
pub struct ObjectDefinition {
    pub object_id: u16,
    pub version: u8,
    pub is_last_in_sequence: bool,
    pub is_first_in_sequence: bool,
    pub width: u16,
    pub height: u16,
    #[derivative(Debug = "ignore")]
    pub data_raw: RLEData,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompositionObject {
    pub object_id: u16,
    pub window_id: u8,
    pub x: u16,
    pub y: u16,
    pub crop: CompositionObjectCrop,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompositionObjectCrop {
    NotCropped,
    Cropped { x: u16, y: u16, width: u16, height: u16 },
}

#[derive(Debug, PartialEq, Clone)]
pub struct WindowDefinition {
    pub id: u8,
    pub x: u16,
    pub y: u16,
    pub width: u16,
    pub height: u16,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PaletteEntry {
    pub id: u8,
    pub color: YCrCbAColor,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct YCrCbAColor {
    pub y: u8,
    pub cr: u8,
    pub cb: u8,
    pub a: u8,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RLEEntry {
    Single(u8),

    Repeated { count: u16, color: u8 },

    EndOfLine,
}

pub type RLEData = Vec<RLEEntry>;
