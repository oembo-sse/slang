use serde::{Deserialize, Serialize};
use slang::{Position, Span};

#[derive(Debug, Clone, Serialize, Deserialize, tapi::Tapi)]
#[serde(rename_all = "camelCase")]
pub struct MonacoPosition {
    pub line_number: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Serialize, tapi::Tapi)]
#[serde(rename_all = "camelCase")]
pub struct MonacoSpan {
    pub start_line_number: usize,
    pub start_column: usize,
    pub end_line_number: usize,
    pub end_column: usize,
}

#[derive(Debug, Clone, Serialize, tapi::Tapi)]
#[serde(rename_all = "camelCase")]
pub struct MarkerData {
    // code?: string | {
    //     value: string;
    //     target: Uri;
    // };
    // source?: string;
    pub related_information: Option<Vec<RelatedInformation>>,
    pub tags: Option<Vec<MarkerTag>>,
    pub severity: MarkerSeverity,
    pub message: String,
    pub span: MonacoSpan,
}

#[derive(Debug, Clone, Serialize, tapi::Tapi)]
#[repr(u8)]
#[allow(dead_code)]
pub enum MarkerSeverity {
    Hint = 1,
    Info = 2,
    Warning = 4,
    Error = 8,
}

#[derive(Debug, Clone, Serialize, tapi::Tapi)]
#[repr(u8)]
#[allow(dead_code)]
pub enum MarkerTag {
    Unnecessary = 1,
    Deprecated = 2,
}

#[derive(Debug, Clone, Serialize, tapi::Tapi)]
#[serde(rename_all = "camelCase")]
pub struct RelatedInformation {
    /// Is actually a `Uri`
    pub resource: String,
    pub message: String,
    pub span: MonacoSpan,
}

impl MonacoSpan {
    pub fn from_source_span(src: &str, span: Span) -> MonacoSpan {
        Self::from_offset_len(src, span.start(), span.len())
    }
    pub fn from_offset_len(src: &str, offset: usize, length: usize) -> MonacoSpan {
        let start = Position::from_byte_offset(src, offset);
        let end = Position::from_byte_offset(src, offset + length);
        MonacoSpan {
            start_line_number: start.line + 1,
            start_column: start.column + 1,
            end_line_number: end.line + 1,
            end_column: end.column + 1,
        }
    }
}

impl MonacoPosition {
    pub fn from_source_position(position: Position) -> MonacoPosition {
        MonacoPosition {
            column: position.column,
            line_number: position.line,
        }
    }
}
