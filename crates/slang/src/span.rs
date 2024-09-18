#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn from_start_end(start: usize, end: usize) -> Span {
        let (start, end) = (start.min(end), start.max(end));
        Span { start, end }
    }

    pub fn from_start_len(start: usize, len: usize) -> Span {
        let end = start + len;
        Span { start, end }
    }

    pub fn start(self) -> usize {
        self.start
    }
    pub fn end(self) -> usize {
        self.end
    }
    pub fn len(self) -> usize {
        self.end.saturating_sub(self.start)
    }
    pub fn is_empty(self) -> bool {
        self.len() == 0
    }
    pub fn is_default(self) -> bool {
        self == Self::default()
    }
    pub fn union(self, other: Span) -> Span {
        if self.is_default() {
            other
        } else if other.is_default() {
            self
        } else {
            Span::from_start_end(self.start.min(other.start), self.end.max(other.end))
        }
    }

    pub fn start_pos(self, src: &str) -> Position {
        Position::from_byte_offset(src, self.start())
    }
    pub fn end_pos(self, src: &str) -> Position {
        Position::from_byte_offset(src, self.end())
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Span({}..{})", self.start(), self.end())
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        miette::SourceSpan::new(miette::SourceOffset::from(span.start()), span.len())
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Position {
        Position { line, column }
    }
    pub fn to_byte_offset(self, src: &str) -> Option<usize> {
        let mut lines = self.line;
        let mut columns = self.column;
        src.char_indices()
            .find(|&(_, c)| {
                if lines == 0 {
                    if columns == 0 {
                        return true;
                    }
                    columns -= c.len_utf8()
                } else if c == '\n' {
                    lines -= 1;
                }
                false
            })
            .map(|(idx, _)| idx)
    }
    pub fn from_byte_offset(src: &str, byte_offset: usize) -> Position {
        if src.is_empty() {
            return Position::new(0, 0);
        }
        if src.get(0..byte_offset).is_none() {
            tracing::debug!(?src, byte_offset, len=?src.len(), "byte offset out of range");
            // Return the final position
            let l = src.lines().count();
            let c = src.lines().last().unwrap().len();
            return Position::new(l as _, c as _);
        }
        if src[0..byte_offset].is_empty() {
            return Position::new(0, 0);
        }

        if src[0..byte_offset].ends_with('\n') {
            let l = src[0..byte_offset].lines().count();
            Position::new(l as _, 0)
        } else {
            let l = src[0..byte_offset].lines().count() - 1;
            let c = src[0..byte_offset].lines().last().unwrap().len();
            Position::new(l as _, c as _)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn round_trip(src in ".*", byte_offset in 0..10_000usize) {
            let pos = Position::from_byte_offset(&src, byte_offset);
            if let Some(b) = pos.to_byte_offset(&src) {
                assert_eq!(byte_offset, b);
            } else if src.is_char_boundary(byte_offset) {
                assert!(byte_offset >= src.len());
            }
        }
    }
}
