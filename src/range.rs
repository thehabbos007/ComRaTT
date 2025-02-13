use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Range {
    pub fn start(&self) -> usize {
        self.start as usize
    }

    pub fn end(&self) -> usize {
        self.end as usize
    }
}

impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<Range> for std::ops::Range<usize> {
    fn from(range: Range) -> std::ops::Range<usize> {
        (range.start as usize)..(range.end as usize)
    }
}

impl From<std::ops::Range<usize>> for Range {
    fn from(range: std::ops::Range<usize>) -> Range {
        Range {
            start: range.start,
            end: range.end,
        }
    }
}
