use std::{collections::HashMap, sync::LazyLock};

pub static OUTPUT_MAP: LazyLock<HashMap<&str, i32>> =
    LazyLock::new(|| HashMap::from([("print", 0), ("print_ascii", 1)]));

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputKind {
    Print,
    PrintAscii,
    Unknown(i32),
}

impl From<i32> for OutputKind {
    fn from(value: i32) -> Self {
        match value {
            0 => OutputKind::Print,
            1 => OutputKind::PrintAscii,
            unknown => OutputKind::Unknown(unknown),
        }
    }
}
