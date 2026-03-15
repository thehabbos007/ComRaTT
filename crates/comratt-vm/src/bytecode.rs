#[derive(Debug, Clone, Copy)]
pub enum Op {
    ConstI32(i32),
    ConstBool(bool),
    ConstUnit,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Load(u16),
    Store(u16),
    JumpIfFalse(u32),
    Jump(u32),
    CallWasm(u32, u8),
    CallBytecode(u32, u8),
    GetClock,
    ConstClock(u32),
    MakeTuple(u8),
    AccessTuple(u8),
    BitOr,
    Return,
    /// (Function index, capture count)
    Thunk(u32, u8),
    /// (Channel index)
    Wait(u16),
    Force,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub param_count: u16,
    pub local_count: u16,
    pub ops: Vec<Op>,
}
