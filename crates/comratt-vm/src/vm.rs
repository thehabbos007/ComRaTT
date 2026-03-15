use crate::bytecode::{Function, Op};

pub trait WasmBackend {
    fn call(&mut self, fn_idx: u32, args: &[i32]) -> i32;
}

#[derive(Debug, Clone)]
pub enum Value {
    I32(i32),
    Bool(bool),
    Unit,
    Thunk {
        fun_idx: u32,
        captures: Box<[Value]>,
        clock: u32,
    },
    Wait {
        channel_idx: u16,
        clock: u32,
    },
    // We don't intend on resizing, so boxed slice is sufficient
    Tuple(Box<[Value]>),
}

impl Value {
    pub fn as_i32(&self) -> i32 {
        match self {
            Value::I32(n) => *n,
            Value::Bool(b) => *b as i32,
            Value::Unit => 0,
            _ => panic!("expected i32, got {self:?}"),
        }
    }

    pub fn clock(&self) -> u32 {
        match self {
            Value::Thunk { clock, .. } => *clock,
            Value::Wait { clock, .. } => *clock,
            _ => 0x00000000,
        }
    }

    fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::I32(n) => *n != 0,
            _ => panic!("expected bool, got {self:?}"),
        }
    }
}

pub struct VM<W: WasmBackend> {
    pub bytecode_fns: Vec<Function>,
    stack: Vec<Value>,
    wasm_backend: W,
    pub channels: Vec<i32>,
}

fn bin_i32(stack: &mut Vec<Value>, f: impl FnOnce(i32, i32) -> Value) {
    let r = stack.pop().unwrap().as_i32();
    let l = stack.pop().unwrap().as_i32();
    stack.push(f(l, r));
}

impl<W: WasmBackend> VM<W> {
    pub fn new(bytecode_fns: Vec<Function>, wasm_backend: W) -> Self {
        VM {
            bytecode_fns,
            stack: Vec::with_capacity(64),
            wasm_backend,
            channels: vec![],
        }
    }

    pub fn init_channels(&mut self, count: usize) {
        self.channels = vec![0; count];
    }

    pub fn call_wasm(&mut self, idx: u32, args: &[i32]) -> i32 {
        self.wasm_backend.call(idx, args)
    }

    pub fn execute(&mut self, fn_idx: u32, args: Vec<Value>) -> Value {
        let local_count = self.bytecode_fns[fn_idx as usize].local_count as usize;
        let mut locals = args;
        locals.resize(local_count, Value::Unit);
        let mut instruction_ptr: usize = 0;

        loop {
            let op = self.bytecode_fns[fn_idx as usize].ops[instruction_ptr];
            instruction_ptr += 1;

            match op {
                Op::ConstI32(n) => self.stack.push(Value::I32(n)),
                Op::ConstBool(b) => self.stack.push(Value::Bool(b)),
                Op::ConstUnit => self.stack.push(Value::Unit),

                Op::Add => bin_i32(&mut self.stack, |a, b| Value::I32(a.wrapping_add(b))),
                Op::Sub => bin_i32(&mut self.stack, |a, b| Value::I32(a.wrapping_sub(b))),
                Op::Mul => bin_i32(&mut self.stack, |a, b| Value::I32(a.wrapping_mul(b))),
                Op::Div => bin_i32(&mut self.stack, |a, b| {
                    if b == 0 {
                        panic!("division by zero")
                    }
                    Value::I32(a.wrapping_div(b))
                }),
                Op::Eq => bin_i32(&mut self.stack, |a, b| Value::Bool(a == b)),
                Op::Neq => bin_i32(&mut self.stack, |a, b| Value::Bool(a != b)),
                Op::Lt => bin_i32(&mut self.stack, |a, b| Value::Bool(a < b)),
                Op::Lte => bin_i32(&mut self.stack, |a, b| Value::Bool(a <= b)),
                Op::Gt => bin_i32(&mut self.stack, |a, b| Value::Bool(a > b)),
                Op::Gte => bin_i32(&mut self.stack, |a, b| Value::Bool(a >= b)),

                Op::Load(idx) => self.stack.push(locals[idx as usize].clone()),

                Op::Store(idx) => {
                    let val = self.stack.pop().unwrap();
                    let idx = idx as usize;
                    if idx >= locals.len() {
                        locals.resize(idx + 1, Value::Unit);
                    }
                    locals[idx] = val;
                }

                Op::JumpIfFalse(target) => {
                    if !self.stack.pop().unwrap().as_bool() {
                        instruction_ptr = target as usize;
                    }
                }
                Op::Jump(target) => {
                    instruction_ptr = target as usize;
                }

                Op::GetClock => {
                    let val = self.stack.pop().unwrap();
                    self.stack.push(Value::I32(val.clock() as i32));
                }

                Op::ConstClock(mask) => {
                    self.stack.push(Value::I32(mask as i32));
                }

                Op::BitOr => {
                    let r = self.stack.pop().unwrap().as_i32();
                    let l = self.stack.pop().unwrap().as_i32();
                    self.stack.push(Value::I32(l | r));
                }

                Op::MakeTuple(count) => {
                    let start = self.stack.len() - count as usize;
                    let elems: Box<[Value]> = self.stack.drain(start..).collect();
                    self.stack.push(Value::Tuple(elems));
                }

                Op::AccessTuple(idx) => match self.stack.pop().unwrap() {
                    Value::Tuple(elems) => self.stack.push(elems[idx as usize].clone()),
                    other => panic!("AccessTuple on non-tuple: {other:?}"),
                },

                Op::CallWasm(idx, argc) => {
                    let start = self.stack.len() - argc as usize;
                    let args: Vec<i32> = self.stack.drain(start..).map(|v| v.as_i32()).collect();
                    let result = self.call_wasm(idx, &args);
                    self.stack.push(Value::I32(result));
                }

                Op::CallBytecode(idx, argc) => {
                    let start = self.stack.len() - argc as usize;
                    let args: Vec<Value> = self.stack.drain(start..).collect();
                    let result = self.execute(idx, args);
                    self.stack.push(result);
                }

                Op::Return => {
                    return self.stack.pop().unwrap_or(Value::Unit);
                }

                Op::Thunk(thunk_fun, capture_count) => {
                    let clock = self.stack.pop().unwrap().as_i32() as u32;
                    let start = self.stack.len() - capture_count as usize;
                    let captures: Box<[Value]> = self.stack.drain(start..).collect();
                    self.stack.push(Value::Thunk {
                        fun_idx: thunk_fun,
                        captures,
                        clock,
                    });
                }

                Op::Wait(channel_idx) => {
                    self.stack.push(Value::Wait {
                        channel_idx,
                        clock: 1u32 << channel_idx,
                    });
                }

                Op::Force => match self.stack.pop().unwrap() {
                    Value::Thunk {
                        fun_idx: thunk_idx,
                        captures,
                        ..
                    } => {
                        let result = self.execute(thunk_idx, captures.into_vec());
                        self.stack.push(result);
                    }
                    Value::Wait { channel_idx, .. } => {
                        let val = self.channels[channel_idx as usize];
                        self.stack.push(Value::I32(val));
                    }
                    other => panic!("Tried to force on non-thunk: {other:?}"),
                },
            }
        }
    }

    pub fn force_all(&mut self, mut val: Value) -> Value {
        loop {
            match val {
                Value::Thunk {
                    fun_idx, captures, ..
                } => {
                    val = self.execute(fun_idx, captures.into_vec());
                }
                Value::Wait { channel_idx, .. } => {
                    val = Value::I32(self.channels[channel_idx as usize]);
                }
                _ => return val,
            }
        }
    }
}
