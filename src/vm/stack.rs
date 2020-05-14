use crate::vm::Error;

pub struct Stack<T> {
    values: Vec<T>,
    capacity: Option<usize>,
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            capacity: None,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            values: Vec::with_capacity(10),
            capacity: Some(capacity),
        }
    }

    pub fn pop(&mut self) -> Result<T, Error> {
        self.values.pop().ok_or_else(|| Error::StackUnderflow)
    }

    pub fn push(&mut self, value: T) -> Result<(), Error> {
        match self.capacity {
            Some(len) if len > self.values.len() => Ok(self.values.push(value)),
            None => Ok(self.values.push(value)),
            _ => Err(Error::StackOverflow),
        }
    }

    pub fn clear(&mut self) {
        self.values.clear();
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }
}
