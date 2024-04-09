use std::collections::VecDeque;

use crate::SourceLocation;

#[derive(Debug, Default)]
pub struct InputBuffer {
    buffer: VecDeque<Input<char>>,
    current_location: SourceLocation,
    /// True if EOF has been popped from the buffer.
    eof: bool,
    /// True if buffer contains an EOF (push_eof() has been called).
    has_eof: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Input<T> {
    Value(T),
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NeedMore(pub usize);

impl InputBuffer {
    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    #[inline]
    pub fn push_str(&mut self, s: &str) {
        assert!(!self.has_eof, "push_str() after push_eof()");
        self.buffer.reserve(s.len());
        self.extend(s.chars());
    }

    #[inline]
    pub fn push_eof(&mut self) {
        if self.has_eof {
            return;
        }
        self.has_eof = true;
        self.buffer.push_back(Input::Eof);
    }

    #[inline]
    pub fn peek(&self) -> Result<Input<char>, NeedMore> {
        self.peek_nth::<0>()
    }

    #[inline]
    pub fn peek_nth<const N: usize>(&self) -> Result<Input<char>, NeedMore> {
        if self.eof {
            return Ok(Input::Eof);
        }

        match self.buffer.get(N).copied() {
            Some(value) => Ok(value),
            None => Err(NeedMore(N + 1)),
        }
    }

    #[inline]
    pub fn peek2(&self) -> Result<[Input<char>; 2], NeedMore> {
        let mut arr = [Input::Eof, Input::Eof];
        if self.eof {
            return Ok(arr);
        }
        if self.buffer.len() < 2 {
            if self.has_eof {
                return Ok(arr);
            }
            return Err(NeedMore(2));
        }

        arr[0] = self.buffer[0];
        arr[1] = self.buffer[1];
        Ok(arr)
    }

    #[inline]
    pub fn pop(&mut self) -> Result<Input<char>, NeedMore> {
        if self.eof {
            return Ok(Input::Eof);
        }

        let popped = self.buffer.pop_front();
        match popped {
            Some(Input::Value(c)) => {
                self.current_location.advance(c);
                Ok(Input::Value(c))
            }
            Some(Input::Eof) => {
                self.eof = true;
                Ok(Input::Eof)
            }
            None => Err(NeedMore(1)),
        }
    }

    #[inline]
    pub fn pop2(&mut self) -> Result<(), NeedMore> {
        self.pop()?;
        self.pop().map(|_| ())
    }

    #[inline]
    pub fn current_location(&self) -> SourceLocation {
        self.current_location
    }

    #[inline]
    pub fn is_eof(&self) -> bool {
        self.eof
    }
}

impl Extend<char> for InputBuffer {
    fn extend<T: IntoIterator<Item = char>>(&mut self, iter: T) {
        self.buffer.extend(iter.into_iter().map(Input::Value));
    }
}
