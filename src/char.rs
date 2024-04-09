pub trait CharExt {
    fn is_valid_plain(self) -> bool;
    fn must_escape(self) -> bool;
    fn is_linebreak(self) -> bool;
    fn escape_quoted<W: std::fmt::Write>(self, w: W, terminate: char) -> std::fmt::Result;
}

impl CharExt for char {
    fn is_valid_plain(self) -> bool {
        // TODO: Unicode
        match self {
            '.' | '!' | '/' | '\\' | '=' | '+' | '-' | '_' => true,
            ch => ch.is_alphanumeric(),
        }
    }

    fn must_escape(self) -> bool {
        match self {
            '\0' | '\x01'..='\x08' | '\x0b' | '\x0c' | '\x0e'..='\x1f' | '"' | '\\' => true,
            _ => self.is_control(),
        }
    }

    fn is_linebreak(self) -> bool {
        self == '\n' || self == '\r'
    }

    fn escape_quoted<W: std::fmt::Write>(self, mut w: W, terminate: char) -> std::fmt::Result {
        if self == terminate {
            w.write_char('\\')?;
            w.write_char(terminate)
        } else {
            match self {
                '\x00' => w.write_str("\\0"),
                '\x07' => w.write_str("\\a"),
                '\x08' => w.write_str("\\b"),
                '\x09' => w.write_str("\\t"),
                '\x0a' => w.write_str("\\n"),
                '\x0b' => w.write_str("\\v"),
                '\x0c' => w.write_str("\\f"),
                '\x0d' => w.write_str("\\r"),
                '\\' => w.write_str("\\\\"),
                ch if self.must_escape() => write!(w, "\\u{:04x}", ch as u32),
                ch => w.write_char(ch),
            }
        }
    }
}
