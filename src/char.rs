pub trait CharExt {
    fn is_valid_plain(self) -> bool;
    fn must_escape_dquote(self) -> bool;
    fn must_escape_squote(self) -> bool;
    fn is_linebreak(self) -> bool;
}

impl CharExt for char {
    fn is_valid_plain(self) -> bool {
        // TODO: Unicode
        match self {
            '.' | '!' | '/' | '\\' | '=' | '+' | '-' | '_' => true,
            ch => ch.is_alphanumeric(),
        }
    }

    fn must_escape_dquote(self) -> bool {
        match self {
            '\0' | '\x01'..='\x08' | '\x0b' | '\x0c' | '\x0e'..='\x1f' | '"' | '\\' => true,
            _ => self.is_control(),
        }
    }

    fn must_escape_squote(self) -> bool {
        match self {
            '\0' | '\x01'..='\x08' | '\x0b' | '\x0c' | '\x0e'..='\x1f' | '\'' | '\\' => true,
            _ => self.is_control(),
        }
    }

    fn is_linebreak(self) -> bool {
        self == '\n' || self == '\r'
    }
}
