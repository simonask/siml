pub trait StringExt {
    /// Trim the common indent from all lines in the string.
    ///
    /// All leading and trailing whitespace is removed, and the common
    /// indentation is removed from all lines in the string, determined by the
    /// first line in the string. If a line in the string has less indent than
    /// the first line, that indentation is removed. If a line is more indented
    /// than the first line, the remaining indentation is preserved.
    ///
    /// When mixing tabs and spaces, tabs are considered to be 4 spaces.
    ///
    /// This implements the behavior of the `|"..."` syntax.
    fn trim_indent_into(&self, buffer: &mut String);
    fn is_all_whitespace(&self) -> bool;
    fn is_all_spaces_and_tabs(&self) -> bool;
    fn indentation(&self) -> usize;
    fn strip_indentation_up_to(&self, up_to: usize) -> &str;
}

impl StringExt for str {
    fn trim_indent_into(&self, buffer: &mut String) {
        buffer.clear();

        let mut lines = self.trim_end().lines();
        let indent;

        // Find the first line that isn't all whitespace.
        loop {
            if let Some(line) = lines.next() {
                if line.is_all_whitespace() {
                    continue;
                } else {
                    indent = line.indentation();
                    buffer.push_str(line.trim_start());
                    break;
                }
            } else {
                return;
            }
        }

        for line in lines {
            buffer.push('\n');
            buffer.push_str(line.strip_indentation_up_to(indent));
        }
    }

    #[inline]
    fn is_all_whitespace(&self) -> bool {
        self.chars().all(char::is_whitespace)
    }

    #[inline]
    fn is_all_spaces_and_tabs(&self) -> bool {
        self.bytes().all(|c| c == b' ' || c == b'\t')
    }

    #[inline]
    fn indentation(&self) -> usize {
        let mut count = 0;
        for c in self.bytes() {
            match c {
                b' ' => count += 1,
                b'\t' => count += 4,
                _ => break,
            }
        }
        count
    }

    #[inline]
    fn strip_indentation_up_to(&self, up_to: usize) -> &str {
        let mut s = self;
        let mut rem = up_to;
        while rem > 0 {
            match s.bytes().next() {
                Some(b' ') => {
                    s = &s[1..];
                    rem -= 1;
                }
                Some(b'\t') => {
                    s = &s[1..];
                    rem = rem.saturating_sub(4);
                }
                _ => break,
            }
        }
        s
    }
}
