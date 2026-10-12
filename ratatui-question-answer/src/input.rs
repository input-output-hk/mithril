/// A single line text input with a cursor.
///
/// The cursor is stored as a char index (not a byte index) so editing is safe with non ASCII text.
#[derive(Debug, Default, Clone)]
pub struct TextInput {
    value: String,
    cursor: usize,
}

impl TextInput {
    /// Create an input pre-filled with `value`, cursor at the end.
    pub fn with_value(value: impl Into<String>) -> Self {
        let value = value.into();
        let cursor = value.chars().count();
        Self { value, cursor }
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    /// Cursor position, in chars, from the start of the value.
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn insert(&mut self, c: char) {
        let index = self.byte_index();
        self.value.insert(index, c);
        self.cursor += 1;
    }

    pub fn backspace(&mut self) {
        if self.cursor > 0 {
            self.cursor -= 1;
            let index = self.byte_index();
            self.value.remove(index);
        }
    }

    pub fn delete(&mut self) {
        if self.cursor < self.len() {
            let index = self.byte_index();
            self.value.remove(index);
        }
    }

    pub fn move_left(&mut self) {
        self.cursor = self.cursor.saturating_sub(1);
    }

    pub fn move_right(&mut self) {
        self.cursor = (self.cursor + 1).min(self.len());
    }

    pub fn move_home(&mut self) {
        self.cursor = 0;
    }

    pub fn move_end(&mut self) {
        self.cursor = self.len();
    }

    fn len(&self) -> usize {
        self.value.chars().count()
    }

    fn byte_index(&self) -> usize {
        self.value
            .char_indices()
            .nth(self.cursor)
            .map(|(i, _)| i)
            .unwrap_or(self.value.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_and_remove_at_cursor() {
        let mut input = TextInput::with_value("ac");
        input.move_left();
        input.insert('b');
        assert_eq!("abc", input.value());
        assert_eq!(2, input.cursor());

        input.backspace();
        assert_eq!("ac", input.value());

        input.move_home();
        input.delete();
        assert_eq!("c", input.value());
        assert_eq!(0, input.cursor());
    }

    #[test]
    fn cursor_stays_within_bounds() {
        let mut input = TextInput::with_value("ab");
        input.move_right();
        assert_eq!(2, input.cursor());
        input.delete();
        assert_eq!("ab", input.value());

        input.move_home();
        input.move_left();
        input.backspace();
        assert_eq!(0, input.cursor());
        assert_eq!("ab", input.value());
    }

    #[test]
    fn editing_is_safe_with_multibyte_chars() {
        let mut input = TextInput::with_value("é/ü");
        input.move_left();
        input.backspace();
        input.insert('x');
        assert_eq!("éxü", input.value());
    }
}
