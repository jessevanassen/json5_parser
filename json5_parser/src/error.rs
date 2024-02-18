use std::{error::Error, fmt::Display};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct JsonParseError {
	pub cause: JsonParseErrorCause,

	pub index: usize,
	pub row: usize,
	pub column: usize,
}

impl Display for JsonParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} (line {}, column {})", self.cause, self.row + 1, self.column + 1)
	}
}

impl Error for JsonParseError {}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum JsonParseErrorCause {
	MismatchedCharacter { expected: u8 },
	UnexpectedCharacter,
	UnexpectedEndOfFile,
	InvalidNumber,
	BadEscapeCharacter,
	ExpectedDigit,
	ExpectedHexadecimalDigit,
	InvalidUnicodeCodePoint { value: u32 },
}

impl Display for JsonParseErrorCause {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use JsonParseErrorCause as E;
		match self {
			E::MismatchedCharacter { expected } => {
				write!(f, "Mismatched character, expected '{}'", *expected as char)
			}
			E::UnexpectedCharacter => {
				write!(f, "Unexpected character")
			}
			E::UnexpectedEndOfFile => {
				write!(f, "Unexpected end of file")
			}
			E::InvalidNumber => {
				write!(f, "Invalid number")
			}
			E::BadEscapeCharacter => {
				write!(f, "Bad escape sequence")
			}
			E::ExpectedDigit => {
				write!(f, "Expected digit")
			}
			E::ExpectedHexadecimalDigit => {
				write!(f, "Expected hexadecimal digit")
			}
			E::InvalidUnicodeCodePoint { value } => {
				write!(f, "Invalid Unicode code point value: {value}")
			}
		}
	}
}
