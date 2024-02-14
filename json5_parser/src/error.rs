use std::{error::Error, fmt::Display};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct JsonParseError {
	pub cause: JsonParseErrorCause,

	pub row: usize,
	pub column: usize,
}

impl Display for JsonParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{} {}", self.row + 1, self.column + 1, self.cause)
	}
}

impl Error for JsonParseError {}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum JsonParseErrorCause {
	MismatchedCharacter { char: char, expected: char },
	UnexpectedCharacter { char: char },
	MismatchedEndOfFile { expected: char },
	UnexpectedEndOfFile,
}

impl Display for JsonParseErrorCause {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			JsonParseErrorCause::MismatchedCharacter { expected, char } => {
				write!(
					f,
					"Unexpected character: expected '{expected}' but got '{char}'"
				)
			}
			JsonParseErrorCause::UnexpectedCharacter { char } => {
				write!(f, "Unexpected character: '{char}'")
			}
			JsonParseErrorCause::MismatchedEndOfFile { expected } => {
				write!(f, "Unexpected end of file: expected '{expected}'")
			}
			JsonParseErrorCause::UnexpectedEndOfFile => {
				write!(f, "Unexpected end of file")
			}
		}
	}
}
