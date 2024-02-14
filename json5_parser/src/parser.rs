use crate::{error::JsonParseErrorCause, Json, JsonParseError};

const WHITESPACE_CHARACTERS: [u8; 4] = [b' ', b'\n', b'\t', b'\r'];

type Result<T = Json> = ::std::result::Result<T, JsonParseError>;

pub fn parse_json(source: impl AsRef<str>) -> Result {
	let source = source.as_ref();
	Parser::new(source).parse()
}

struct Parser<'a> {
	source: &'a [u8],
	index: usize,
	row: usize,
	column: usize,
}

impl<'a> Parser<'a> {
	pub fn new(source: &'a str) -> Self {
		Parser {
			source: source.as_bytes(),
			index: 0,
			row: 0,
			column: 0,
		}
	}

	pub fn parse(&mut self) -> Result {
		self.consume_whitespace();

		let result = self.parse_value()?;

		self.consume_whitespace();

		if let Some(char) = self.peek() {
			return Err(self.create_error(JsonParseErrorCause::UnexpectedCharacter { char: char as char }));
		}

		Ok(result)
	}

	fn parse_value(&mut self) -> Result {
		match self.peek() {
			Some(b'f') => self.parse_literal(b"false", Json::Boolean(false)),
			Some(b't') => self.parse_literal(b"true", Json::Boolean(true)),
			Some(b'n') => self.parse_literal(b"null", Json::Null),
			Some(c) => {
				Err(self
					.create_error(JsonParseErrorCause::UnexpectedCharacter { char: c as char }))
			}
			None => Err(self.create_error(JsonParseErrorCause::UnexpectedEndOfFile)),
		}
	}

	fn parse_literal(&mut self, literal: &[u8], result: Json) -> Result {
		self.match_str(literal)?;
		Ok(result)
	}

	fn peek(&self) -> Option<u8> {
		self.source.get(self.index).copied()
	}

	fn match_char(&mut self, expected: u8) -> Result<()> {
		match self.peek() {
			Some(ch) if ch == expected => {
				self.consume().unwrap();
				Ok(())
			}
			Some(ch) => {
				let cause = JsonParseErrorCause::MismatchedCharacter {
					expected: expected as char,
					char: ch as char,
				};
				Err(self.create_error(cause))
			}
			None => {
				let cause = JsonParseErrorCause::MismatchedEndOfFile {
					expected: expected as char,
				};
				Err(self.create_error(cause))
			}
		}
	}

	fn match_str(&mut self, expected: &[u8]) -> Result<()> {
		for &char in expected {
			self.match_char(char)?;
		}

		Ok(())
	}

	fn consume(&mut self) -> Option<u8> {
		let peeked = self.peek()?;

		self.index += 1;

		if peeked == b'\n' {
			self.row += 1;
			self.column = 0;
		} else {
			self.column += 1;
		}

		Some(peeked)
	}

	fn consume_whitespace(&mut self) {
		while self.peek().is_some_and(|ch| WHITESPACE_CHARACTERS.contains(&ch)) {
			self.consume();
		}
	}

	fn create_error(&self, cause: JsonParseErrorCause) -> JsonParseError {
		JsonParseError {
			row: self.row,
			column: self.column,
			cause,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	mod literal_value {
		use super::*;

		#[test]
		fn test_parse_false() {
			test_parse_literal_value(Json::Boolean(false), "false");
		}

		#[test]
		fn test_parse_true() {
			test_parse_literal_value(Json::Boolean(true), "true");
		}

		#[test]
		fn test_parse_null() {
			test_parse_literal_value(Json::Null, "null");
		}

		fn test_parse_literal_value(expected: Json, input: &str) {
			for (leading, trailing) in [(0, 0), (0, 3), (4, 0), (5, 6)] {
				let input = format!("{}{input}{}", generate_whitespace(leading), generate_whitespace(trailing));
				let mut parser = Parser::new(&input);
				assert_eq!(parser.parse(), Ok(expected.clone()));
			}
		}
	}

	#[test]
	fn test_trailing_value_returns_err() {
		assert!(parse_json("true false").is_err());
	}

	fn generate_whitespace(length: usize) -> String {
		let chars = (0..length)
			.map(|_| {
				rand::seq::SliceRandom::choose(&WHITESPACE_CHARACTERS[..], &mut rand::thread_rng())
					.copied()
					.unwrap()
			})
			.collect();
		String::from_utf8(chars).unwrap()
	}
}
