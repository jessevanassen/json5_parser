use crate::{error::JsonParseErrorCause, Json, JsonParseError};

const WHITESPACE_CHARACTERS: [u8; 4] = [b' ', b'\n', b'\t', b'\r'];

type Result<T = Json> = ::std::result::Result<T, JsonParseError>;

pub fn parse_json(source: impl AsRef<str>) -> Result {
	let source = source.as_ref();
	Parser::new(source).parse()
}

struct Parser<'a> {
	source: &'a str,
	index: usize,
	row: usize,
	column: usize,
}

impl<'a> Parser<'a> {
	pub fn new(source: &'a str) -> Self {
		Parser {
			source,
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
			return Err(
				self.create_error(JsonParseErrorCause::UnexpectedCharacter { char: char as char })
			);
		}

		Ok(result)
	}

	fn parse_value(&mut self) -> Result {
		match self.peek() {
			Some(b'f') => self.parse_literal(b"false", Json::Boolean(false)),
			Some(b't') => self.parse_literal(b"true", Json::Boolean(true)),
			Some(b'n') => self.parse_literal(b"null", Json::Null),
			Some(b'-' | b'0'..=b'9') => self.parse_number(),
			Some(c) => {
				Err(self.create_error(JsonParseErrorCause::UnexpectedCharacter { char: c as char }))
			}
			None => Err(self.create_error(JsonParseErrorCause::UnexpectedEndOfFile)),
		}
	}

	fn parse_literal(&mut self, literal: &[u8], result: Json) -> Result {
		self.match_str(literal)?;
		Ok(result)
	}

	fn parse_number(&mut self) -> Result {
		let start = self.index;

		self.consume_if(|ch| ch == b'-');

		self.match_predicate(|ch| ch.is_ascii_digit())?;
		self.consume_while(|ch| ch.is_ascii_digit());

		if self.consume_if(|ch| ch == b'.') {
			self.match_predicate(|ch| ch.is_ascii_digit())?;
			self.consume_while(|ch| ch.is_ascii_digit());
		}

		if self.consume_if(|ch| ch == b'e' || ch == b'E') {
			self.consume_if(|ch| ch == b'+' || ch == b'-');
			self.match_predicate(|ch| ch.is_ascii_digit())?;
			self.consume_while(|ch| ch.is_ascii_digit());
		}

		self.source[start..self.index]
			.parse()
			.map(Json::Number)
			.map_err(|_| self.create_error(JsonParseErrorCause::InvalidNumber))
	}

	fn peek(&self) -> Option<u8> {
		self.source.as_bytes().get(self.index).copied()
	}

	fn match_predicate(&mut self, predicate: fn(u8) -> bool) -> Result<u8> {
		match self.peek() {
			Some(ch) if predicate(ch) => Ok(self.consume().unwrap()),
			Some(ch) => {
				Err(self
					.create_error(JsonParseErrorCause::UnexpectedCharacter { char: ch as char }))
			}
			None => Err(self.create_error(JsonParseErrorCause::UnexpectedEndOfFile)),
		}
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

	fn consume_if(&mut self, predicate: fn(u8) -> bool) -> bool {
		if self.peek().is_some_and(predicate) {
			self.consume();
			true
		} else {
			false
		}
	}

	fn consume_while(&mut self, predicate: fn(u8) -> bool) {
		while self.consume_if(predicate) {}
	}

	fn consume_whitespace(&mut self) {
		self.consume_while(|ch| WHITESPACE_CHARACTERS.contains(&ch));
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
				let input = format!(
					"{}{input}{}",
					generate_whitespace(leading),
					generate_whitespace(trailing)
				);
				assert_eq!(parse_json(&input), Ok(expected.clone()));
			}
		}
	}

	mod numbers {
		use super::*;

		#[test]
		fn test_positive_integer() {
			assert_eq!(parse_json("0"), Ok(Json::Number(0.)));
			assert_eq!(parse_json("12345"), Ok(Json::Number(12345.)));
		}

		#[test]
		fn test_negative_integer() {
			assert!(parse_json("-").is_err());
			assert_eq!(parse_json("-1"), Ok(Json::Number(-1.)));
			assert_eq!(parse_json("-12345"), Ok(Json::Number(-12345.)));
		}

		#[test]
		fn test_positive_floats() {
			assert!(parse_json("0.").is_err());
			assert_eq!(parse_json("1.5"), Ok(Json::Number(1.5)));
			assert_eq!(parse_json("123.25"), Ok(Json::Number(123.25)));
		}

		#[test]
		fn test_negative_floats() {
			assert!(parse_json("-0.").is_err());
			assert_eq!(parse_json("-1.5"), Ok(Json::Number(-1.5)));
			assert_eq!(parse_json("-123.25"), Ok(Json::Number(-123.25)));
		}

		#[test]
		fn test_exponents() {
			for input in ["1e", "1E", "1e+", "1e-"] {
				assert!(parse_json(input).is_err());
			}

			assert_eq!(parse_json("1e3"), Ok(Json::Number(1000.)));
			assert_eq!(parse_json("1E3"), Ok(Json::Number(1000.)));
			assert_eq!(parse_json("1e+3"), Ok(Json::Number(1000.)));
			assert_eq!(parse_json("1E+3"), Ok(Json::Number(1000.)));
			assert_eq!(parse_json("5e-1"), Ok(Json::Number(0.5)));
			assert_eq!(parse_json("5E-1"), Ok(Json::Number(0.5)));
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
