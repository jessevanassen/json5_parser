use crate::Json;

type Result<T = Json> = ::std::result::Result<T, ()>;

pub fn parse_json(source: impl AsRef<str>) -> Result {
	let source = source.as_ref();
	Parser::new(source).parse_value()
}

struct Parser<'a> {
	source: &'a [u8],
	index: usize,
	row: usize,
	column: usize,
}

impl<'a> Parser<'a> {
	fn new(source: &'a str) -> Self {
		Parser {
			source: source.as_bytes(),
			index: 0,
			row: 0,
			column: 0,
		}
	}

	fn parse_value(&mut self) -> Result {
		match self.peek() {
			Some(b'f') => self.parse_literal(b"false", Json::Boolean(false)),
			Some(b't') => self.parse_literal(b"true", Json::Boolean(true)),
			Some(b'n') => self.parse_literal(b"null", Json::Null),
			_ => Err(()),
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
			None => Err(()),
			Some(char) if char != expected => Err(()),
			Some(_) => {
				self.consume().unwrap();
				Ok(())
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
			let mut parser = Parser::new(input);
			assert_eq!(parser.parse_value(), Ok(expected));
			assert_eq!(parser.row, 0);
			assert_eq!(parser.column, input.len());
			assert_eq!(parser.index, input.len());
		}
	}
}
