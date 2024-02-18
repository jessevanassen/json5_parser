use crate::{error::JsonParseErrorCause, JsonParseError};
use serde_json::Value as Json;

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
		self.consume_ignorables()?;

		let result = self.parse_value()?;

		self.consume_ignorables()?;

		if self.peek().is_some() {
			return Err(self.create_error(JsonParseErrorCause::UnexpectedCharacter));
		}

		Ok(result)
	}

	fn parse_value(&mut self) -> Result {
		match self.peek_some()? {
			b'f' => self.parse_literal(b"false", Json::Bool(false)),
			b't' => self.parse_literal(b"true", Json::Bool(true)),
			b'n' => self.parse_literal(b"null", Json::Null),
			b'0'..=b'9' | b'-' | b'+' | b'.' => self.parse_number(),
			b'"' | b'\'' => self.parse_string(),
			b'[' => self.parse_array(),
			b'{' => self.parse_object(),
			_ => Err(self.create_error(JsonParseErrorCause::UnexpectedCharacter)),
		}
	}

	fn parse_literal(&mut self, literal: &[u8], result: Json) -> Result {
		self.match_str(literal)?;
		Ok(result)
	}

	fn parse_number(&mut self) -> Result {
		let start = self.index;

		self.consume_if(|ch| ch == b'-' || ch == b'+');

		self.consume_while(|ch| ch.is_ascii_digit());

		if self.consume_if(|ch| ch == b'.').is_some() {
			self.consume_while(|ch| ch.is_ascii_digit());
		}

		if self.consume_if(|ch| ch == b'e' || ch == b'E').is_some() {
			self.consume_if(|ch| ch == b'+' || ch == b'-');
			self.match_digit()?;
			self.consume_while(|ch| ch.is_ascii_digit());
		}

		self.source[start..self.index]
			.parse::<f64>()
			.map(number)
			.map_err(|_| self.create_error(JsonParseErrorCause::InvalidNumber))
	}

	fn parse_string(&mut self) -> Result {
		fn parse_escape(parser: &mut Parser) -> Result<char> {
			Ok(match parser.match_any()? {
				b'"' => '"',
				b'\'' => '\'',
				b'\\' => '\\',
				b'/' => '/',
				b'b' => '\u{08}',
				b'f' => '\u{0C}',
				b'n' | b'\n' => '\n',
				b'r' | b'\r' => '\r',
				b't' => '\t',
				b'u' => {
					/* TODO: surrogate pairs */
					let mut code_point = 0u32;

					for _ in 0..4 {
						code_point *= 16;

						let value = parse_hexdigit(parser.match_hexdigit()?);
						code_point += value as u32;
					}

					char::from_u32(code_point).ok_or_else(|| {
						parser.create_error(JsonParseErrorCause::InvalidUnicodeCodePoint {
							value: code_point,
						})
					})?
				}
				_ => Err(parser.create_error(JsonParseErrorCause::BadEscapeCharacter))?,
			})
		}

		let mut result = Vec::<u8>::new();
		let quote_style = self.match_if(|ch| ch == b'"' || ch == b'\'')?;

		while self
			.peek()
			.is_some_and(|ch| ch != quote_style && ch != b'\n')
		{
			if self.consume_if(|ch| ch == b'\\').is_some() {
				if self.consume_str(b"\r\n") {
					/* Exceptional case: a single `\` in front of a \r\n
					 * sequence escapes both the \r and \n characters. ` */
					result.push(b'\r');
					result.push(b'\n');
				} else {
					let char = parse_escape(self)?;

					let mut bytes = [0; 4];
					char.encode_utf8(&mut bytes);
					result.extend_from_slice(&bytes[..char.len_utf8()]);
				}
			} else {
				result.push(self.match_any()?);
			}
		}

		self.match_char(quote_style)?;

		let result = unsafe {
			/* The input is a valid UTF-8 string, and we're only ending up here
			 * if we matched a " character, which means the source bytes are
			 * also valid UTF-8. */
			String::from_utf8_unchecked(result)
		};

		Ok(Json::String(result))
	}

	fn parse_array(&mut self) -> Result {
		self.match_char(b'[')?;
		self.consume_ignorables()?;

		let mut content = Vec::<Json>::new();

		if self.peek_some()? != b']' {
			loop {
				content.push(self.parse_value()?);
				self.consume_ignorables()?;

				if self.peek_some()? == b']' {
					break;
				}

				self.match_char(b',')?;
				self.consume_ignorables()?;

				if self.peek_some()? == b']' {
					break;
				}
			}
		}

		self.match_char(b']')?;

		Ok(Json::Array(content))
	}

	fn parse_object(&mut self) -> Result {
		self.match_char(b'{')?;
		self.consume_ignorables()?;

		let mut entries = serde_json::Map::new();

		if self.peek_some()? != b'}' {
			loop {
				let Json::String(key) = self.parse_key()? else {
					unreachable!()
				};
				self.consume_ignorables()?;
				self.match_char(b':')?;
				self.consume_ignorables()?;
				let value = self.parse_value()?;
				self.consume_ignorables()?;

				entries.insert(key, value);

				if self.peek_some()? == b'}' {
					break;
				}

				self.match_char(b',')?;
				self.consume_ignorables()?;

				if self.peek_some()? == b'}' {
					break;
				}
			}
		}

		self.match_char(b'}')?;

		Ok(Json::Object(entries))
	}

	fn parse_key(&mut self) -> Result {
		fn parse_identifier(parser: &mut Parser) -> Result {
			fn is_identifier_symbol(ch: u8) -> bool {
				matches!(ch, b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'$')
			}

			let start = parser.index;
			parser.match_if(is_identifier_symbol)?;
			parser.consume_while(|ch| is_identifier_symbol(ch) || ch.is_ascii_digit());

			let identifier = parser.source[start..parser.index].to_string();
			Ok(Json::String(identifier))
		}

		if let identifier @ Ok(_) = parse_identifier(self) {
			identifier
		} else {
			self.parse_string()
		}
	}

	/// Things that don't contribute to the actual value, like comments and
	/// whitespace.
	fn consume_ignorables(&mut self) -> Result<()> {
		fn consume_whitespace(parser: &mut Parser) {
			parser.consume_while(|ch| WHITESPACE_CHARACTERS.contains(&ch));
		}

		fn consume_comments(parser: &mut Parser) -> Result<()> {
			fn consume_single_line_comment(parser: &mut Parser) -> Result<()> {
				parser.match_str(b"//")?;
				parser.consume_while(|ch| ch != b'\r' && ch != b'\n');
				parser.consume_if(|ch| ch == b'\r');
				parser.consume_if(|ch| ch == b'\n');
				Ok(())
			}

			fn consume_multi_line_comment(parser: &mut Parser) -> Result<()> {
				parser.match_str(b"/*")?;

				while !matches!(
					(parser.peek_n(0), parser.peek_n(1)),
					(Some(b'*'), Some(b'/'))
				) {
					parser.match_any()?;
				}

				parser.match_str(b"*/")?;
				Ok(())
			}

			loop {
				match (parser.peek_n(0), parser.peek_n(1)) {
					(Some(b'/'), Some(b'/')) => consume_single_line_comment(parser)?,
					(Some(b'/'), Some(b'*')) => consume_multi_line_comment(parser)?,
					_ => {
						return Ok(());
					}
				}
				consume_whitespace(parser);
			}
		}

		consume_whitespace(self);
		consume_comments(self)?;

		Ok(())
	}

	fn peek(&self) -> Option<u8> {
		self.peek_n(0)
	}

	/// Peek n characters ahead.
	fn peek_n(&self, n: usize) -> Option<u8> {
		self.source.as_bytes().get(self.index + n).copied()
	}

	fn peek_some(&self) -> Result<u8> {
		self.peek()
			.ok_or_else(|| self.create_error(JsonParseErrorCause::UnexpectedEndOfFile))
	}

	fn peek_str(&self, str: &[u8]) -> bool {
		str.iter()
			.copied()
			.enumerate()
			.all(|(i, ch)| self.peek_n(i) == Some(ch))
	}

	fn match_any(&mut self) -> Result<u8> {
		self.consume()
			.ok_or_else(|| self.create_error(JsonParseErrorCause::UnexpectedEndOfFile))
	}

	fn match_if(&mut self, predicate: impl Fn(u8) -> bool) -> Result<u8> {
		self.consume_if(predicate)
			.ok_or_else(|| self.create_error(JsonParseErrorCause::UnexpectedCharacter))
	}

	fn match_digit(&mut self) -> Result<u8> {
		self.match_if(|ch| ch.is_ascii_digit())
			.map_err(|error| JsonParseError {
				cause: JsonParseErrorCause::ExpectedDigit,
				..error
			})
	}

	fn match_hexdigit(&mut self) -> Result<u8> {
		self.match_if(|ch| ch.is_ascii_hexdigit())
			.map_err(|error| JsonParseError {
				cause: JsonParseErrorCause::ExpectedHexadecimalDigit,
				..error
			})
	}

	fn match_char(&mut self, expected: u8) -> Result<u8> {
		self.match_if(|ch| ch == expected)
			.map_err(|error| JsonParseError {
				cause: JsonParseErrorCause::MismatchedCharacter { expected },
				..error
			})
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

	fn consume_if(&mut self, predicate: impl Fn(u8) -> bool) -> Option<u8> {
		if self.peek().is_some_and(predicate) {
			self.consume()
		} else {
			None
		}
	}

	fn consume_while(&mut self, predicate: fn(u8) -> bool) {
		loop {
			if self.consume_if(predicate).is_none() {
				break;
			}
		}
	}

	fn consume_str(&mut self, str: &[u8]) -> bool {
		if self.peek_str(str) {
			self.index += str.len();
			true
		} else {
			false
		}
	}

	fn create_error(&self, cause: JsonParseErrorCause) -> JsonParseError {
		JsonParseError {
			index: self.index,
			row: self.row,
			column: self.column,
			cause,
		}
	}
}

fn parse_hexdigit(digit: u8) -> u8 {
	match digit {
		b'0'..=b'9' => digit - b'0',
		b'A'..=b'F' => digit - b'A' + 10,
		b'a'..=b'f' => digit - b'a' + 10,
		_ => panic!("Not a hexdigit: {digit}"),
	}
}

fn number(n: f64) -> Json {
	serde_json::Number::from_f64(n)
		.expect("Not a valid serde_json::Number")
		.into()
}

#[cfg(test)]
mod tests {
	use super::*;

	mod literal_value {
		use super::*;

		#[test]
		fn test_parse_false() {
			test_parse_literal_value(Json::Bool(false), "false");
		}

		#[test]
		fn test_parse_true() {
			test_parse_literal_value(Json::Bool(true), "true");
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
			assert_eq!(parse_json("0"), Ok(number(0.)));
			assert_eq!(parse_json("12345"), Ok(number(12345.)));
		}

		#[test]
		fn test_negative_integer() {
			assert!(parse_json("-").is_err());
			assert_eq!(parse_json("-1"), Ok(number(-1.)));
			assert_eq!(parse_json("-12345"), Ok(number(-12345.)));
		}

		#[test]
		fn test_positive_floats() {
			assert_eq!(parse_json("0."), Ok(number(0.0)));
			assert_eq!(parse_json("1.5"), Ok(number(1.5)));
			assert_eq!(parse_json("123.25"), Ok(number(123.25)));
		}

		#[test]
		fn test_negative_floats() {
			assert_eq!(parse_json("-0."), Ok(number(-0.0)));
			assert_eq!(parse_json("-1.5"), Ok(number(-1.5)));
			assert_eq!(parse_json("-123.25"), Ok(number(-123.25)));
		}

		#[test]
		fn test_exponents() {
			for input in ["1e", "1E", "1e+", "1e-"] {
				assert!(parse_json(input).is_err());
			}

			assert_eq!(parse_json("1e3"), Ok(number(1000.)));
			assert_eq!(parse_json("1E3"), Ok(number(1000.)));
			assert_eq!(parse_json("1e+3"), Ok(number(1000.)));
			assert_eq!(parse_json("1E+3"), Ok(number(1000.)));
			assert_eq!(parse_json("5e-1"), Ok(number(0.5)));
			assert_eq!(parse_json("5E-1"), Ok(number(0.5)));
		}
	}

	mod string {
		use super::*;

		#[test]
		fn test_parse_string() {
			assert_string(r#""""#, "");
			assert_string(r#""a""#, "a");
			assert_string(r#""abc""#, "abc");
		}

		#[test]
		fn test_parse_multiline_string() {
			assert_string("\"Line 1\\\nLine 2\"", "Line 1\nLine 2");
		}

		#[test]
		fn test_newline_in_string_returns_error() {
			assert_error(
				"\"abc\ndef\"",
				JsonParseErrorCause::MismatchedCharacter { expected: b'"' },
			);
		}

		#[test]
		fn test_parse_escape_characters() {
			for (escape, char) in [
				('"', '"'),
				('\'', '\''),
				('\\', '\\'),
				('/', '/'),
				('b', '\u{08}'),
				('f', '\u{0C}'),
				('n', '\n'),
				('r', '\r'),
				('t', '\t'),
			] {
				assert_eq!(
					parse_json(format!("\"\\{escape}\"")),
					Ok(Json::String(format!("{char}"))),
					"Escape sequence: \\{escape}",
				);
			}
		}

		#[test]
		fn test_unicode_escape() {
			assert_string(r#""\u000A""#, "\n");
			assert_string(r#""\u00DF""#, "ß");
			assert_string(r#""\u03BB""#, "λ");
		}

		#[test]
		fn test_bad_escape_char() {
			assert_error(r#""\q""#, JsonParseErrorCause::BadEscapeCharacter)
		}

		#[test]
		fn test_single_quoted_string() {
			assert_string("'hello'", "hello");
		}

		fn assert_string(input: impl AsRef<str>, str: &str) {
			assert_eq!(parse_json(input), Ok(Json::String(String::from(str))),)
		}
	}

	mod array {
		use super::*;

		#[test]
		fn test_parse_empty_array() {
			assert_json("[]", Json::Array(Default::default()));
		}

		#[test]
		fn test_parse_array_with_primitives() {
			assert_json(
				"[true, 0,null]",
				Json::Array(vec![Json::Bool(true), number(0.), Json::Null]),
			)
		}

		#[test]
		fn test_parse_nested_arrays() {
			assert_json(
				"[[true, 0],[null]]",
				Json::Array(vec![
					Json::Array(vec![Json::Bool(true), number(0.)]),
					Json::Array(vec![Json::Null]),
				]),
			)
		}

		#[test]
		fn test_no_separator() {
			assert_error(
				"[true false]",
				JsonParseErrorCause::MismatchedCharacter { expected: b',' },
			)
		}

		#[test]
		fn test_bare_comma() {
			assert_error("[, ]", JsonParseErrorCause::UnexpectedCharacter);
		}

		#[test]
		fn test_trailing_comma() {
			assert_json(
				"[true, false, ]",
				Json::Array(vec![true.into(), false.into()]),
			);
		}

		#[test]
		fn test_multiple_trailing_commas() {
			assert_error("[true, false,, ]", JsonParseErrorCause::UnexpectedCharacter);
		}
	}

	mod object {
		use super::*;

		#[test]
		fn test_parse_empty_object() {
			assert_json("{}", Json::Object(Default::default()));
		}

		#[test]
		fn test_parse_object_with_primitives() {
			let mut entries = serde_json::Map::new();
			entries.insert(String::from("first"), Json::Bool(true));
			entries.insert(String::from("second"), number(0.));

			assert_json(r#"{ "first": true, "second": 0 }"#, Json::Object(entries))
		}

		#[test]
		fn test_parse_nested_object() {
			let mut leaf = serde_json::Map::new();
			leaf.insert(String::from("second"), number(0.));

			let mut root = serde_json::Map::new();
			root.insert(String::from("first"), Json::Object(leaf));

			assert_json(r#"{ "first": {"second":0} }"#, Json::Object(root))
		}

		#[test]
		fn test_js_name_as_key() {
			fn input(key: &str) -> String {
				format!(r"{{ {key}: null }}")
			}

			fn test(key: &str) {
				let mut entries = serde_json::Map::new();
				entries.insert(String::from(key), Json::Null);
				let expected = Json::Object(entries);

				assert_json(input(key), expected);
			}

			test("a");
			test("Z");
			test("$");
			test("_");
			test("_1");
			test("aBcD");
			test("AbCd");
			test("_123abc$");

			assert!(parse_json(input("1")).is_err());
			assert!(parse_json(input("123")).is_err());
			assert!(parse_json(input("1x")).is_err());
		}

		#[test]
		fn test_no_separator() {
			assert_error(
				r#"{ "first": 1 "second": 2 }"#,
				JsonParseErrorCause::MismatchedCharacter { expected: b',' },
			);
		}

		#[test]
		fn test_bare_comma() {
			assert_error("{, }", JsonParseErrorCause::UnexpectedCharacter);
		}

		#[test]
		fn test_trailing_comma() {
			let mut entries = serde_json::Map::new();
			entries.insert(String::from("first"), Json::Bool(true));
			entries.insert(String::from("second"), number(0.));

			assert_json(r#"{ "first": true, "second": 0, }"#, Json::Object(entries))
		}

		#[test]
		fn test_multiple_trailing_commas() {
			assert_error(
				r#"{ "first": true,, }"#,
				JsonParseErrorCause::UnexpectedCharacter,
			);
		}
	}

	#[test]
	fn test_trailing_value_returns_err() {
		assert!(parse_json("true false").is_err());
	}

	fn assert_json(input: impl AsRef<str>, expected: Json) {
		assert_eq!(parse_json(input), Ok(expected),)
	}

	fn assert_error(input: impl AsRef<str>, expected_cause: JsonParseErrorCause) {
		let result = parse_json(input);
		match result {
			Err(JsonParseError { cause, .. }) if cause == expected_cause => {
				/* Expected result */
			}
			Ok(_) => panic!("Expected Err with cause {expected_cause:?}, but was {result:?}"),
			Err(JsonParseError { cause, .. }) => {
				panic!("Expected Err with cause {expected_cause:?}, but had cause {cause:?}")
			}
		}
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

	mod comments {
		use super::*;

		#[test]
		fn comments_at_start() {
			assert_json("// This is a bool\nfalse", Json::Bool(false));
			assert_json("/* This is a bool */ false", Json::Bool(false));
			assert_json("/**\n * This is a bool\n **/\nfalse", Json::Bool(false));
		}

		#[test]
		fn comments_at_end() {
			assert_json("false // This is a bool\n", Json::Bool(false));
			assert_json("false /* This was a bool */", Json::Bool(false));
			assert_json("false /**\n * This was a bool\n **/\n", Json::Bool(false));
		}

		#[test]
		fn repeated_comments() {
			assert_json(
				"// This is a bool\n// This is a bool\nfalse",
				Json::Bool(false),
			);
			assert_json(
				"/* This is a bool */\n/* This is a bool *//* This is a bool */ false",
				Json::Bool(false),
			);

			assert_json(
				"false // This is a bool\n// This is a bool\n",
				Json::Bool(false),
			);
			assert_json(
				"false /* This is a bool */\n/* This is a bool *//* This is a bool */",
				Json::Bool(false),
			);
		}

		#[test]
		fn comment_within_array() {
			let expected = Json::Array(vec![Json::Bool(true), Json::Bool(false)]);
			assert_json("[/* null, */ true, false ]", expected.clone());
			assert_json("[true, /* null, */ false ]", expected.clone());
			assert_json("[true, false, /* null */ ]", expected.clone());
		}

		#[test]
		fn comment_within_object() {
			let mut entries = serde_json::Map::new();
			entries.insert(String::from("first"), Json::Bool(true));
			entries.insert(String::from("second"), number(0.));
			let expected = Json::Object(entries);

			assert_json(
				r#"{ /* null, */ "first": true, "second": 0 }"#,
				expected.clone(),
			);
			assert_json(
				r#"{ "first": true, /* null, */ "second": 0 }"#,
				expected.clone(),
			);
			assert_json(
				r#"{ "first": true, "second": 0 /* null, */ }"#,
				expected.clone(),
			);
		}

		#[test]
		fn unfinished_multiline_comment() {
			assert_error(
				"false /* Not closing!",
				JsonParseErrorCause::UnexpectedEndOfFile,
			);
			assert_error(
				"false /* Still\n * not\n *closing!",
				JsonParseErrorCause::UnexpectedEndOfFile,
			);
		}
	}
}
