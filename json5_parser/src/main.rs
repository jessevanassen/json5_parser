use std::{io::{stdin, Read}, process::exit};

use json5_parser::parse_json;

fn main() {
	let input = {
		let mut buf = String::new();
		stdin().read_to_string(&mut buf).unwrap();
		buf
	};

	match parse_json(input) {
		Ok(json) => {
			println!("{json}");
		}
		Err(error) => {
			eprintln!("Error: {error}");
			exit(1);
		}
	}
}
