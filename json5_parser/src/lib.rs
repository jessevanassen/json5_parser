mod error;
pub use error::JsonParseError;

mod parser;
pub use parser::parse_json;
