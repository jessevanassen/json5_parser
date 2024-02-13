use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Json {
	Null,
	Boolean(bool),
	Number(f64),
	String(String),
	Array(Vec<Json>),
	Object(HashMap<String, Json>),
}
