use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

#[wasm_bindgen(js_name="parseJson")]
pub fn parse_json(source: &str) -> JsValue {
	let value = json5_parser::parse_json(source).unwrap();
	serde_wasm_bindgen::to_value(&value).unwrap()
}
