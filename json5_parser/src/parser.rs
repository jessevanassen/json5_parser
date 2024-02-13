use crate::Json;

pub fn parse_json(_input: impl AsRef<str>) -> Result<Json, ()> {
	todo!();
}

#[cfg(test)]
mod tests {
	use super::*;

	mod boolean {
		use super::*;

		#[test]
		fn test_parse_boolean() {
			assert_eq!(Ok(Json::Boolean(false)), parse_json("false"));
			assert_eq!(Ok(Json::Boolean(true)), parse_json("true"));
		}
	}
}
