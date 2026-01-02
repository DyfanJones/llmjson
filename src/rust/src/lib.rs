use extendr_api::prelude::*;
use serde_json::Value;
use std::io::Cursor;

mod json_to_r;
mod schema;

/// Helper function to get a Schema from an Robj (either external pointer or list)
fn get_schema(schema_robj: &Robj) -> Option<schema::Schema> {
    if schema_robj.is_null() {
        return None;
    }

    // Check if it's an external pointer to a BuiltSchema
    if let Some(built_schema) = <&schema::BuiltSchema>::try_from(schema_robj).ok() {
        return Some(built_schema.get_schema().clone());
    }

    // Otherwise, try to parse it as an R list schema (backwards compatibility)
    match schema::Schema::from_robj(schema_robj) {
        Ok(s) => Some(s),
        Err(e) => {
            throw_r_error(&format!("Invalid schema: {}", e));
        }
    }
}

/// Helper function to apply schema and return R objects
fn apply_schema_to_robj(value: &Value, schema: &Robj) -> Robj {
    if let Some(s) = get_schema(schema) {
        match s.apply(value) {
            Ok(robj) => robj,
            Err(e) => {
                throw_r_error(&format!("Schema validation failed: {}", e));
            }
        }
    } else {
        // No schema, use generic conversion
        json_to_r::json_value_to_robj(value)
    }
}

/// Helper function to apply schema defaults and return JSON string
fn apply_schema_to_json_string(value: &Value, schema: &Robj) -> Robj {
    // Apply schema defaults if provided
    let final_value = if let Some(s) = get_schema(schema) {
        match s.apply_defaults(value) {
            Ok(v) => v,
            Err(e) => {
                throw_r_error(&format!("Schema validation failed: {}", e));
            }
        }
    } else {
        value.clone()
    };

    // Convert to JSON string
    match serde_json::to_string(&final_value) {
        Ok(json_str) => Robj::from(json_str),
        Err(e) => {
            throw_r_error(&format!("Failed to serialize JSON: {}", e));
        }
    }
}

/// Repair malformed JSON strings
///
/// This function repairs malformed JSON strings, particularly those generated
/// by Large Language Models. It handles missing quotes, trailing commas,
/// unquoted keys, and other common JSON syntax errors.
///
/// @param json_str A character string containing malformed JSON
/// @param schema Optional schema definition for validation and type conversion
/// @param return_objects Logical indicating whether to return R objects (TRUE) or JSON string (FALSE, default)
/// @return A character string containing the repaired JSON, or an R object if return_objects is TRUE
/// @export
/// @examples
/// repair_json_str('{"key": "value",}')  # Removes trailing comma
/// repair_json_str('{key: "value"}')     # Adds quotes around unquoted key
/// repair_json_str('{"key": "value"}', return_objects = TRUE)  # Returns R list
#[extendr(r_name = "repair_json_str")]
fn repair_json_str_impl(json_str: &str, #[default = "NULL"] schema: Robj, #[default = "FALSE"] return_objects: bool) -> Robj {
    let options = llm_json::RepairOptions::default();

    match llm_json::loads(json_str, &options) {
        Ok(value) => {
            if return_objects {
                apply_schema_to_robj(&value, &schema)
            } else {
                apply_schema_to_json_string(&value, &schema)
            }
        }
        Err(e) => {
            rprintln!("Error: Failed to repair JSON: {}", e);
            if return_objects {
                r!(NULL)
            } else {
                Robj::from("{}")
            }
        }
    }
}

/// Repair malformed JSON from a file
///
/// This function reads a file containing malformed JSON and repairs it.
///
/// @param path A character string with the file path
/// @param schema Optional schema definition for validation and type conversion
/// @param return_objects Logical indicating whether to return R objects (TRUE) or JSON string (FALSE, default)
/// @return A character string containing the repaired JSON, or an R object if return_objects is TRUE
/// @export
/// @examples
/// \dontrun{
/// repair_json_file("malformed.json")
/// repair_json_file("malformed.json", return_objects = TRUE)
/// }
#[extendr(r_name = "repair_json_file")]
fn repair_json_file_impl(path: &str, #[default = "NULL"] schema: Robj, #[default = "FALSE"] return_objects: bool) -> Robj {
    if !std::path::Path::new(path).exists() {
        throw_r_error(&format!("File not found: '{}'. Please check the file path.", path));
    }

    let options = llm_json::RepairOptions::default();

    match llm_json::from_file(path, &options) {
        Ok(value) => {
            if return_objects {
                apply_schema_to_robj(&value, &schema)
            } else {
                apply_schema_to_json_string(&value, &schema)
            }
        }
        Err(e) => {
            let error_msg = if e.to_string().contains("permission") {
                format!("Permission denied reading file '{}'", path)
            } else if e.to_string().contains("Is a directory") {
                format!("'{}' is a directory, not a file", path)
            } else {
                format!("Failed to repair JSON from file '{}': {}", path, e)
            };
            throw_r_error(&error_msg);
        }
    }
}

/// Repair malformed JSON from raw bytes
///
/// This function repairs malformed JSON from a raw vector of bytes.
///
/// @param raw_bytes A raw vector containing malformed JSON bytes
/// @param schema Optional schema definition for validation and type conversion
/// @param return_objects Logical indicating whether to return R objects (TRUE) or JSON string (FALSE, default)
/// @return A character string containing the repaired JSON, or an R object if return_objects is TRUE
/// @export
/// @examples
/// \dontrun{
/// raw_data <- charToRaw('{"key": "value",}')
/// repair_json_raw(raw_data)
/// repair_json_raw(raw_data, return_objects = TRUE)
/// }
#[extendr(r_name = "repair_json_raw")]
fn repair_json_raw_impl(raw_bytes: &[u8], #[default = "NULL"] schema: Robj, #[default = "FALSE"] return_objects: bool) -> Robj {
    if raw_bytes.is_empty() {
        throw_r_error("Empty raw vector provided. Please provide valid JSON bytes.");
    }

    let options = llm_json::RepairOptions::default();
    let cursor = Cursor::new(raw_bytes);

    match llm_json::load(cursor, &options) {
        Ok(value) => {
            if return_objects {
                apply_schema_to_robj(&value, &schema)
            } else {
                apply_schema_to_json_string(&value, &schema)
            }
        }
        Err(e) => {
            let error_msg = if e.to_string().contains("UTF") || e.to_string().contains("utf") {
                format!("Invalid UTF-8 data in raw bytes: {}", e)
            } else {
                format!("Failed to repair JSON from raw bytes: {}", e)
            };
            throw_r_error(&error_msg);
        }
    }
}

// Macro to generate exports
extendr_module! {
    mod llmjson;
    fn repair_json_str_impl;
    fn repair_json_file_impl;
    fn repair_json_raw_impl;
    use schema;
}
