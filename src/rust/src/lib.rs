use extendr_api::prelude::*;
use serde_json::{to_string, Value};
use std::io::Cursor;
use std::path::Path;

mod json_to_r;
mod llm_json;
mod schema;

// Re-export what we need from llm_json
use llm_json::{from_file, load, loads, Int64Policy, RepairOptions};

/// Helper function to get a Schema from an Robj (either external pointer or list)
fn get_schema(schema_robj: &Robj) -> Option<schema::Schema> {
    if schema_robj.is_null() {
        return None;
    }

    // Check if it's an external pointer to a LLMJsonSchemaBuilt
    if let Some(built_schema) = <&schema::LLMJsonSchemaBuilt>::try_from(schema_robj).ok() {
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
fn apply_schema_to_robj(value: &Value, schema: &Robj, int64_policy: Int64Policy) -> Robj {
    if let Some(s) = get_schema(schema) {
        match s.apply(value) {
            Ok(robj) => robj,
            Err(e) => {
                throw_r_error(&format!("Schema validation failed: {}", e));
            }
        }
    } else {
        // No schema, use generic conversion
        json_to_r::json_value_to_robj(value, int64_policy)
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
    match to_string(&final_value) {
        Ok(json_str) => Robj::from(json_str),
        Err(e) => {
            throw_r_error(&format!("Failed to serialize JSON: {}", e));
        }
    }
}

fn create_repair_options(ensure_ascii: bool, int64: &str) -> RepairOptions {
    let int64_policy = match int64 {
        "string" => Int64Policy::String,
        "bit64" => Int64Policy::Bit64,
        _ => Int64Policy::Double, // default
    };

    RepairOptions {
        ensure_ascii,
        int64_policy,
        ..Default::default()
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
/// @param ensure_ascii Logical; if TRUE, escape non-ASCII characters
/// @param int64 Policy for handling 64-bit integers: "double" (default, may lose precision), "string" (preserves exact value), or "bit64" (requires bit64 package)
/// @return A character string containing the repaired JSON, or an R object if return_objects is TRUE
/// @seealso [repair_json_file()], [repair_json_raw()], [repair_json_conn()], [schema()], [json_schema()]
/// @export
/// @examples
/// repair_json_str('{"key": "value",}')  # Removes trailing comma
/// repair_json_str('{key: "value"}')     # Adds quotes around unquoted key
/// repair_json_str('{"key": "value"}', return_objects = TRUE)  # Returns R list
///
/// # Handle large integers (beyond i32 range)
/// json_str <- '{"id": 9007199254740993}'
/// repair_json_str(json_str, return_objects = TRUE, int64 = "string")  # Preserves as "9007199254740993"
/// repair_json_str(json_str, return_objects = TRUE, int64 = "double")  # May lose precision
/// repair_json_str(json_str, return_objects = TRUE, int64 = "bit64")   # Requires bit64 package
#[extendr]
fn repair_json_str(
    json_str: &str,
    #[default = "NULL"] schema: Robj,
    #[default = "FALSE"] return_objects: bool,
    #[default = "TRUE"] ensure_ascii: bool,
    #[default = "\"double\""] int64: &str,
) -> Robj {
    let options: RepairOptions = create_repair_options(ensure_ascii, int64);

    match loads(json_str, &options) {
        Ok(value) => {
            if return_objects {
                apply_schema_to_robj(&value, &schema, options.int64_policy)
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
/// @param ensure_ascii Logical; if TRUE, escape non-ASCII characters
/// @param int64 Policy for handling 64-bit integers: "double" (default, may lose precision), "string" (preserves exact value), or "bit64" (requires bit64 package)
/// @return A character string containing the repaired JSON, or an R object if return_objects is TRUE
/// @seealso [repair_json_str()], [repair_json_raw()], [repair_json_conn()], [schema()], [json_schema()]
/// @export
/// @examples
/// \dontrun{
/// repair_json_file("malformed.json")
/// repair_json_file("malformed.json", return_objects = TRUE)
/// repair_json_file("data.json", return_objects = TRUE, int64 = "string")  # Preserve large integers
/// }
#[extendr]
fn repair_json_file(
    path: &str,
    #[default = "NULL"] schema: Robj,
    #[default = "FALSE"] return_objects: bool,
    #[default = "TRUE"] ensure_ascii: bool,
    #[default = "\"double\""] int64: &str,
) -> Robj {
    if !Path::new(path).exists() {
        throw_r_error(&format!(
            "File not found: '{}'. Please check the file path.",
            path
        ));
    }

    let options: RepairOptions = create_repair_options(ensure_ascii, int64);

    match from_file(path, &options) {
        Ok(value) => {
            if return_objects {
                apply_schema_to_robj(&value, &schema, options.int64_policy)
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
/// @param ensure_ascii Logical; if TRUE, escape non-ASCII characters
/// @param int64 Policy for handling 64-bit integers: "double" (default, may lose precision), "string" (preserves exact value), or "bit64" (requires bit64 package)
/// @return A character string containing the repaired JSON, or an R object if return_objects is TRUE
/// @seealso [repair_json_str()], [repair_json_file()], [repair_json_conn()], [schema()], [json_schema()]
/// @export
/// @examples
/// \dontrun{
/// raw_data <- charToRaw('{"key": "value",}')
/// repair_json_raw(raw_data)
/// repair_json_raw(raw_data, return_objects = TRUE)
/// repair_json_raw(raw_data, return_objects = TRUE, int64 = "bit64")  # Use bit64 for large integers
/// }
#[extendr]
fn repair_json_raw(
    raw_bytes: &[u8],
    #[default = "NULL"] schema: Robj,
    #[default = "FALSE"] return_objects: bool,
    #[default = "TRUE"] ensure_ascii: bool,
    #[default = "\"double\""] int64: &str,
) -> Robj {
    if raw_bytes.is_empty() {
        throw_r_error("Empty raw vector provided. Please provide valid JSON bytes.");
    }

    let options: RepairOptions = create_repair_options(ensure_ascii, int64);
    let cursor = Cursor::new(raw_bytes);

    match load(cursor, &options) {
        Ok(value) => {
            if return_objects {
                apply_schema_to_robj(&value, &schema, options.int64_policy)
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
    fn repair_json_str;
    fn repair_json_file;
    fn repair_json_raw;
    use schema;
}
