use extendr_api::prelude::*;
use std::io::Cursor;

/// Repair malformed JSON strings
///
/// This function repairs malformed JSON strings, particularly those generated
/// by Large Language Models. It handles missing quotes, trailing commas,
/// unquoted keys, and other common JSON syntax errors.
///
/// @param json_str A character string containing malformed JSON
/// @return A character string containing the repaired JSON
/// @export
/// @examples
/// repair_json_str('{"key": "value",}')  # Removes trailing comma
/// repair_json_str('{key: "value"}')     # Adds quotes around unquoted key
#[extendr]
fn repair_json_str(json_str: &str) -> String {
    // Create default options
    let options = llm_json::RepairOptions::default();

    // Repair the JSON string
    match llm_json::repair_json(json_str, &options) {
        Ok(repaired) => repaired,
        Err(e) => {
            rprintln!("Error: Failed to repair JSON: {}", e);
            "{}".to_string()
        }
    }
}

/// Repair malformed JSON from a file
///
/// This function reads a file containing malformed JSON and repairs it.
///
/// @param path A character string with the file path
/// @return A character string containing the repaired JSON, or an empty object on error
/// @export
/// @examples
/// \dontrun{
/// repair_json_file("malformed.json")
/// }
#[extendr]
fn repair_json_file(path: &str) -> String {
    // First check if file exists
    if !std::path::Path::new(path).exists() {
        let msg = format!("File not found: '{}'. Please check the file path.", path);
        throw_r_error(&msg);
    }

    // Create default options
    let options = llm_json::RepairOptions::default();

    // Use llm_json's from_file function
    match llm_json::from_file(path, &options) {
        Ok(value) => {
            // Convert the Value to a JSON string
            match serde_json::to_string(&value) {
                Ok(json_str) => json_str,
                Err(e) => {
                    let msg = format!("Failed to serialize repaired JSON: {}", e);
                    throw_r_error(&msg);
                }
            }
        }
        Err(e) => {
            // Provide more specific error messages
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
/// @return A character string containing the repaired JSON, or an empty object on error
/// @export
/// @examples
/// \dontrun{
/// raw_data <- charToRaw('{"key": "value",}')
/// repair_json_raw(raw_data)
/// }
#[extendr]
fn repair_json_raw(raw_bytes: &[u8]) -> String {
    // Check if raw bytes is empty
    if raw_bytes.is_empty() {
        throw_r_error("Empty raw vector provided. Please provide valid JSON bytes.");
    }

    // Create default options
    let options = llm_json::RepairOptions::default();

    // Create a cursor from the raw bytes
    let cursor = Cursor::new(raw_bytes);

    // Use llm_json's load function
    match llm_json::load(cursor, &options) {
        Ok(value) => {
            // Convert the Value to a JSON string
            match serde_json::to_string(&value) {
                Ok(json_str) => json_str,
                Err(e) => {
                    let msg = format!("Failed to serialize repaired JSON: {}", e);
                    throw_r_error(&msg);
                }
            }
        }
        Err(e) => {
            // Provide clearer error message
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
}
