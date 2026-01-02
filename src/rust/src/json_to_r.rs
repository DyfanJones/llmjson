use extendr_api::prelude::*;
use serde_json::Value;

/// Convert a serde_json::Value to an R object (Robj)
///
/// This function recursively converts JSON values to their R equivalents:
/// - JSON objects become named R lists
/// - JSON arrays become R vectors (if homogeneous) or lists (if heterogeneous)
/// - JSON primitives become R scalars
/// - JSON null becomes R NULL
pub fn json_value_to_robj(value: &Value) -> Robj {
    match value {
        Value::Null => r!(NULL),
        Value::Bool(b) => Robj::from(*b),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                // Check if it fits in i32 for R integer
                if i >= i32::MIN as i64 && i <= i32::MAX as i64 {
                    Robj::from(i as i32)
                } else {
                    // Use double for large integers
                    Robj::from(i as f64)
                }
            } else if let Some(u) = n.as_u64() {
                Robj::from(u as f64)
            } else if let Some(f) = n.as_f64() {
                Robj::from(f)
            } else {
                r!(NULL)
            }
        }
        Value::String(s) => Robj::from(s.as_str()),
        Value::Array(arr) => convert_array_to_robj(arr),
        Value::Object(obj) => {
            // Convert to named R list
            let mut names = Vec::with_capacity(obj.len());
            let mut values = Vec::with_capacity(obj.len());

            for (key, val) in obj.iter() {
                names.push(key.as_str());
                values.push(json_value_to_robj(val));
            }

            let mut list = List::from_values(values);
            list.set_names(names).unwrap();
            list.into_robj()
        }
    }
}

/// Convert a JSON array to an R object
///
/// This function attempts to create homogeneous R vectors when possible,
/// falling back to R lists for heterogeneous arrays.
fn convert_array_to_robj(arr: &[Value]) -> Robj {
    if arr.is_empty() {
        return List::from_values(Vec::<Robj>::new()).into_robj();
    }

    // Check if all elements are the same type
    let first_type = get_json_type(&arr[0]);
    let is_homogeneous = arr.iter().all(|v| get_json_type(v) == first_type);

    if !is_homogeneous {
        // Heterogeneous array - convert to R list
        let values: Vec<Robj> = arr.iter().map(json_value_to_robj).collect();
        return List::from_values(values).into_robj();
    }

    // Homogeneous array - try to create typed vector
    match first_type {
        JsonType::Null => {
            // Array of nulls - return list of NULLs
            let values: Vec<Robj> = arr.iter().map(|_| r!(NULL)).collect();
            List::from_values(values).into_robj()
        }
        JsonType::Bool => {
            // Logical vector
            let values: Vec<bool> = arr
                .iter()
                .map(|v| {
                    if let Value::Bool(b) = v {
                        *b
                    } else {
                        false // This shouldn't happen due to homogeneity check
                    }
                })
                .collect();
            Robj::from(values)
        }
        JsonType::Number => {
            // Check if all numbers are integers
            let all_integers = arr.iter().all(|v| {
                if let Value::Number(n) = v {
                    n.as_i64().is_some()
                        && n.as_i64().unwrap() >= i32::MIN as i64
                        && n.as_i64().unwrap() <= i32::MAX as i64
                } else {
                    false
                }
            });

            if all_integers {
                // Integer vector
                let values: Vec<i32> = arr
                    .iter()
                    .map(|v| {
                        if let Value::Number(n) = v {
                            n.as_i64().unwrap() as i32
                        } else {
                            i32::MIN // This shouldn't happen due to our check
                        }
                    })
                    .collect();
                Robj::from(values)
            } else {
                // Double vector
                let values: Vec<f64> = arr
                    .iter()
                    .map(|v| {
                        if let Value::Number(n) = v {
                            n.as_f64().unwrap_or(0.0)
                        } else {
                            0.0 // This shouldn't happen due to our check
                        }
                    })
                    .collect();
                Robj::from(values)
            }
        }
        JsonType::String => {
            // Character vector
            let values: Vec<&str> = arr
                .iter()
                .map(|v| {
                    if let Value::String(s) = v {
                        s.as_str()
                    } else {
                        ""
                    }
                })
                .collect();
            Robj::from(values)
        }
        JsonType::Array | JsonType::Object => {
            // Nested structures - convert to list
            let values: Vec<Robj> = arr.iter().map(json_value_to_robj).collect();
            List::from_values(values).into_robj()
        }
    }
}

/// Enum to represent JSON value types for homogeneity checking
#[derive(Debug, PartialEq)]
enum JsonType {
    Null,
    Bool,
    Number,
    String,
    Array,
    Object,
}

/// Get the type of a JSON value
fn get_json_type(value: &Value) -> JsonType {
    match value {
        Value::Null => JsonType::Null,
        Value::Bool(_) => JsonType::Bool,
        Value::Number(_) => JsonType::Number,
        Value::String(_) => JsonType::String,
        Value::Array(_) => JsonType::Array,
        Value::Object(_) => JsonType::Object,
    }
}
