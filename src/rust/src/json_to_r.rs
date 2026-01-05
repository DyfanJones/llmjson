use extendr_api::prelude::*;
use serde_json::Value;
use crate::llm_json::Int64Policy;

/// Convert a serde_json::Value to an R object (Robj)
///
/// This function recursively converts JSON values to their R equivalents:
/// - JSON objects become named R lists
/// - JSON arrays become R vectors (if homogeneous) or lists (if heterogeneous)
/// - JSON primitives become R scalars
/// - JSON null becomes R NULL
///
/// # Arguments
/// * `value` - The JSON value to convert
/// * `int64_policy` - Policy for handling 64-bit integers
pub fn json_value_to_robj(value: &Value, int64_policy: Int64Policy) -> Robj {
    match value {
        Value::Null => r!(NULL),
        Value::Bool(b) => Robj::from(*b),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                // Check if it fits in i32 for R integer
                if i >= i32::MIN as i64 && i <= i32::MAX as i64 {
                    Robj::from(i as i32)
                } else {
                    // Handle large integers based on policy
                    match int64_policy {
                        Int64Policy::String => Robj::from(i.to_string()),
                        Int64Policy::Double => Robj::from(i as f64),
                        Int64Policy::Bit64 => {
                            // Reinterpret i64 bits as f64 for integer64 storage
                            let as_f64 = f64::from_bits(i as u64);
                            let mut result = Robj::from(as_f64);
                            // Set the integer64 class attribute
                            result.set_class(&["integer64"]).ok();
                            result
                        }
                    }
                }
            } else if let Some(u) = n.as_u64() {
                // Check if unsigned fits in i32
                if u <= i32::MAX as u64 {
                    Robj::from(u as i32)
                } else if u <= i64::MAX as u64 {
                    // Fits in signed i64, apply policy
                    let i = u as i64;
                    match int64_policy {
                        Int64Policy::String => Robj::from(i.to_string()),
                        Int64Policy::Double => Robj::from(u as f64),
                        Int64Policy::Bit64 => {
                            let as_f64 = f64::from_bits(i as u64);
                            let mut result = Robj::from(as_f64);
                            result.set_class(&["integer64"]).ok();
                            result
                        }
                    }
                } else {
                    // Too large even for i64, always use string or double
                    match int64_policy {
                        Int64Policy::String => Robj::from(u.to_string()),
                        _ => Robj::from(u as f64),
                    }
                }
            } else if let Some(f) = n.as_f64() {
                Robj::from(f)
            } else {
                r!(NULL)
            }
        }
        Value::String(s) => Robj::from(s.as_str()),
        Value::Array(arr) => convert_array_to_robj(arr, int64_policy),
        Value::Object(obj) => {
            // Convert to named R list
            let mut names = Vec::with_capacity(obj.len());
            let mut values = Vec::with_capacity(obj.len());

            for (key, val) in obj.iter() {
                names.push(key.as_str());
                values.push(json_value_to_robj(val, int64_policy));
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
fn convert_array_to_robj(arr: &[Value], int64_policy: Int64Policy) -> Robj {
    if arr.is_empty() {
        return List::from_values(Vec::<Robj>::new()).into_robj();
    }

    // Check if all elements are the same type
    let first_type = get_json_type(&arr[0]);
    let is_homogeneous = arr.iter().all(|v| get_json_type(v) == first_type);

    if !is_homogeneous {
        // Heterogeneous array - convert to R list
        let values: Vec<Robj> = arr.iter().map(|v| json_value_to_robj(v, int64_policy)).collect();
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
            // Check if all numbers will be the same type after int64 policy is applied
            // This is important because int64="string" or int64="bit64" can make an array heterogeneous
            let has_large_int = arr.iter().any(|v| {
                if let Value::Number(n) = v {
                    if let Some(i) = n.as_i64() {
                        i < i32::MIN as i64 || i > i32::MAX as i64
                    } else {
                        false
                    }
                } else {
                    false
                }
            });

            // If we have large integers and the policy converts them to a different type,
            // we need to return a list
            if has_large_int && (int64_policy == Int64Policy::String || int64_policy == Int64Policy::Bit64) {
                let values: Vec<Robj> = arr.iter().map(|v| json_value_to_robj(v, int64_policy)).collect();
                return List::from_values(values).into_robj();
            }

            // Check if all numbers are integers (that fit in i32)
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
                // Double vector (int64="double" policy or floats)
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
            let values: Vec<Robj> = arr.iter().map(|v| json_value_to_robj(v, int64_policy)).collect();
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
