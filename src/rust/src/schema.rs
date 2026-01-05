use extendr_api::prelude::*;
use indexmap::IndexMap;
use serde_json::Value;

// Use std::result::Result explicitly to avoid conflict with extendr's Result
type StdResult<T, E> = std::result::Result<T, E>;

/// Named date format presets
fn get_date_format(name: &str) -> Option<&'static str> {
    match name {
        "iso8601" => Some("%Y-%m-%d"),
        "us_date" => Some("%m/%d/%Y"),
        "eu_date" => Some("%d/%m/%Y"),
        _ => None,
    }
}

/// Named datetime format presets
fn get_datetime_format(name: &str) -> Option<&'static str> {
    match name {
        "iso8601" => Some("%Y-%m-%dT%H:%M:%S"),
        "iso8601z" => Some("%Y-%m-%dT%H:%M:%SZ"),
        "iso8601_ms" => Some("%Y-%m-%dT%H:%M:%S%.3f"),
        "rfc822" => Some("%a, %d %b %Y %H:%M:%S"),
        "rfc3339" => Some("%Y-%m-%dT%H:%M:%S%:z"),
        "us_datetime" => Some("%m/%d/%Y %H:%M:%S"),
        "eu_datetime" => Some("%d/%m/%Y %H:%M:%S"),
        _ => None,
    }
}

/// Parse a date string using R's strptime
fn parse_date_string(date_str: &str, format: &str) -> StdResult<f64, String> {
    // Call R's as.Date function with the format
    let result = R!("as.numeric(as.Date({{date_str}}, format={{format}}, tz='UTC'))");

    match result {
        Ok(robj) => {
            if let Some(date_num) = robj.as_real() {
                if date_num.is_nan() {
                    Err(format!(
                        "Failed to parse date '{}' with format '{}'",
                        date_str, format
                    ))
                } else {
                    Ok(date_num)
                }
            } else {
                Err(format!(
                    "Failed to parse date '{}' with format '{}'",
                    date_str, format
                ))
            }
        }
        Err(e) => Err(format!("Error parsing date: {:?}", e)),
    }
}

/// Parse a datetime string using R's strptime
fn parse_datetime_string(datetime_str: &str, format: &str, tz: &str) -> StdResult<f64, String> {
    // Call R's as.POSIXct function with the format and timezone
    let result = R!("as.numeric(as.POSIXct({{datetime_str}}, format={{format}}, tz={{tz}}))");

    match result {
        Ok(robj) => {
            if let Some(datetime_num) = robj.as_real() {
                if datetime_num.is_nan() {
                    Err(format!(
                        "Failed to parse datetime '{}' with format '{}'",
                        datetime_str, format
                    ))
                } else {
                    Ok(datetime_num)
                }
            } else {
                Err(format!(
                    "Failed to parse datetime '{}' with format '{}'",
                    datetime_str, format
                ))
            }
        }
        Err(e) => Err(format!("Error parsing datetime: {:?}", e)),
    }
}

/// Wrapper struct for Schema that can be passed to/from R as an external pointer
/// This allows us to build a schema once and reuse it many times
#[derive(Debug, Clone, PartialEq)]
#[extendr]
pub struct LLMJsonSchemaBuilt {
    schema: Schema,
}

#[extendr]
impl LLMJsonSchemaBuilt {
    /// Create a new built schema from an R schema definition
    pub fn new(robj: Robj) -> Self {
        match Schema::from_robj(&robj) {
            Ok(schema) => LLMJsonSchemaBuilt { schema },
            Err(e) => {
                throw_r_error(&format!("Invalid schema: {}", e));
            }
        }
    }

    /// Format schema as string for display
    pub fn format(&self) -> String {
        format_schema_helper(&self.schema, 0)
    }
}

impl LLMJsonSchemaBuilt {
    /// Get a reference to the internal Schema
    pub fn get_schema(&self) -> &Schema {
        &self.schema
    }
}

/// Helper function to recursively format schema structure as a string in JSON-like format
fn format_schema_helper(schema: &Schema, indent: usize) -> String {
    let padding = "  ".repeat(indent);

    match schema {
        Schema::Object {
            fields,
            required: _,
        } => {
            let mut result = String::new();

            let field_names: Vec<&String> = fields.keys().collect();
            for (i, field_name) in field_names.iter().enumerate() {
                let field_schema = &fields[*field_name];
                result.push_str(&format!("{}  \"{}\": ", padding, field_name));

                match field_schema {
                    Schema::Object { .. } | Schema::Array { .. } => {
                        result.push('\n');
                        result.push_str(&format_schema_helper(field_schema, indent + 1));
                    }
                    _ => {
                        // Inline simple types
                        let mut type_str = format!("\"{}\"", get_type_name(field_schema));

                        // Add enum values if this is an enum
                        if let Schema::Enum { values, .. } = field_schema {
                            type_str.push_str(&format!(" {}", get_enum_values_string(values)));
                        }

                        if field_schema.is_required() {
                            type_str.push_str(" (required)");
                        }
                        if let Some(default_str) = get_default_string(field_schema) {
                            type_str.push_str(&format!(" [default: {}]", default_str));
                        }
                        result.push_str(&type_str);
                    }
                }

                if i < field_names.len() - 1 {
                    result.push(',');
                }
                result.push('\n');
            }

            format!("{}{{\n{}{}}}", padding, result, padding)
        }
        Schema::Array { items, required: _ } => {
            format!(
                "{}[\n{}\n{}]",
                padding,
                format_schema_helper(items, indent + 1),
                padding
            )
        }
        _ => {
            // Simple types at top level (shouldn't happen in normal usage)
            let mut type_str = format!("{}\"{}\"", padding, get_type_name(schema));

            // Add enum values if this is an enum
            if let Schema::Enum { values, .. } = schema {
                type_str.push_str(&format!(" {}", get_enum_values_string(values)));
            }

            if schema.is_required() {
                type_str.push_str(" (required)");
            }
            if let Some(default_str) = get_default_string(schema) {
                type_str.push_str(&format!(" [default: {}]", default_str));
            }
            type_str
        }
    }
}

/// Get the type name for a schema (using JSON Schema naming conventions)
fn get_type_name(schema: &Schema) -> &str {
    match schema {
        Schema::Integer { .. } => "integer",
        Schema::Number { .. } => "number",
        Schema::String { .. } => "string",
        Schema::Boolean { .. } => "boolean",
        Schema::Enum { .. } => "enum",
        Schema::Date { .. } => "date",
        Schema::Timestamp { .. } => "timestamp",
        Schema::Array { .. } => "array",
        Schema::Object { .. } => "object",
        Schema::Any { .. } => "any",
    }
}

/// Get the default value as a formatted string
fn get_default_string(schema: &Schema) -> Option<String> {
    match schema {
        Schema::Integer { default, .. } => default.map(|v| v.to_string()),
        Schema::Number { default, .. } => default.map(|v| v.to_string()),
        Schema::String { default, .. } => default.as_ref().map(|v| format!("\"{}\"", v)),
        Schema::Boolean { default, .. } => default.map(|v| v.to_string()),
        Schema::Enum { default, .. } => default.as_ref().map(|v| format!("\"{}\"", v)),
        Schema::Date { default, .. } => default.as_ref().map(|v| format!("\"{}\"", v)),
        Schema::Timestamp { default, .. } => default.map(|v| v.to_string()),
        _ => None,
    }
}

/// Get enum values as a formatted string (truncated if too long)
fn get_enum_values_string(values: &[String]) -> String {
    const MAX_VALUES: usize = 5;
    const MAX_LENGTH: usize = 60;

    if values.is_empty() {
        return "[]".to_string();
    }

    let mut result = String::from("[");
    let mut current_length = 1;
    let mut values_shown = 0;

    for (i, value) in values.iter().enumerate() {
        let value_str = format!("\"{}\"", value);
        let separator = if i > 0 { ", " } else { "" };
        let addition = format!("{}{}", separator, value_str);

        // Check if we've hit our limits
        if values_shown >= MAX_VALUES || current_length + addition.len() > MAX_LENGTH {
            let remaining = values.len() - values_shown;
            result.push_str(&format!(", ... {} more", remaining));
            break;
        }

        result.push_str(&addition);
        current_length += addition.len();
        values_shown += 1;
    }

    result.push(']');
    result
}

/// Schema definition for JSON validation and conversion (using JSON Schema naming)
#[derive(Debug, Clone, PartialEq)]
pub enum Schema {
    Integer {
        required: bool,
        default: Option<i32>,
    },
    Number {
        required: bool,
        default: Option<f64>,
    },
    String {
        required: bool,
        default: Option<String>,
    },
    Boolean {
        required: bool,
        default: Option<bool>,
    },
    Enum {
        values: Vec<String>,
        required: bool,
        default: Option<String>,
    },
    Date {
        required: bool,
        default: Option<String>,     // ISO format date string
        format: Option<Vec<String>>, // Format string(s) for parsing
    },
    Timestamp {
        required: bool,
        default: Option<f64>,        // Unix timestamp
        format: Option<Vec<String>>, // Format string(s) for parsing
        tz: String,                  // Timezone (default "UTC")
    },
    Array {
        items: Box<Schema>,
        required: bool,
    },
    Object {
        fields: IndexMap<String, Schema>,
        required: bool,
    },
    Any {
        required: bool,
    },
}

impl Schema {
    /// Parse a schema from an R object
    pub fn from_robj(robj: &Robj) -> StdResult<Self, String> {
        if robj.is_null() {
            return Err("Schema cannot be NULL".to_string());
        }

        if !robj.is_list() {
            return Err("Schema must be a list".to_string());
        }

        let list = robj.as_list().ok_or("Failed to convert to list")?;

        // Get the 'type' field
        let type_robj = list
            .iter()
            .find(|(name, _)| *name == "type")
            .ok_or("Schema must have a 'type' field")?
            .1;
        let schema_type = type_robj.as_str().ok_or("Schema 'type' must be a string")?;

        // Get the 'required' field
        let required = list
            .iter()
            .find(|(name, _)| *name == "required")
            .and_then(|(_, robj)| robj.as_bool())
            .unwrap_or(false);

        match schema_type {
            "integer" => {
                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_integer());
                Ok(Schema::Integer { required, default })
            }
            "number" => {
                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_real());
                Ok(Schema::Number { required, default })
            }
            "string" => {
                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_str())
                    .map(|s| s.to_string());
                Ok(Schema::String { required, default })
            }
            "boolean" => {
                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_bool());
                Ok(Schema::Boolean { required, default })
            }
            "enum" => {
                let values_robj = list
                    .iter()
                    .find(|(name, _)| *name == "values")
                    .ok_or("Enum schema must have 'values' field")?
                    .1;
                let values = values_robj
                    .as_str_vector()
                    .ok_or("Enum 'values' must be a character vector")?
                    .iter()
                    .map(|s| s.to_string())
                    .collect();

                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_str())
                    .map(|s| s.to_string());

                Ok(Schema::Enum {
                    values,
                    required,
                    default,
                })
            }
            "date" => {
                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_str())
                    .map(|s| s.to_string());

                let format =
                    list.iter()
                        .find(|(name, _)| *name == "format")
                        .and_then(|(_, robj)| {
                            if robj.is_null() {
                                None
                            } else {
                                robj.as_str_vector()
                                    .map(|v| v.iter().map(|s| s.to_string()).collect())
                            }
                        });

                Ok(Schema::Date {
                    required,
                    default,
                    format,
                })
            }
            "timestamp" => {
                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_real());

                let format =
                    list.iter()
                        .find(|(name, _)| *name == "format")
                        .and_then(|(_, robj)| {
                            if robj.is_null() {
                                None
                            } else {
                                robj.as_str_vector()
                                    .map(|v| v.iter().map(|s| s.to_string()).collect())
                            }
                        });

                let tz = list
                    .iter()
                    .find(|(name, _)| *name == "tz")
                    .and_then(|(_, robj)| robj.as_str())
                    .unwrap_or("UTC")
                    .to_string();

                Ok(Schema::Timestamp {
                    required,
                    default,
                    format,
                    tz,
                })
            }
            "any" => Ok(Schema::Any { required }),
            "array" => {
                let items_robj = list
                    .iter()
                    .find(|(name, _)| *name == "items")
                    .ok_or("Array schema must have 'items' field")?
                    .1;
                let items = Schema::from_robj(&items_robj)?;
                Ok(Schema::Array {
                    items: Box::new(items),
                    required,
                })
            }
            "object" => {
                let fields_robj = list
                    .iter()
                    .find(|(name, _)| *name == "fields")
                    .ok_or("Object schema must have 'fields' field")?
                    .1;
                let fields_list = fields_robj
                    .as_list()
                    .ok_or("Object 'fields' must be a list")?;

                let mut fields = IndexMap::new();
                for (name, field_schema) in fields_list.iter() {
                    let schema = Schema::from_robj(&field_schema)?;
                    fields.insert(name.to_string(), schema);
                }

                Ok(Schema::Object { fields, required })
            }
            _ => Err(format!("Unknown schema type: {}", schema_type)),
        }
    }

    /// Apply the schema to a JSON value, converting and validating
    pub fn apply(&self, value: &Value) -> StdResult<Robj, String> {
        // Handle null values
        if value.is_null() {
            return match self.is_required() {
                true => Err("Value is null but schema requires non-null".to_string()),
                false => Ok(r!(NULL)),
            };
        }

        match self {
            Schema::Integer { .. } => {
                // Try direct integer conversion
                if let Some(n) = value.as_i64() {
                    if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                        return Ok(Robj::from(n as i32));
                    } else {
                        return Err(format!("Integer value {} out of range for i32", n));
                    }
                } else if let Some(n) = value.as_u64() {
                    if n <= i32::MAX as u64 {
                        return Ok(Robj::from(n as i32));
                    } else {
                        return Err(format!("Integer value {} out of range for i32", n));
                    }
                }

                // Try coercion from other types
                match value {
                    Value::String(s) => {
                        // Try to parse string as integer
                        match s.parse::<i32>() {
                            Ok(i) => Ok(Robj::from(i)),
                            Err(_) => Err(format!("Cannot parse '{}' as integer", s)),
                        }
                    }
                    Value::Bool(b) => Ok(Robj::from(if *b { 1i32 } else { 0i32 })),
                    _ => Err(format!("Cannot convert {:?} to integer", value)),
                }
            }
            Schema::Number { .. } => {
                // Try direct numeric conversion
                if let Some(f) = value.as_f64() {
                    return Ok(Robj::from(f));
                } else if let Some(i) = value.as_i64() {
                    return Ok(Robj::from(i as f64));
                } else if let Some(u) = value.as_u64() {
                    return Ok(Robj::from(u as f64));
                }

                // Try coercion from other types
                match value {
                    Value::String(s) => {
                        // Try to parse string as double
                        match s.parse::<f64>() {
                            Ok(d) => Ok(Robj::from(d)),
                            Err(_) => Err(format!("Cannot parse '{}' as number", s)),
                        }
                    }
                    Value::Bool(b) => Ok(Robj::from(if *b { 1.0 } else { 0.0 })),
                    _ => Err(format!("Cannot convert {:?} to number", value)),
                }
            }
            Schema::String { .. } => {
                // Try direct string first
                if let Some(s) = value.as_str() {
                    Ok(Robj::from(s))
                } else {
                    // Coerce other types to string
                    let string_value = match value {
                        Value::Number(n) => n.to_string(),
                        Value::Bool(b) => b.to_string(),
                        Value::Null => "".to_string(),
                        Value::Array(_) | Value::Object(_) => {
                            // For complex types, convert to JSON string
                            serde_json::to_string(value).unwrap_or_default()
                        }
                        _ => return Err(format!("Cannot convert {:?} to string", value)),
                    };
                    Ok(Robj::from(string_value.as_str()))
                }
            }
            Schema::Boolean { .. } => {
                // Try direct boolean conversion
                if let Some(b) = value.as_bool() {
                    return Ok(Robj::from(b));
                }

                // Try coercion from other types
                match value {
                    Value::Number(n) => {
                        // 0 is false, non-zero is true
                        if let Some(i) = n.as_i64() {
                            Ok(Robj::from(i != 0))
                        } else if let Some(f) = n.as_f64() {
                            Ok(Robj::from(f != 0.0))
                        } else {
                            Ok(Robj::from(true))
                        }
                    }
                    Value::String(s) => {
                        // Parse common boolean string representations
                        let s_lower = s.to_lowercase();
                        match s_lower.as_str() {
                            "true" | "t" | "yes" | "y" | "1" => Ok(Robj::from(true)),
                            "false" | "f" | "no" | "n" | "0" | "" => Ok(Robj::from(false)),
                            _ => Err(format!("Cannot parse '{}' as boolean", s)),
                        }
                    }
                    Value::Null => Ok(Robj::from(false)),
                    _ => Err(format!("Cannot convert {:?} to boolean", value)),
                }
            }
            Schema::Enum { values, .. } => {
                // Get the string value
                let str_value = if let Some(s) = value.as_str() {
                    s.to_string()
                } else {
                    // Try to coerce to string
                    match value {
                        Value::Number(n) => n.to_string(),
                        Value::Bool(b) => b.to_string(),
                        _ => return Err(format!("Cannot convert {:?} to enum value", value)),
                    }
                };

                // Validate that the value is in the allowed list
                if values.contains(&str_value) {
                    Ok(Robj::from(str_value.as_str()))
                } else {
                    Err(format!(
                        "Value '{}' is not one of the allowed enum values: {:?}",
                        str_value, values
                    ))
                }
            }
            Schema::Date { format, .. } => {
                // Handle string dates
                if let Some(s) = value.as_str() {
                    // Get formats to try (either from schema or default)
                    let formats_to_try = if let Some(fmt_vec) = format {
                        fmt_vec.clone()
                    } else {
                        vec!["%Y-%m-%d".to_string()] // Default ISO8601 date format
                    };

                    // Try each format in order
                    let mut last_error = String::new();
                    for fmt_str in formats_to_try {
                        // Resolve named format or use as-is
                        let actual_format =
                            get_date_format(&fmt_str).unwrap_or(&fmt_str).to_string();

                        match parse_date_string(s, &actual_format) {
                            Ok(date_num) => {
                                // Convert numeric date to R Date object
                                let result = R!("structure({{date_num}}, class='Date')");
                                return result
                                    .map_err(|e| format!("Error creating Date object: {:?}", e));
                            }
                            Err(e) => {
                                last_error = e;
                                continue;
                            }
                        }
                    }
                    Err(format!("Could not parse '{}' as date: {}", s, last_error))
                } else if let Some(n) = value.as_f64() {
                    // Handle numeric date (days since epoch)
                    let result = R!("structure({{n}}, class='Date')");
                    result.map_err(|e| format!("Error creating Date object: {:?}", e))
                } else if let Some(n) = value.as_i64() {
                    // Handle integer date
                    let n_f64 = n as f64;
                    let result = R!("structure({{n_f64}}, class='Date')");
                    result.map_err(|e| format!("Error creating Date object: {:?}", e))
                } else {
                    Err(format!("Cannot convert {:?} to Date", value))
                }
            }
            Schema::Timestamp { format, tz, .. } => {
                // Handle datetime strings
                if let Some(s) = value.as_str() {
                    // Get formats to try (either from schema or default)
                    let formats_to_try = if let Some(fmt_vec) = format {
                        fmt_vec.clone()
                    } else {
                        vec!["%Y-%m-%dT%H:%M:%S".to_string()] // Default ISO8601 datetime format
                    };

                    // Try each format in order
                    let mut last_error = String::new();
                    for fmt_str in formats_to_try {
                        // Check for special timestamp formats
                        if fmt_str == "unix" || fmt_str == "epoch" {
                            // Try to parse as Unix timestamp from string
                            if let Ok(timestamp) = s.parse::<f64>() {
                                let result = R!("structure({{timestamp}}, class=c('POSIXct', 'POSIXt'), tzone={{tz}})");
                                return result.map_err(|e| {
                                    format!("Error creating POSIXct object: {:?}", e)
                                });
                            }
                        } else if fmt_str == "unix_ms" {
                            // Try to parse as millisecond timestamp from string
                            if let Ok(timestamp_ms) = s.parse::<f64>() {
                                let timestamp = timestamp_ms / 1000.0;
                                let result = R!("structure({{timestamp}}, class=c('POSIXct', 'POSIXt'), tzone={{tz}})");
                                return result.map_err(|e| {
                                    format!("Error creating POSIXct object: {:?}", e)
                                });
                            }
                        } else {
                            // Resolve named format or use as-is
                            let actual_format = get_datetime_format(&fmt_str)
                                .unwrap_or(&fmt_str)
                                .to_string();

                            match parse_datetime_string(s, &actual_format, tz) {
                                Ok(datetime_num) => {
                                    let result = R!("structure({{datetime_num}}, class=c('POSIXct', 'POSIXt'), tzone={{tz}})");
                                    return result.map_err(|e| {
                                        format!("Error creating POSIXct object: {:?}", e)
                                    });
                                }
                                Err(e) => {
                                    last_error = e;
                                    continue;
                                }
                            }
                        }
                    }
                    Err(format!(
                        "Could not parse '{}' as datetime: {}",
                        s, last_error
                    ))
                } else if let Some(n) = value.as_f64() {
                    // Handle numeric timestamp (assume Unix epoch seconds)
                    let result = R!("structure({{n}}, class=c('POSIXct', 'POSIXt'), tzone={{tz}})");
                    result.map_err(|e| format!("Error creating POSIXct object: {:?}", e))
                } else if let Some(n) = value.as_i64() {
                    // Handle integer timestamp
                    let n_f64 = n as f64;
                    let result =
                        R!("structure({{n_f64}}, class=c('POSIXct', 'POSIXt'), tzone={{tz}})");
                    result.map_err(|e| format!("Error creating POSIXct object: {:?}", e))
                } else {
                    Err(format!("Cannot convert {:?} to POSIXct", value))
                }
            }
            Schema::Array { items, .. } => {
                if let Some(arr) = value.as_array() {
                    self.apply_to_array(arr, items)
                } else {
                    Err(format!("Expected array, got {:?}", value))
                }
            }
            Schema::Object { fields, .. } => {
                if let Some(obj) = value.as_object() {
                    self.apply_to_object(obj, fields)
                } else {
                    Err(format!("Expected object, got {:?}", value))
                }
            }
            Schema::Any { .. } => {
                // For 'any', use the generic JSON to R conversion with default policy
                // When using schema, we respect the schema types, so use Double policy
                Ok(crate::json_to_r::json_value_to_robj(value, crate::llm_json::Int64Policy::Double))
            }
        }
    }

    /// Apply schema to an array
    fn apply_to_array(&self, arr: &[Value], item_schema: &Schema) -> StdResult<Robj, String> {
        // Check if all elements are the same primitive type for vectorization
        if arr.is_empty() {
            return Ok(List::from_values(Vec::<Robj>::new()).into_robj());
        }

        let mut results = Vec::with_capacity(arr.len());
        for (i, item) in arr.iter().enumerate() {
            match item_schema.apply(item) {
                Ok(robj) => results.push(robj),
                Err(e) => return Err(format!("Error at array index {}: {}", i, e)),
            }
        }

        // Try to create a homogeneous vector if possible
        match item_schema {
            Schema::Integer { .. } => {
                // Try to create an integer vector
                let mut integers = Vec::with_capacity(results.len());
                let mut all_integers = true;
                for r in &results {
                    if let Some(i) = r.as_integer() {
                        integers.push(i);
                    } else {
                        all_integers = false;
                        break;
                    }
                }

                if all_integers {
                    Ok(Robj::from(integers))
                } else {
                    Ok(List::from_values(results).into_robj())
                }
            }
            Schema::Number { .. } => {
                // Try to create a double vector
                let mut doubles = Vec::with_capacity(results.len());
                let mut all_doubles = true;
                for r in &results {
                    if let Some(d) = r.as_real() {
                        doubles.push(d);
                    } else {
                        all_doubles = false;
                        break;
                    }
                }

                if all_doubles {
                    Ok(Robj::from(doubles))
                } else {
                    Ok(List::from_values(results).into_robj())
                }
            }
            Schema::String { .. } => {
                // Try to create a character vector
                let mut strings = Vec::with_capacity(results.len());
                let mut all_strings = true;
                for r in &results {
                    if let Some(s) = r.as_str() {
                        strings.push(s.to_string());
                    } else {
                        all_strings = false;
                        break;
                    }
                }

                if all_strings {
                    let str_refs: Vec<&str> = strings.iter().map(|s| s.as_str()).collect();
                    Ok(Robj::from(str_refs))
                } else {
                    Ok(List::from_values(results).into_robj())
                }
            }
            Schema::Boolean { .. } => {
                // Try to create a logical vector
                let mut logicals = Vec::with_capacity(results.len());
                let mut all_logicals = true;
                for r in &results {
                    if let Some(b) = r.as_bool() {
                        logicals.push(b);
                    } else {
                        all_logicals = false;
                        break;
                    }
                }

                if all_logicals {
                    Ok(Robj::from(logicals))
                } else {
                    Ok(List::from_values(results).into_robj())
                }
            }
            _ => {
                // For complex types, return as list
                Ok(List::from_values(results).into_robj())
            }
        }
    }

    /// Apply schema to an object
    fn apply_to_object(
        &self,
        obj: &serde_json::Map<String, Value>,
        fields: &IndexMap<String, Schema>,
    ) -> StdResult<Robj, String> {
        let mut names = Vec::with_capacity(fields.len());
        let mut values = Vec::with_capacity(fields.len());

        for (field_name, field_schema) in fields.iter() {
            let value = obj.get(field_name);

            match value {
                Some(v) => {
                    let converted = field_schema.apply(v)?;
                    names.push(field_name.as_str());
                    values.push(converted);
                }
                None => {
                    // Field is missing from input JSON
                    if field_schema.is_required() {
                        // Required field - add default value (or NULL if no default)
                        names.push(field_name.as_str());
                        let default_robj = field_schema.get_default();
                        if let Some(default) = default_robj {
                            values.push(default);
                        } else {
                            values.push(r!(NULL));
                        }
                    } else {
                        // Optional field and not in input - omit it entirely from output
                        // (don't add to names/values)
                    }
                }
            }
        }

        let mut list = List::from_values(values);
        list.set_names(names).unwrap();
        Ok(list.into_robj())
    }

    /// Check if this schema is required (does not allow null values)
    fn is_required(&self) -> bool {
        match self {
            Schema::Integer { required, .. }
            | Schema::Number { required, .. }
            | Schema::String { required, .. }
            | Schema::Boolean { required, .. }
            | Schema::Enum { required, .. }
            | Schema::Date { required, .. }
            | Schema::Timestamp { required, .. }
            | Schema::Array { required, .. }
            | Schema::Object { required, .. }
            | Schema::Any { required } => *required,
        }
    }

    /// Get the default value for this schema if one is defined
    fn get_default(&self) -> Option<Robj> {
        match self {
            Schema::Integer { default, .. } => default.map(|v| Robj::from(v)),
            Schema::Number { default, .. } => default.map(|v| Robj::from(v)),
            Schema::String { default, .. } => default.as_ref().map(|v| Robj::from(v.as_str())),
            Schema::Boolean { default, .. } => default.map(|v| Robj::from(v)),
            Schema::Enum { default, .. } => default.as_ref().map(|v| Robj::from(v.as_str())),
            Schema::Date { default, .. } => {
                default.as_ref().and_then(|date_str| {
                    // Parse date string and create R Date object
                    let date_num = parse_date_string(date_str, "%Y-%m-%d").ok()?;
                    R!("structure({{date_num}}, class='Date')").ok()
                })
            }
            Schema::Timestamp { default, tz, .. } => {
                default.and_then(|timestamp| {
                    // Create R POSIXct object from timestamp
                    R!("structure({{timestamp}}, class=c('POSIXct', 'POSIXt'), tzone={{tz}})").ok()
                })
            }
            _ => None,
        }
    }

    /// Apply schema to a JSON value, filling in defaults for missing fields
    /// Returns a modified JSON value with defaults applied
    pub fn apply_defaults(&self, value: &Value) -> StdResult<Value, String> {
        match self {
            Schema::Object { fields, .. } => {
                if let Some(obj) = value.as_object() {
                    let mut new_obj = serde_json::Map::new();

                    // Copy existing fields and add missing ones with defaults
                    for (field_name, field_schema) in fields.iter() {
                        if let Some(v) = obj.get(field_name) {
                            // Field exists, recursively apply defaults if needed
                            let processed = field_schema.apply_defaults(v)?;
                            new_obj.insert(field_name.clone(), processed);
                        } else {
                            // Field is missing from input JSON
                            if field_schema.is_required() {
                                // Required field - add default value (or null if no default)
                                let default_value = match field_schema.get_default_json() {
                                    Some(v) => v,
                                    None => Value::Null,
                                };
                                new_obj.insert(field_name.clone(), default_value);
                            } else {
                                // Optional field and not in input - omit it entirely from output
                                // (don't add to new_obj)
                            }
                        }
                    }

                    Ok(Value::Object(new_obj))
                } else {
                    Err("Expected object for map schema".to_string())
                }
            }
            Schema::Array { items, .. } => {
                if let Some(arr) = value.as_array() {
                    let new_arr: StdResult<Vec<Value>, String> =
                        arr.iter().map(|item| items.apply_defaults(item)).collect();
                    Ok(Value::Array(new_arr?))
                } else {
                    Err("Expected array".to_string())
                }
            }
            Schema::Integer { .. } => {
                // Coerce to integer
                self.coerce_to_integer(value)
            }
            Schema::Number { .. } => {
                // Coerce to double
                self.coerce_to_double(value)
            }
            Schema::String { .. } => {
                // Coerce to string
                self.coerce_to_string(value)
            }
            Schema::Boolean { .. } => {
                // Coerce to boolean
                self.coerce_to_boolean(value)
            }
            Schema::Enum { .. } => {
                // Coerce to enum (validate)
                self.coerce_to_enum(value)
            }
            Schema::Date { .. } | Schema::Timestamp { .. } => {
                // For date types in apply_defaults, just return the value as-is
                // The actual conversion happens in apply() method
                Ok(value.clone())
            }
            Schema::Any { .. } => {
                // For 'any', return as-is
                Ok(value.clone())
            }
        }
    }

    /// Coerce a JSON value to integer
    fn coerce_to_integer(&self, value: &Value) -> StdResult<Value, String> {
        // Try direct integer conversion
        if let Some(n) = value.as_i64() {
            if n >= i32::MIN as i64 && n <= i32::MAX as i64 {
                return Ok(Value::Number((n as i32).into()));
            } else {
                return Err(format!("Integer value {} out of range for i32", n));
            }
        } else if let Some(n) = value.as_u64() {
            if n <= i32::MAX as u64 {
                return Ok(Value::Number((n as i32).into()));
            } else {
                return Err(format!("Integer value {} out of range for i32", n));
            }
        }

        // Try coercion from other types
        match value {
            Value::String(s) => match s.parse::<i32>() {
                Ok(i) => Ok(Value::Number(i.into())),
                Err(_) => Err(format!("Cannot parse '{}' as integer", s)),
            },
            Value::Bool(b) => Ok(Value::Number(if *b { 1 } else { 0 }.into())),
            _ => Err(format!("Cannot convert {:?} to integer", value)),
        }
    }

    /// Coerce a JSON value to double
    fn coerce_to_double(&self, value: &Value) -> StdResult<Value, String> {
        // Try direct numeric conversion
        if let Some(f) = value.as_f64() {
            return serde_json::Number::from_f64(f)
                .map(Value::Number)
                .ok_or_else(|| "Invalid floating point number".to_string());
        } else if let Some(i) = value.as_i64() {
            return serde_json::Number::from_f64(i as f64)
                .map(Value::Number)
                .ok_or_else(|| "Invalid floating point number".to_string());
        } else if let Some(u) = value.as_u64() {
            return serde_json::Number::from_f64(u as f64)
                .map(Value::Number)
                .ok_or_else(|| "Invalid floating point number".to_string());
        }

        // Try coercion from other types
        match value {
            Value::String(s) => match s.parse::<f64>() {
                Ok(d) => serde_json::Number::from_f64(d)
                    .map(Value::Number)
                    .ok_or_else(|| format!("Cannot parse '{}' as number", s)),
                Err(_) => Err(format!("Cannot parse '{}' as number", s)),
            },
            Value::Bool(b) => serde_json::Number::from_f64(if *b { 1.0 } else { 0.0 })
                .map(Value::Number)
                .ok_or_else(|| "Invalid floating point number".to_string()),
            _ => Err(format!("Cannot convert {:?} to number", value)),
        }
    }

    /// Coerce a JSON value to string
    fn coerce_to_string(&self, value: &Value) -> StdResult<Value, String> {
        if let Some(s) = value.as_str() {
            Ok(Value::String(s.to_string()))
        } else {
            let string_value = match value {
                Value::Number(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Null => "".to_string(),
                Value::Array(_) | Value::Object(_) => {
                    serde_json::to_string(value).unwrap_or_default()
                }
                _ => return Err(format!("Cannot convert {:?} to string", value)),
            };
            Ok(Value::String(string_value))
        }
    }

    /// Coerce a JSON value to boolean
    fn coerce_to_boolean(&self, value: &Value) -> StdResult<Value, String> {
        if let Some(b) = value.as_bool() {
            return Ok(Value::Bool(b));
        }

        match value {
            Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Ok(Value::Bool(i != 0))
                } else if let Some(f) = n.as_f64() {
                    Ok(Value::Bool(f != 0.0))
                } else {
                    Ok(Value::Bool(true))
                }
            }
            Value::String(s) => {
                let s_lower = s.to_lowercase();
                match s_lower.as_str() {
                    "true" | "t" | "yes" | "y" | "1" => Ok(Value::Bool(true)),
                    "false" | "f" | "no" | "n" | "0" | "" => Ok(Value::Bool(false)),
                    _ => Err(format!("Cannot parse '{}' as boolean", s)),
                }
            }
            Value::Null => Ok(Value::Bool(false)),
            _ => Err(format!("Cannot convert {:?} to boolean", value)),
        }
    }

    /// Coerce a JSON value to enum (validate against allowed values)
    fn coerce_to_enum(&self, value: &Value) -> StdResult<Value, String> {
        if let Schema::Enum { values, .. } = self {
            // Get the string value
            let str_value = if let Some(s) = value.as_str() {
                s.to_string()
            } else {
                // Try to coerce to string
                match value {
                    Value::Number(n) => n.to_string(),
                    Value::Bool(b) => b.to_string(),
                    _ => return Err(format!("Cannot convert {:?} to enum value", value)),
                }
            };

            // Validate that the value is in the allowed list
            if values.contains(&str_value) {
                Ok(Value::String(str_value))
            } else {
                Err(format!(
                    "Value '{}' is not one of the allowed enum values: {:?}",
                    str_value, values
                ))
            }
        } else {
            Err("coerce_to_enum called on non-enum schema".to_string())
        }
    }

    /// Get the default value as a JSON Value
    fn get_default_json(&self) -> Option<Value> {
        match self {
            Schema::Integer { default, .. } => default.map(|v| Value::Number(v.into())),
            Schema::Number { default, .. } => {
                default.and_then(|v| serde_json::Number::from_f64(v).map(Value::Number))
            }
            Schema::String { default, .. } => default.as_ref().map(|v| Value::String(v.clone())),
            Schema::Boolean { default, .. } => default.map(|v| Value::Bool(v)),
            Schema::Enum { default, .. } => default.as_ref().map(|v| Value::String(v.clone())),
            Schema::Date { default, .. } => default.as_ref().map(|v| Value::String(v.clone())),
            Schema::Timestamp { default, .. } => {
                default.and_then(|v| serde_json::Number::from_f64(v).map(Value::Number))
            }
            _ => None,
        }
    }
}

// Generate metadata for the LLMJsonSchemaBuilt type
extendr_module! {
    mod schema;
    impl LLMJsonSchemaBuilt;
}
