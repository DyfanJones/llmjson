use extendr_api::prelude::*;
use serde_json::Value;
use indexmap::IndexMap;

// Use std::result::Result explicitly to avoid conflict with extendr's Result
type StdResult<T, E> = std::result::Result<T, E>;

/// Wrapper struct for Schema that can be passed to/from R as an external pointer
/// This allows us to build a schema once and reuse it many times
#[derive(Debug, Clone, PartialEq)]
#[extendr]
pub struct BuiltSchema {
    schema: Schema,
}

#[extendr]
impl BuiltSchema {
    /// Create a new built schema from an R schema definition
    pub fn new(robj: Robj) -> Self {
        match Schema::from_robj(&robj) {
            Ok(schema) => BuiltSchema { schema },
            Err(e) => {
                throw_r_error(&format!("Invalid schema: {}", e));
            }
        }
    }

    /// Print method for R console
    pub fn print(&self) {
        print_schema_helper(&self.schema, 0);
        rprintln!("");
    }

    /// Format schema as string (for benchmarking)
    pub fn format(&self) -> String {
        format_schema_helper(&self.schema, 0)
    }
}

impl BuiltSchema {
    /// Get a reference to the internal Schema
    pub fn get_schema(&self) -> &Schema {
        &self.schema
    }
}

/// Helper function to recursively print schema structure in JSON-like format
fn print_schema_helper(schema: &Schema, indent: usize) {
    let result = format_schema_helper(schema, indent);
    rprint!("{}", result);
}

/// Format schema as string (used for both printing and benchmarking)
fn format_schema_helper(schema: &Schema, indent: usize) -> String {
    let padding = "  ".repeat(indent);

    match schema {
        Schema::Map { fields, optional: _ } => {
            let mut result = String::new();

            let field_names: Vec<&String> = fields.keys().collect();
            for (i, field_name) in field_names.iter().enumerate() {
                let field_schema = &fields[*field_name];
                result.push_str(&format!("{}  \"{}\": ", padding, field_name));

                match field_schema {
                    Schema::Map { .. } | Schema::Array { .. } => {
                        result.push('\n');
                        result.push_str(&format_schema_helper(field_schema, indent + 1));
                    }
                    _ => {
                        // Inline simple types
                        let mut type_str = format!("\"{}\"", get_type_name(field_schema));
                        if field_schema.is_optional() {
                            type_str.push_str(" (optional)");
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
        Schema::Array { items, optional: _ } => {
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
            if schema.is_optional() {
                type_str.push_str(" (optional)");
            }
            if let Some(default_str) = get_default_string(schema) {
                type_str.push_str(&format!(" [default: {}]", default_str));
            }
            type_str
        }
    }
}

/// Get the type name for a schema
fn get_type_name(schema: &Schema) -> &str {
    match schema {
        Schema::Integer { .. } => "integer",
        Schema::Double { .. } => "double",
        Schema::String { .. } => "string",
        Schema::Logical { .. } => "logical",
        Schema::Array { .. } => "array",
        Schema::Map { .. } => "map",
        Schema::Any { .. } => "any",
    }
}

/// Get the default value as a formatted string
fn get_default_string(schema: &Schema) -> Option<String> {
    match schema {
        Schema::Integer { default, .. } => default.map(|v| v.to_string()),
        Schema::Double { default, .. } => default.map(|v| v.to_string()),
        Schema::String { default, .. } => default.as_ref().map(|v| format!("\"{}\"", v)),
        Schema::Logical { default, .. } => default.map(|v| v.to_string()),
        _ => None,
    }
}

/// Schema definition for JSON validation and conversion
#[derive(Debug, Clone, PartialEq)]
pub enum Schema {
    Integer {
        optional: bool,
        default: Option<i32>,
    },
    Double {
        optional: bool,
        default: Option<f64>,
    },
    String {
        optional: bool,
        default: Option<String>,
    },
    Logical {
        optional: bool,
        default: Option<bool>,
    },
    Array {
        items: Box<Schema>,
        optional: bool,
    },
    Map {
        fields: IndexMap<String, Schema>,
        optional: bool,
    },
    Any {
        optional: bool,
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

        // Get the 'optional' field
        let optional = list
            .iter()
            .find(|(name, _)| *name == "optional")
            .and_then(|(_, robj)| robj.as_bool())
            .unwrap_or(false);

        match schema_type {
            "integer" => {
                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_integer());
                Ok(Schema::Integer { optional, default })
            }
            "double" => {
                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_real());
                Ok(Schema::Double { optional, default })
            }
            "string" => {
                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_str())
                    .map(|s| s.to_string());
                Ok(Schema::String { optional, default })
            }
            "logical" => {
                let default = list
                    .iter()
                    .find(|(name, _)| *name == "default")
                    .and_then(|(_, robj)| robj.as_bool());
                Ok(Schema::Logical { optional, default })
            }
            "any" => Ok(Schema::Any { optional }),
            "array" => {
                let items_robj = list
                    .iter()
                    .find(|(name, _)| *name == "items")
                    .ok_or("Array schema must have 'items' field")?
                    .1;
                let items = Schema::from_robj(&items_robj)?;
                Ok(Schema::Array {
                    items: Box::new(items),
                    optional,
                })
            }
            "map" => {
                let fields_robj = list
                    .iter()
                    .find(|(name, _)| *name == "fields")
                    .ok_or("Map schema must have 'fields' field")?
                    .1;
                let fields_list = fields_robj.as_list().ok_or("Map 'fields' must be a list")?;

                let mut fields = IndexMap::new();
                for (name, field_schema) in fields_list.iter() {
                    let schema = Schema::from_robj(&field_schema)?;
                    fields.insert(name.to_string(), schema);
                }

                Ok(Schema::Map { fields, optional })
            }
            _ => Err(format!("Unknown schema type: {}", schema_type)),
        }
    }

    /// Apply the schema to a JSON value, converting and validating
    pub fn apply(&self, value: &Value) -> StdResult<Robj, String> {
        // Handle null values
        if value.is_null() {
            return match self.is_optional() {
                true => Ok(r!(NULL)),
                false => Err("Value is null but schema requires non-null".to_string()),
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
            Schema::Double { .. } => {
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
            Schema::Logical { .. } => {
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
            Schema::Array { items, .. } => {
                if let Some(arr) = value.as_array() {
                    self.apply_to_array(arr, items)
                } else {
                    Err(format!("Expected array, got {:?}", value))
                }
            }
            Schema::Map { fields, .. } => {
                if let Some(obj) = value.as_object() {
                    self.apply_to_object(obj, fields)
                } else {
                    Err(format!("Expected object, got {:?}", value))
                }
            }
            Schema::Any { .. } => {
                // For 'any', use the generic JSON to R conversion
                Ok(crate::json_to_r::json_value_to_robj(value))
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
            Schema::Double { .. } => {
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
            Schema::Logical { .. } => {
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
                    if field_schema.is_optional() {
                        // Optional field and not in input - omit it entirely from output
                        // (don't add to names/values)
                    } else {
                        // Required field - add default value (or NULL if no default)
                        names.push(field_name.as_str());
                        let default_robj = field_schema.get_default();
                        if let Some(default) = default_robj {
                            values.push(default);
                        } else {
                            values.push(r!(NULL));
                        }
                    }
                }
            }
        }

        let mut list = List::from_values(values);
        list.set_names(names).unwrap();
        Ok(list.into_robj())
    }

    /// Check if this schema allows null values
    fn is_optional(&self) -> bool {
        match self {
            Schema::Integer { optional, .. }
            | Schema::Double { optional, .. }
            | Schema::String { optional, .. }
            | Schema::Logical { optional, .. }
            | Schema::Array { optional, .. }
            | Schema::Map { optional, .. }
            | Schema::Any { optional } => *optional,
        }
    }

    /// Get the default value for this schema if one is defined
    fn get_default(&self) -> Option<Robj> {
        match self {
            Schema::Integer { default, .. } => default.map(|v| Robj::from(v)),
            Schema::Double { default, .. } => default.map(|v| Robj::from(v)),
            Schema::String { default, .. } => default.as_ref().map(|v| Robj::from(v.as_str())),
            Schema::Logical { default, .. } => default.map(|v| Robj::from(v)),
            _ => None,
        }
    }

    /// Apply schema to a JSON value, filling in defaults for missing fields
    /// Returns a modified JSON value with defaults applied
    pub fn apply_defaults(&self, value: &Value) -> StdResult<Value, String> {
        match self {
            Schema::Map { fields, .. } => {
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
                            if field_schema.is_optional() {
                                // Optional field and not in input - omit it entirely from output
                                // (don't add to new_obj)
                            } else {
                                // Required field - add default value (or null if no default)
                                let default_value = match field_schema.get_default_json() {
                                    Some(v) => v,
                                    None => Value::Null,
                                };
                                new_obj.insert(field_name.clone(), default_value);
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
            Schema::Double { .. } => {
                // Coerce to double
                self.coerce_to_double(value)
            }
            Schema::String { .. } => {
                // Coerce to string
                self.coerce_to_string(value)
            }
            Schema::Logical { .. } => {
                // Coerce to boolean
                self.coerce_to_boolean(value)
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
            Value::String(s) => {
                match s.parse::<i32>() {
                    Ok(i) => Ok(Value::Number(i.into())),
                    Err(_) => Err(format!("Cannot parse '{}' as integer", s)),
                }
            }
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
            Value::String(s) => {
                match s.parse::<f64>() {
                    Ok(d) => serde_json::Number::from_f64(d)
                        .map(Value::Number)
                        .ok_or_else(|| format!("Cannot parse '{}' as number", s)),
                    Err(_) => Err(format!("Cannot parse '{}' as number", s)),
                }
            }
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

    /// Get the default value as a JSON Value
    fn get_default_json(&self) -> Option<Value> {
        match self {
            Schema::Integer { default, .. } => default.map(|v| Value::Number(v.into())),
            Schema::Double { default, .. } => {
                default.and_then(|v| serde_json::Number::from_f64(v).map(Value::Number))
            }
            Schema::String { default, .. } => default.as_ref().map(|v| Value::String(v.clone())),
            Schema::Logical { default, .. } => default.map(|v| Value::Bool(v)),
            _ => None,
        }
    }
}

// Generate metadata for the BuiltSchema type
extendr_module! {
    mod schema;
    impl BuiltSchema;
}
