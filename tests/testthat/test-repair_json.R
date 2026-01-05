# Tests for repair_json
test_that("repair_json handles trailing commas", {
  result <- repair_json_str('{"key": "value",}')
  expect_type(result, "character")
  expect_true(grepl('"key"', result))
  expect_true(grepl('"value"', result))
  expect_false(grepl(',}', result))
})

test_that("repair_json handles unquoted keys", {
  result <- repair_json_str('{key: "value"}')
  expect_type(result, "character")
  expect_true(grepl('"key"', result))
  expect_true(grepl('"value"', result))
})

test_that("repair_json handles single quotes", {
  result <- repair_json_str("{'name': 'John'}")
  expect_type(result, "character")
  expect_true(grepl('"name"', result))
  expect_true(grepl('"John"', result))
})

test_that("repair_json handles incomplete JSON", {
  result <- repair_json_str('{"name": "John", "age": 30')
  expect_type(result, "character")
  expect_true(grepl('"name"', result))
  expect_true(grepl('"age"', result))
})

test_that("repair_json handles valid JSON", {
  valid_json <- '{"key": "value"}'
  result <- repair_json_str(valid_json)
  expect_type(result, "character")
  expect_true(grepl('"key"', result))
  expect_true(grepl('"value"', result))
})

test_that("repair_json handles arrays with trailing commas", {
  result <- repair_json_str('[1, 2, 3,]')
  expect_type(result, "character")
  expect_true(grepl('1', result))
  expect_true(grepl('2', result))
  expect_true(grepl('3', result))
})

test_that("repair_json handles nested objects", {
  result <- repair_json_str('{a: {b: "c",},}')
  expect_type(result, "character")
  expect_true(grepl('"a"', result))
  expect_true(grepl('"b"', result))
  expect_true(grepl('"c"', result))
})

test_that("repair_json handles empty input", {
  expect_equal(repair_json_str(''), "{}")
})

# Tests for repair_json_file
test_that("repair_json_file reads and repairs JSON from file", {
  # Create a temporary file with malformed JSON
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"key": "value",}', tmp_file)

  result <- repair_json_file(tmp_file)
  expect_type(result, "character")
  expect_true(grepl('"key"', result))
  expect_true(grepl('"value"', result))
  expect_false(grepl(',}', result))

  # Clean up
  unlink(tmp_file)
})

test_that("repair_json_file handles unquoted keys in file", {
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{name: "Alice", age: 30,}', tmp_file)

  result <- repair_json_file(tmp_file)
  expect_type(result, "character")
  expect_true(grepl('"name"', result))
  expect_true(grepl('"Alice"', result))
  expect_true(grepl('"age"', result))

  unlink(tmp_file)
})

test_that("repair_json_file handles non-existent file", {
  expect_error(
    repair_json_file("nonexistent_file.json"),
    "File not found"
  )
})

test_that("repair_json_file handles complex nested JSON", {
  tmp_file <- tempfile(fileext = ".json")
  complex_json <- '{
    users: [
      {name: "Alice", age: 30,},
      {name: "Bob", age: 25,}
    ],
  }'
  writeLines(complex_json, tmp_file)

  result <- repair_json_file(tmp_file)
  expect_type(result, "character")
  expect_true(grepl('"users"', result))
  expect_true(grepl('"Alice"', result))
  expect_true(grepl('"Bob"', result))

  unlink(tmp_file)
})

# Tests for repair_json_raw
test_that("repair_json_raw repairs JSON from raw bytes", {
  raw_data <- charToRaw('{"key": "value",}')

  result <- repair_json_raw(raw_data)
  expect_type(result, "character")
  expect_true(grepl('"key"', result))
  expect_true(grepl('"value"', result))
  expect_false(grepl(',}', result))
})

test_that("repair_json_raw handles unquoted keys in raw bytes", {
  raw_data <- charToRaw('{name: "Alice"}')

  result <- repair_json_raw(raw_data)
  expect_type(result, "character")
  expect_true(grepl('"name"', result))
  expect_true(grepl('"Alice"', result))
})

test_that("repair_json_raw handles single quotes in raw bytes", {
  raw_data <- charToRaw("{'key': 'value'}")

  result <- repair_json_raw(raw_data)
  expect_type(result, "character")
  expect_true(grepl('"key"', result))
  expect_true(grepl('"value"', result))
})

test_that("repair_json_raw handles arrays with trailing commas", {
  raw_data <- charToRaw('[1, 2, 3,]')

  result <- repair_json_raw(raw_data)
  expect_type(result, "character")
  expect_true(grepl('1', result))
  expect_true(grepl('2', result))
  expect_true(grepl('3', result))
})

test_that("repair_json_raw handles empty raw bytes", {
  raw_data <- raw(0)
  expect_error(
    repair_json_raw(raw_data),
    "Empty raw vector provided"
  )
})

# Tests for return_objects parameter
test_that("repair_json_str returns R objects when return_objects = TRUE", {
  result <- repair_json_str(
    '{"name": "Alice", "age": 30}',
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$age, 30)
})

test_that("repair_json_str returns JSON string when return_objects = FALSE", {
  result <- repair_json_str(
    '{"name": "Alice", "age": 30}',
    return_objects = FALSE
  )
  expect_type(result, "character")
  expect_true(grepl('"name"', result))
  expect_true(grepl('"Alice"', result))
})

test_that("repair_json_file returns R objects when return_objects = TRUE", {
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"name": "Bob", "score": 95}', tmp_file)

  result <- repair_json_file(tmp_file, return_objects = TRUE)
  expect_type(result, "list")
  expect_equal(result$name, "Bob")
  expect_equal(result$score, 95)

  unlink(tmp_file)
})

test_that("repair_json_raw returns R objects when return_objects = TRUE", {
  raw_data <- charToRaw('{"city": "NYC", "population": 8000000}')

  result <- repair_json_raw(raw_data, return_objects = TRUE)
  expect_type(result, "list")
  expect_equal(result$city, "NYC")
  expect_equal(result$population, 8000000)
})

# Tests for schema functionality
test_that("json_object creates a valid schema", {
  schema <- json_object(name = json_string(), age = json_integer())
  expect_s3_class(schema, "LLMJsonSchema")
  expect_equal(schema$type, "object")
  expect_length(schema$fields, 2)
})

test_that("json_integer, json_number, json_string, json_boolean create valid schemas", {
  expect_s3_class(json_integer(), "LLMJsonSchema")
  expect_s3_class(json_number(), "LLMJsonSchema")
  expect_s3_class(json_string(), "LLMJsonSchema")
  expect_s3_class(json_boolean(), "LLMJsonSchema")
})

test_that("schema with return_objects validates and converts types", {
  schema <- json_object(
    name = json_string(),
    age = json_integer()
  )

  result <- repair_json_str(
    '{"name": "Alice", "age": 30}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$age, 30L)
})

test_that("schema omits missing optional fields even with defaults (return_objects = TRUE)", {
  schema <- json_object(
    name = json_string(),
    age = json_integer(.default = 0L, .required = FALSE)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_false("age" %in% names(result))
})

test_that("schema omits missing optional fields even with defaults (return_objects = FALSE)", {
  schema <- json_object(
    name = json_string(),
    age = json_integer(.default = 0L, .required = FALSE)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = FALSE
  )
  expect_type(result, "character")
  expect_false(grepl('"age"', result))
  expect_true(grepl('"name"', result))
})

test_that("schema omits optional fields without defaults when missing", {
  schema <- json_object(
    name = json_string(),
    age = json_integer(.required = FALSE)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_false("age" %in% names(result))
})

test_that("schema adds null for missing required fields without defaults", {
  schema <- json_object(
    name = json_string(.required = TRUE),
    age = json_integer(.required = TRUE)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(
    result,
    list(
      name = "Alice",
      age = 0
    )
  )

  result2 <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = FALSE
  )
  expect_equal(result2, "{\"age\":0,\"name\":\"Alice\"}")
})

test_that("schema adds default for missing required fields with defaults", {
  schema <- json_object(
    name = json_string(.required = TRUE),
    age = json_integer(.default = 25L, .required = TRUE)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$age, 25L)

  result2 <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = FALSE
  )
  expect_type(result2, "character")
  expect_true(grepl('"name"', result2))
  expect_true(grepl('"age"', result2))
  expect_true(grepl('25', result2))
})

test_that("schema coerces number to string", {
  schema <- json_object(
    name = json_string(),
    age = json_string()
  )

  result <- repair_json_str(
    '{"name": "Alice", "age": 30}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$age, "30")
})

test_that("schema coerces string to integer", {
  schema <- json_object(
    name = json_string(),
    age = json_integer()
  )

  result <- repair_json_str(
    '{"name": "Alice", "age": "30"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$age, 30L)
})

test_that("schema coerces boolean to integer", {
  schema <- json_object(active = json_integer())

  result <- repair_json_str(
    '{"active": true}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result$active, 1L)

  result2 <- repair_json_str(
    '{"active": false}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result2$active, 0L)
})

test_that("schema coerces types with return_objects = FALSE (JSON output)", {
  # Integer to String
  schema <- json_object(var1 = json_string())
  result <- repair_json_str(
    '{var1: 2}',
    schema = schema,
    return_objects = FALSE
  )
  expect_equal(result, "{\"var1\":\"2\"}")

  # String to Integer
  schema <- json_object(var1 = json_integer())
  result <- repair_json_str(
    '{var1: "42"}',
    schema = schema,
    return_objects = FALSE
  )
  expect_equal(result, "{\"var1\":42}")

  # Boolean to Integer
  schema <- json_object(active = json_integer())
  result <- repair_json_str(
    '{active: true}',
    schema = schema,
    return_objects = FALSE
  )
  expect_equal(result, "{\"active\":1}")

  # Integer to Double
  schema <- json_object(value = json_number())
  result <- repair_json_str(
    '{value: 10}',
    schema = schema,
    return_objects = FALSE
  )
  expect_equal(result, "{\"value\":10.0}")

  # String to Boolean
  schema <- json_object(flag = json_boolean())
  result <- repair_json_str(
    '{flag: "true"}',
    schema = schema,
    return_objects = FALSE
  )
  expect_equal(result, "{\"flag\":true}")

  # Number to String (multiple fields)
  schema <- json_object(
    name = json_string(),
    age = json_string()
  )
  result <- repair_json_str(
    '{"name": "Alice", "age": 30}',
    schema = schema,
    return_objects = FALSE
  )
  expect_equal(result, "{\"age\":\"30\",\"name\":\"Alice\"}")
})

test_that("schema coerces types in nested structures with return_objects = FALSE", {
  # Nested map with coercion
  schema <- json_object(
    name = json_string(),
    details = json_object(
      age = json_string(), # Coerce number to string
      score = json_integer() # Coerce string to integer
    )
  )
  result <- repair_json_str(
    '{"name": "Alice", "details": {"age": 30, "score": "95"}}',
    schema = schema,
    return_objects = FALSE
  )
  expect_equal(
    result,
    "{\"details\":{\"age\":\"30\",\"score\":95},\"name\":\"Alice\"}"
  )

  # Array with coercion
  schema <- json_object(
    values = json_array(json_string()) # Coerce numbers to strings
  )
  result <- repair_json_str(
    '{"values": [1, 2, 3]}',
    schema = schema,
    return_objects = FALSE
  )
  expect_equal(result, "{\"values\":[\"1\",\"2\",\"3\"]}")
})

test_that("schema coercion works with built schemas", {
  # Test with json_schema for performance
  built_schema <- json_schema(json_object(
    var1 = json_string(),
    var2 = json_integer()
  ))

  result <- repair_json_str(
    '{var1: 123, var2: "456"}',
    schema = built_schema,
    return_objects = FALSE
  )
  expect_equal(result, "{\"var1\":\"123\",\"var2\":456}")
})

test_that("schema handles arrays", {
  schema <- json_object(
    name = json_string(),
    scores = json_array(json_integer())
  )

  result <- repair_json_str(
    '{"name": "Alice", "scores": [90, 85, 95]}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(
    result,
    list(
      name = "Alice",
      scores = c(90, 85, 95)
    )
  )
})

test_that("schema handles nested maps", {
  schema <- json_object(
    name = json_string(),
    address = json_object(
      city = json_string(),
      zip = json_integer()
    )
  )

  result <- repair_json_str(
    '{"name": "Alice", "address": {"city": "NYC", "zip": 10001}}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(
    result,
    list(
      name = "Alice",
      address = list(city = "NYC", zip = 10001L)
    )
  )
})

test_that("schema with json_any accepts any type", {
  schema <- json_object(
    name = json_string(),
    metadata = json_any()
  )

  result <- repair_json_str(
    '{"name": "Alice", "metadata": {"custom": "value", "count": 5}}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(
    result,
    list(
      name = "Alice",
      metadata = list(count = 5, custom = "value")
    )
  )
})

test_that("schema works with repair_json_file", {
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"name": "Alice"}', tmp_file)

  schema <- json_object(
    name = json_string(),
    age = json_integer(.default = 25L, .required = FALSE)
  )

  result <- repair_json_file(tmp_file, schema = schema, return_objects = TRUE)
  expect_equal(result$name, "Alice")
  expect_false("age" %in% names(result))

  unlink(tmp_file)
})

test_that("schema works with repair_json_raw", {
  raw_data <- charToRaw('{"name": "Bob"}')

  schema <- json_object(
    name = json_string(),
    active = json_boolean(.default = TRUE, .required = FALSE)
  )

  result <- repair_json_raw(raw_data, schema = schema, return_objects = TRUE)
  expect_equal(result$name, "Bob")
  expect_false("active" %in% names(result))
})

# Tests for Date schema
test_that("json_date creates valid schema", {
  schema <- json_date()
  expect_s3_class(schema, "LLMJsonSchema")
  expect_equal(schema$type, "date")
})

test_that("json_date parses ISO8601 date strings", {
  schema <- json_object(event_date = json_date())

  result <- repair_json_str(
    '{"event_date": "2024-01-15"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result$event_date, as.Date("2024-01-15"))
})

test_that("json_date parses custom format date strings", {
  schema <- json_object(event_date = json_date(.format = "%m/%d/%Y"))

  result <- repair_json_str(
    '{"event_date": "01/15/2024"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result$event_date, as.Date("2024-01-15"))
})

test_that("json_date parses named format date strings", {
  schema <- json_object(event_date = json_date(.format = "us_date"))

  result <- repair_json_str(
    '{"event_date": "01/15/2024"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result$event_date, as.Date("2024-01-15"))
})

test_that("json_date handles numeric dates", {
  schema <- json_object(event_date = json_date())

  result <- repair_json_str(
    '{"event_date": 19737}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result$event_date, as.Date(19737))
})

test_that("json_date handles optional dates", {
  schema <- json_object(
    name = json_string(),
    birthday = json_date(.required = FALSE)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result$name, "Alice")
  expect_false("birthday" %in% names(result))
})

test_that("json_date handles default dates", {
  default_date <- as.Date("2024-01-01")
  schema <- json_object(
    name = json_string(),
    start_date = json_date(.default = default_date, .required = TRUE)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result$name, "Alice")
  expect_equal(result$start_date, default_date)
})

# Tests for POSIXct schema
test_that("json_timestamp creates valid schema", {
  schema <- json_timestamp()
  expect_s3_class(schema, "LLMJsonSchema")
  expect_equal(schema$type, "timestamp")
})

test_that("json_timestamp parses ISO8601 datetime strings", {
  schema <- json_object(timestamp = json_timestamp())

  result <- repair_json_str(
    '{"timestamp": "2024-01-15T10:30:45"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_s3_class(result$timestamp, "POSIXct")
  expect_equal(
    result$timestamp,
    as.POSIXct("2024-01-15 10:30:45", tz = "UTC")
  )
})

test_that("json_timestamp parses ISO8601Z datetime strings", {
  schema <- json_object(timestamp = json_timestamp(.format = "iso8601z"))

  result <- repair_json_str(
    '{"timestamp": "2024-01-15T10:30:45Z"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(
    result$timestamp,
    as.POSIXct("2024-01-15 10:30:45", tz = "UTC")
  )
})

test_that("json_timestamp parses Unix timestamps from strings", {
  schema <- json_object(timestamp = json_timestamp(.format = "unix"))

  result <- repair_json_str(
    '{"timestamp": "1705318245"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(
    result$timestamp,
    as.POSIXct("2024-01-15 11:30:45", tz = "UTC")
  )
})

test_that("json_timestamp parses Unix timestamps from numbers", {
  schema <- json_object(timestamp = json_timestamp())

  result <- repair_json_str(
    '{"timestamp": 1705318245}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(
    result$timestamp,
    as.POSIXct("2024-01-15 11:30:45", tz = "UTC")
  )
})

test_that("json_timestamp parses millisecond timestamps", {
  schema <- json_object(timestamp = json_timestamp(.format = "unix_ms"))

  result <- repair_json_str(
    '{"timestamp": "1705318245000"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(
    result$timestamp,
    as.POSIXct("2024-01-15 11:30:45", tz = "UTC")
  )
})

test_that("json_timestamp handles custom format", {
  schema <- json_object(
    timestamp = json_timestamp(.format = "%m/%d/%Y %H:%M:%S")
  )

  result <- repair_json_str(
    '{"timestamp": "01/15/2024 10:30:45"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(
    result$timestamp,
    as.POSIXct("2024-01-15 10:30:45", tz = "UTC")
  )
})

test_that("json_timestamp handles timezone", {
  schema <- json_object(timestamp = json_timestamp(.tz = "America/New_York"))

  result <- repair_json_str(
    '{"timestamp": "2024-01-15T10:30:45"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(
    result$timestamp,
    as.POSIXct("2024-01-15 10:30:45", tz = "America/New_York")
  )
})

test_that("json_timestamp handles optional timestamps", {
  schema <- json_object(
    name = json_string(),
    created_at = json_timestamp(.required = FALSE)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result$name, "Alice")
  expect_false("created_at" %in% names(result))
})

test_that("json_timestamp handles default timestamps", {
  default_time <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  schema <- json_object(
    name = json_string(),
    created_at = json_timestamp(.default = default_time, .required = TRUE)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result$name, "Alice")
  expect_s3_class(result$created_at, "POSIXct")
  expect_equal(as.numeric(result$created_at), as.numeric(default_time))
})

test_that("json_date and json_timestamp work together in schema", {
  schema <- json_object(
    name = json_string(),
    birthday = json_date(),
    last_login = json_timestamp()
  )

  result <- repair_json_str(
    '{"name": "Alice", "birthday": "1990-05-15", "last_login": "2024-01-15T10:30:45"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_equal(result$name, "Alice")
  expect_s3_class(result$birthday, "Date")
  expect_s3_class(result$last_login, "POSIXct")
})

# Tests for int64 parameter
test_that("int64 parameter with 'double' policy (default)", {
  json_str <- '{"small": 42, "big": 9007199254740993}'
  result <- repair_json_str(json_str, return_objects = TRUE, int64 = "double")

  expect_equal(class(result$small), "integer")
  expect_equal(result$small, 42L)

  # Large integer becomes double (may lose precision)
  expect_equal(class(result$big), "numeric")
  expect_equal(result$big, 9007199254740992) # Note: loses precision
})

test_that("int64 parameter with 'string' policy", {
  json_str <- '{"small": 42, "big": 9007199254740993}'
  result <- repair_json_str(json_str, return_objects = TRUE, int64 = "string")

  expect_equal(class(result$small), "integer")
  expect_equal(result$small, 42L)

  # Large integer becomes string (preserves exact value)
  expect_equal(class(result$big), "character")
  expect_equal(result$big, "9007199254740993")
})

test_that("int64 parameter with 'bit64' policy", {
  skip_if_not_installed("bit64")

  json_str <- '{"small": 42, "big": 9007199254740993}'
  result <- repair_json_str(json_str, return_objects = TRUE, int64 = "bit64")

  expect_equal(class(result$small), "integer")
  expect_equal(result$small, 42L)

  # Large integer becomes integer64
  expect_equal(class(result$big), "integer64")
  expect_true(bit64::is.integer64(result$big))
  expect_equal(format(result$big), "9007199254740993")
})

test_that("int64 parameter works with arrays", {
  json_str <- '{"ids": [1, 9007199254740993, 42]}'

  # String policy
  result_str <- repair_json_str(
    json_str,
    return_objects = TRUE,
    int64 = "string"
  )
  expect_true(is.list(result_str$ids)) # Mixed types -> list
  expect_equal(result_str$ids[[1]], 1L)
  expect_equal(result_str$ids[[2]], "9007199254740993")
  expect_equal(result_str$ids[[3]], 42L)
})

test_that("int64 parameter works with repair_json_file", {
  skip_if_not_installed("bit64")

  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"big": 9007199254740993}', tmp_file)
  on.exit(unlink(tmp_file))

  result <- repair_json_file(tmp_file, return_objects = TRUE, int64 = "bit64")
  expect_equal(class(result$big), "integer64")
  expect_equal(format(result$big), "9007199254740993")
})

test_that("int64 parameter works with repair_json_raw", {
  skip_if_not_installed("bit64")

  raw_data <- charToRaw('{"big": 9007199254740993}')
  result <- repair_json_raw(raw_data, return_objects = TRUE, int64 = "bit64")

  expect_equal(class(result$big), "integer64")
  expect_equal(format(result$big), "9007199254740993")
})

# Tests for repair_json_conn
test_that("repair_json_conn reads and repairs JSON from connection", {
  # Create a temporary file with malformed JSON
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"key": "value",}', tmp_file)
  
  # Open connection and repair
  conn <- file(tmp_file, "r")
  result <- repair_json_conn(conn)
  close(conn)
  
  expect_type(result, "character")
  expect_true(grepl('"key"', result))
  expect_true(grepl('"value"', result))
  expect_false(grepl(',}', result))
  
  # Clean up
  unlink(tmp_file)
})

test_that("repair_json_conn works with return_objects = TRUE", {
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"name": "Alice", "age": 30}', tmp_file)
  
  conn <- file(tmp_file, "r")
  result <- repair_json_conn(conn, return_objects = TRUE)
  close(conn)
  
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$age, 30)
  
  unlink(tmp_file)
})

test_that("repair_json_conn works with schema", {
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"name": "Bob"}', tmp_file)

  schema <- json_object(
    name = json_string(),
    age = json_integer(.default = 25L, .required = TRUE)
  )

  conn <- file(tmp_file, "r")
  result <- repair_json_conn(conn, schema = schema, return_objects = TRUE)
  close(conn)

  expect_equal(result$name, "Bob")
  expect_equal(result$age, 25)

  unlink(tmp_file)
})

test_that("repair_json_conn handles int64 parameter", {
  skip_if_not_installed("bit64")
  
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"big": 9007199254740993}', tmp_file)
  
  conn <- file(tmp_file, "r")
  result <- repair_json_conn(conn, return_objects = TRUE, int64 = "bit64")
  close(conn)
  
  expect_equal(class(result$big), "integer64")
  expect_equal(format(result$big), "9007199254740993")
  
  unlink(tmp_file)
})

test_that("repair_json_conn handles multiline JSON", {
  tmp_file <- tempfile(fileext = ".json")
  writeLines(c(
    '{',
    '  "users": [',
    '    {"name": "Alice", "age": 30,},',
    '    {"name": "Bob", "age": 25,}',
    '  ],',
    '}'
  ), tmp_file)
  
  conn <- file(tmp_file, "r")
  result <- repair_json_conn(conn, return_objects = TRUE)
  close(conn)
  
  expect_type(result, "list")
  expect_length(result$users, 2)
  expect_equal(result$users[[1]]$name, "Alice")
  expect_equal(result$users[[2]]$name, "Bob")
  
  unlink(tmp_file)
})

test_that("repair_json_conn fails with non-connection input", {
  expect_error(
    repair_json_conn("not a connection"),
    "must be a connection object"
  )
})

test_that("repair_json_conn uses optimized readChar for binary connections", {
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"name": "Alice", "age": 30}', tmp_file)
  
  # Open as binary connection
  conn <- file(tmp_file, "rb")
  result <- repair_json_conn(conn, return_objects = TRUE)
  close(conn)
  
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$age, 30)
  
  unlink(tmp_file)
})
