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
test_that("s_map creates a valid schema", {
  schema <- s_map(name = s_string(), age = s_integer())
  expect_s3_class(schema, "llmjson_schema")
  expect_equal(schema$type, "map")
  expect_length(schema$fields, 2)
})

test_that("s_integer, s_double, s_string, s_logical create valid schemas", {
  expect_s3_class(s_integer(), "llmjson_schema")
  expect_s3_class(s_double(), "llmjson_schema")
  expect_s3_class(s_string(), "llmjson_schema")
  expect_s3_class(s_logical(), "llmjson_schema")
})

test_that("schema with return_objects validates and converts types", {
  schema <- s_map(
    name = s_string(),
    age = s_integer()
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

test_that("schema applies defaults for missing optional fields with return_objects = TRUE", {
  schema <- s_map(
    name = s_string(),
    age = s_integer(.optional = TRUE, .default = 0L)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$age, 0L)
})

test_that("schema applies defaults for missing optional fields with return_objects = FALSE", {
  schema <- s_map(
    name = s_string(),
    age = s_integer(.optional = TRUE, .default = 0L)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = FALSE
  )
  expect_type(result, "character")
  expect_true(grepl('"age"', result))
  expect_true(grepl('"name"', result))
})

test_that("schema returns NULL for optional fields without defaults", {
  schema <- s_map(
    name = s_string(),
    age = s_integer(.optional = TRUE)
  )

  result <- repair_json_str(
    '{"name": "Alice"}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_null(result$age)
})

test_that("schema errors on missing required fields", {
  schema <- s_map(
    name = s_string(),
    age = s_integer()
  )

  expect_error(
    repair_json_str(
      '{"name": "Alice"}',
      schema = schema,
      return_objects = TRUE
    ),
    "Required field 'age' is missing"
  )

  expect_error(
    repair_json_str(
      '{"name": "Alice"}',
      schema = schema,
      return_objects = FALSE
    ),
    "Required field 'age' is missing"
  )
})

test_that("schema coerces number to string", {
  schema <- s_map(
    name = s_string(),
    age = s_string()
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
  schema <- s_map(
    name = s_string(),
    age = s_integer()
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
  schema <- s_map(active = s_integer())

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

test_that("schema handles arrays", {
  schema <- s_map(
    name = s_string(),
    scores = s_array(s_integer())
  )

  result <- repair_json_str(
    '{"name": "Alice", "scores": [90, 85, 95]}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$scores, c(90L, 85L, 95L))
})

test_that("schema handles nested maps", {
  schema <- s_map(
    name = s_string(),
    address = s_map(
      city = s_string(),
      zip = s_integer()
    )
  )

  result <- repair_json_str(
    '{"name": "Alice", "address": {"city": "NYC", "zip": 10001}}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$address$city, "NYC")
  expect_equal(result$address$zip, 10001L)
})

test_that("schema with s_any accepts any type", {
  schema <- s_map(
    name = s_string(),
    metadata = s_any()
  )

  result <- repair_json_str(
    '{"name": "Alice", "metadata": {"custom": "value", "count": 5}}',
    schema = schema,
    return_objects = TRUE
  )
  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_type(result$metadata, "list")
})

test_that("schema works with repair_json_file", {
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"name": "Alice"}', tmp_file)

  schema <- s_map(
    name = s_string(),
    age = s_integer(.optional = TRUE, .default = 25L)
  )

  result <- repair_json_file(tmp_file, schema = schema, return_objects = TRUE)
  expect_equal(result$name, "Alice")
  expect_equal(result$age, 25L)

  unlink(tmp_file)
})

test_that("schema works with repair_json_raw", {
  raw_data <- charToRaw('{"name": "Bob"}')

  schema <- s_map(
    name = s_string(),
    active = s_logical(.optional = TRUE, .default = TRUE)
  )

  result <- repair_json_raw(raw_data, schema = schema, return_objects = TRUE)
  expect_equal(result$name, "Bob")
  expect_equal(result$active, TRUE)
})
