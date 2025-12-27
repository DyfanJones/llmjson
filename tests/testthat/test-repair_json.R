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
