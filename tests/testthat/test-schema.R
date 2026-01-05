# Tests for ellmer to llmjson schema conversion
skip_if_not_installed("ellmer")

# Load ellmer to register S3 methods
suppressPackageStartupMessages(library(ellmer))

# Basic type conversion tests
test_that("ellmer TypeBasic converts to llmjson basic types", {
  # String type
  ellmer_string <- type_string(required = TRUE)
  llmjson_string <- json_string(.required = TRUE)

  expect_equal(
    json_schema(ellmer_string),
    json_schema(llmjson_string)
  )

  # Integer type
  ellmer_int <- type_integer(required = TRUE)
  llmjson_int <- json_integer(.required = TRUE)

  expect_equal(
    json_schema(ellmer_int),
    json_schema(llmjson_int)
  )

  # Number type
  ellmer_num <- type_number(required = TRUE)
  llmjson_num <- json_number(.required = TRUE)

  expect_equal(
    json_schema(ellmer_num),
    json_schema(llmjson_num)
  )

  # Boolean type
  ellmer_bool <- type_boolean(required = TRUE)
  llmjson_bool <- json_boolean(.required = TRUE)

  expect_equal(
    json_schema(ellmer_bool),
    json_schema(llmjson_bool)
  )
})

test_that("ellmer TypeBasic handles required field correctly", {
  # Required string
  ellmer_required <- type_string(required = TRUE)
  llmjson_required <- json_string(.required = TRUE)

  expect_equal(
    json_schema(ellmer_required),
    json_schema(llmjson_required)
  )

  # Optional string (default)
  ellmer_optional <- type_string(required = FALSE)
  llmjson_optional <- json_string(.required = FALSE)

  expect_equal(
    json_schema(ellmer_optional),
    json_schema(llmjson_optional)
  )
})

# Enum type conversion tests
test_that("ellmer TypeEnum converts to llmjson json_enum", {
  ellmer_enum <- type_enum(c("active", "inactive"), required = TRUE)
  llmjson_enum <- json_enum(.values = c("active", "inactive"), .required = TRUE)

  expect_equal(
    json_schema(ellmer_enum),
    json_schema(llmjson_enum)
  )
})

test_that("ellmer TypeEnum handles multiple values", {
  values <- c("red", "green", "blue", "yellow")
  ellmer_enum <- type_enum(values, required = FALSE)
  llmjson_enum <- json_enum(.values = values, .required = FALSE)

  expect_equal(
    json_schema(ellmer_enum),
    json_schema(llmjson_enum)
  )
})

# Array type conversion tests
test_that("ellmer TypeArray converts to llmjson json_array", {
  ellmer_array <- type_array(type_integer(), required = TRUE)
  llmjson_array <- json_array(json_integer(), .required = TRUE)

  expect_equal(
    json_schema(ellmer_array),
    json_schema(llmjson_array)
  )
})

test_that("ellmer TypeArray handles nested arrays", {
  ellmer_nested <- type_array(
    type_array(type_string(), required = FALSE),
    required = TRUE
  )
  llmjson_nested <- json_array(
    json_array(json_string(), .required = FALSE),
    .required = TRUE
  )

  expect_equal(
    json_schema(ellmer_nested),
    json_schema(llmjson_nested)
  )
})

test_that("ellmer TypeArray handles arrays of enums", {
  ellmer_array_enum <- type_array(
    type_enum(c("small", "medium", "large")),
    required = FALSE
  )
  llmjson_array_enum <- json_array(
    json_enum(.values = c("small", "medium", "large")),
    .required = FALSE
  )

  expect_equal(
    json_schema(ellmer_array_enum),
    json_schema(llmjson_array_enum)
  )
})

# Object type conversion tests
test_that("ellmer TypeObject converts to llmjson json_object - basic", {
  ellmer_obj <- type_object(
    name = type_string(required = TRUE),
    age = type_integer(required = FALSE)
  )

  llmjson_obj <- json_object(
    name = json_string(.required = TRUE),
    age = json_integer(.required = FALSE)
  )

  expect_equal(
    json_schema(ellmer_obj),
    json_schema(llmjson_obj)
  )
})

test_that("ellmer TypeObject converts to llmjson json_object - user example", {
  # Original example from user
  ellmer_user <- type_object(
    name = type_string(required = TRUE),
    age = type_integer(),
    status = type_enum(c("active", "inactive"), required = TRUE)
  )

  llmjson_user <- json_object(
    name = json_string(.required = TRUE),
    age = json_integer(),
    status = json_enum(.values = c("active", "inactive"), .required = TRUE)
  )

  expect_equal(
    json_schema(ellmer_user),
    json_schema(llmjson_user)
  )
})

test_that("ellmer TypeObject handles nested objects", {
  ellmer_nested <- type_object(
    user = type_object(
      name = type_string(required = TRUE),
      email = type_string(required = TRUE)
    ),
    age = type_integer()
  )

  llmjson_nested <- json_object(
    user = json_object(
      name = json_string(.required = TRUE),
      email = json_string(.required = TRUE)
    ),
    age = json_integer()
  )

  expect_equal(
    json_schema(ellmer_nested),
    json_schema(llmjson_nested)
  )
})

test_that("ellmer TypeObject handles objects with arrays", {
  ellmer_with_array <- type_object(
    name = type_string(required = TRUE),
    scores = type_array(type_number(), required = TRUE),
    tags = type_array(type_string())
  )

  llmjson_with_array <- json_object(
    name = json_string(.required = TRUE),
    scores = json_array(json_number(), .required = TRUE),
    tags = json_array(json_string())
  )

  expect_equal(
    json_schema(ellmer_with_array),
    json_schema(llmjson_with_array)
  )
})

test_that("ellmer TypeObject handles mixed complex types", {
  ellmer_complex <- type_object(
    id = type_integer(required = TRUE),
    status = type_enum(c("draft", "published", "archived"), required = TRUE),
    metadata = type_object(
      created_by = type_string(),
      tags = type_array(type_string())
    ),
    scores = type_array(type_number(), required = TRUE)
  )

  llmjson_complex <- json_object(
    id = json_integer(.required = TRUE),
    status = json_enum(.values = c("draft", "published", "archived"), .required = TRUE),
    metadata = json_object(
      created_by = json_string(),
      tags = json_array(json_string())
    ),
    scores = json_array(json_number(), .required = TRUE)
  )

  expect_equal(
    json_schema(ellmer_complex),
    json_schema(llmjson_complex)
  )
})

# TypeIgnore handling tests
test_that("ellmer TypeIgnore fields are filtered out", {
  ellmer_with_ignore <- type_object(
    name = type_string(required = TRUE),
    internal = type_ignore(),
    age = type_integer()
  )

  # Should be equivalent to schema without the ignored field
  llmjson_without_ignore <- json_object(
    name = json_string(.required = TRUE),
    age = json_integer()
  )

  expect_equal(
    json_schema(ellmer_with_ignore),
    json_schema(llmjson_without_ignore)
  )
})

test_that("ellmer TypeObject with all TypeIgnore fields fails", {
  expect_error(
    json_schema(type_object(
      field1 = type_ignore(),
      field2 = type_ignore()
    )),
    "must have at least one non-ignored field"
  )
})

# Error handling tests
test_that("ellmer TypeArray with TypeIgnore items fails", {
  expect_error(
    json_schema(type_array(type_ignore())),
    "Array items type cannot be TypeIgnore"
  )
})

test_that("ellmer TypeJsonSchema is not supported", {
  skip("TypeJsonSchema class doesn't exist in current ellmer version")

  json_schema_type <- new(
    "ellmer::TypeJsonSchema",
    schema = list(type = "string"),
    required = FALSE
  )

  expect_error(
    json_schema(json_schema_type),
    "TypeJsonSchema is not directly supported"
  )
})

test_that("unsupported ellmer type produces error", {
  skip("Cannot create mock S7 objects easily - would need proper S7 class definition")

  # Create a mock S7 class that's not a valid ellmer type
  mock_type <- structure(
    list(),
    class = c("ellmer::UnknownType", "ellmer::Type", "S7_object")
  )

  expect_error(
    llmjson:::convert_ellmer_type(mock_type),
    "Unsupported ellmer type"
  )
})

# Integration tests with actual JSON repair
test_that("ellmer schema works with repair_json_str", {
  ellmer_schema <- type_object(
    name = type_string(required = TRUE),
    age = type_integer(),
    status = type_enum(c("active", "inactive"), required = TRUE)
  )

  built_schema <- json_schema(ellmer_schema)

  result <- repair_json_str(
    '{"name": "Alice", "age": 30, "status": "active"}',
    schema = built_schema,
    return_objects = TRUE
  )

  expect_type(result, "list")
  expect_equal(result$name, "Alice")
  expect_equal(result$age, 30L)
  expect_equal(result$status, "active")
})

test_that("ellmer schema validates enum values", {
  ellmer_schema <- type_object(
    status = type_enum(c("active", "inactive"), required = TRUE)
  )

  built_schema <- json_schema(ellmer_schema)

  # Valid enum value
  result <- repair_json_str(
    '{"status": "active"}',
    schema = built_schema,
    return_objects = TRUE
  )
  expect_equal(result$status, "active")

  # Invalid enum value should error
  expect_error(
    repair_json_str(
      '{"status": "unknown"}',
      schema = built_schema,
      return_objects = TRUE
    ),
    "not one of the allowed enum values"
  )
})

test_that("ellmer schema handles nested structures in repair", {
  ellmer_schema <- type_object(
    user = type_object(
      name = type_string(required = TRUE),
      roles = type_array(type_string(), required = TRUE)
    ),
    active = type_boolean(required = TRUE)
  )

  built_schema <- json_schema(ellmer_schema)

  result <- repair_json_str(
    '{"user": {"name": "Alice", "roles": ["admin", "user"]}, "active": true}',
    schema = built_schema,
    return_objects = TRUE
  )

  expect_equal(result$user$name, "Alice")
  expect_equal(result$user$roles, c("admin", "user"))
  expect_equal(result$active, TRUE)
})

test_that("ellmer schema works with repair_json_file", {
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"name": "Bob", "age": 25}', tmp_file)
  on.exit(unlink(tmp_file))

  ellmer_schema <- type_object(
    name = type_string(required = TRUE),
    age = type_integer()
  )

  built_schema <- json_schema(ellmer_schema)

  result <- repair_json_file(
    tmp_file,
    schema = built_schema,
    return_objects = TRUE
  )

  expect_equal(result$name, "Bob")
  expect_equal(result$age, 25L)
})

test_that("ellmer schema works with repair_json_raw", {
  raw_data <- charToRaw('{"name": "Charlie", "active": true}')

  ellmer_schema <- type_object(
    name = type_string(required = TRUE),
    active = type_boolean()
  )

  built_schema <- json_schema(ellmer_schema)

  result <- repair_json_raw(
    raw_data,
    schema = built_schema,
    return_objects = TRUE
  )

  expect_equal(result$name, "Charlie")
  expect_equal(result$active, TRUE)
})

test_that("ellmer schema works with repair_json_conn", {
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"name": "David", "score": 95}', tmp_file)
  on.exit(unlink(tmp_file))

  ellmer_schema <- type_object(
    name = type_string(required = TRUE),
    score = type_integer()
  )

  built_schema <- json_schema(ellmer_schema)

  conn <- file(tmp_file, "r")
  result <- repair_json_conn(
    conn,
    schema = built_schema,
    return_objects = TRUE
  )
  close(conn)

  expect_equal(result$name, "David")
  expect_equal(result$score, 95L)
})

# Performance test: built schema reuse
test_that("ellmer schema can be reused efficiently", {
  ellmer_schema <- type_object(
    name = type_string(required = TRUE),
    value = type_number()
  )

  # Build once
  built_schema <- json_schema(ellmer_schema)

  # Reuse multiple times
  result1 <- repair_json_str(
    '{"name": "test1", "value": 1.5}',
    schema = built_schema,
    return_objects = TRUE
  )

  result2 <- repair_json_str(
    '{"name": "test2", "value": 2.5}',
    schema = built_schema,
    return_objects = TRUE
  )

  result3 <- repair_json_str(
    '{"name": "test3", "value": 3.5}',
    schema = built_schema,
    return_objects = TRUE
  )

  expect_equal(result1$name, "test1")
  expect_equal(result2$name, "test2")
  expect_equal(result3$name, "test3")
  expect_equal(result1$value, 1.5)
  expect_equal(result2$value, 2.5)
  expect_equal(result3$value, 3.5)
})

# Schema printing tests
test_that("ellmer schema prints correctly", {
  ellmer_schema <- type_object(
    name = type_string(required = TRUE),
    age = type_integer(),
    status = type_enum(c("active", "inactive"), required = TRUE)
  )

  built_schema <- json_schema(ellmer_schema)

  # Should not error when printing
  expect_output(print(built_schema), "name")
  expect_output(print(built_schema), "age")
  expect_output(print(built_schema), "status")
  expect_output(print(built_schema), "enum")
  expect_output(print(built_schema), "active")
  expect_output(print(built_schema), "inactive")
})

# Edge cases
test_that("ellmer deeply nested structure converts correctly", {
  ellmer_deep <- type_object(
    level1 = type_object(
      level2 = type_object(
        level3 = type_object(
          value = type_string(required = TRUE)
        )
      )
    )
  )

  llmjson_deep <- json_object(
    level1 = json_object(
      level2 = json_object(
        level3 = json_object(
          value = json_string(.required = TRUE)
        )
      )
    )
  )

  expect_equal(
    json_schema(ellmer_deep),
    json_schema(llmjson_deep)
  )
})

test_that("ellmer array of objects converts correctly", {
  ellmer_array_obj <- type_array(
    type_object(
      id = type_integer(required = TRUE),
      name = type_string(required = TRUE)
    ),
    required = TRUE
  )

  llmjson_array_obj <- json_array(
    json_object(
      id = json_integer(.required = TRUE),
      name = json_string(.required = TRUE)
    ),
    .required = TRUE
  )

  expect_equal(
    json_schema(ellmer_array_obj),
    json_schema(llmjson_array_obj)
  )

  # Test with actual data
  built_schema <- json_schema(ellmer_array_obj)

  result <- repair_json_str(
    '[{"id": 1, "name": "Alice"}, {"id": 2, "name": "Bob"}]',
    schema = built_schema,
    return_objects = TRUE
  )

  expect_length(result, 2)
  expect_equal(result[[1]]$id, 1L)
  expect_equal(result[[1]]$name, "Alice")
  expect_equal(result[[2]]$id, 2L)
  expect_equal(result[[2]]$name, "Bob")
})
