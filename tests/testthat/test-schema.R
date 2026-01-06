# Basic type conversion tests
test_that("ellmer TypeBasic converts to llmjson basic types", {
  skip_if_not_installed("ellmer")
  # String type
  ellmer_string <- ellmer::type_string(required = TRUE)
  llmjson_string <- json_string(.required = TRUE)

  expect_equal(
    json_schema(ellmer_string),
    json_schema(llmjson_string)
  )

  # Integer type
  ellmer_int <- ellmer::type_integer(required = TRUE)
  llmjson_int <- json_integer(.required = TRUE)

  expect_equal(
    json_schema(ellmer_int),
    json_schema(llmjson_int)
  )

  # Number type
  ellmer_num <- ellmer::type_number(required = TRUE)
  llmjson_num <- json_number(.required = TRUE)

  expect_equal(
    json_schema(ellmer_num),
    json_schema(llmjson_num)
  )

  # Boolean type
  ellmer_bool <- ellmer::type_boolean(required = TRUE)
  llmjson_bool <- json_boolean(.required = TRUE)

  expect_equal(
    json_schema(ellmer_bool),
    json_schema(llmjson_bool)
  )
})

test_that("ellmer TypeBasic handles required field correctly", {
  skip_if_not_installed("ellmer")
  # Required string
  ellmer_required <- ellmer::type_string(required = TRUE)
  llmjson_required <- json_string(.required = TRUE)

  expect_equal(
    json_schema(ellmer_required),
    json_schema(llmjson_required)
  )

  # Optional string (default)
  ellmer_optional <- ellmer::type_string(required = FALSE)
  llmjson_optional <- json_string(.required = FALSE)

  expect_equal(
    json_schema(ellmer_optional),
    json_schema(llmjson_optional)
  )
})

# Enum type conversion tests
test_that("ellmer TypeEnum converts to llmjson json_enum", {
  skip_if_not_installed("ellmer")
  ellmer_enum <- ellmer::type_enum(c("active", "inactive"), required = TRUE)
  llmjson_enum <- json_enum(.values = c("active", "inactive"), .required = TRUE)

  expect_equal(
    json_schema(ellmer_enum),
    json_schema(llmjson_enum)
  )
})

test_that("ellmer TypeEnum handles multiple values", {
  skip_if_not_installed("ellmer")
  values <- c("red", "green", "blue", "yellow")
  ellmer_enum <- ellmer::type_enum(values, required = FALSE)
  llmjson_enum <- json_enum(.values = values, .required = FALSE)

  expect_equal(
    json_schema(ellmer_enum),
    json_schema(llmjson_enum)
  )
})

# Array type conversion tests
test_that("ellmer TypeArray converts to llmjson json_array", {
  skip_if_not_installed("ellmer")
  ellmer_array <- ellmer::type_array(ellmer::type_integer(), required = TRUE)
  llmjson_array <- json_array(json_integer(), .required = TRUE)

  expect_equal(
    json_schema(ellmer_array),
    json_schema(llmjson_array)
  )
})

test_that("ellmer TypeArray handles nested arrays", {
  skip_if_not_installed("ellmer")
  ellmer_nested <- ellmer::type_array(
    ellmer::type_array(ellmer::type_string(), required = FALSE),
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
  skip_if_not_installed("ellmer")
  ellmer_array_enum <- ellmer::type_array(
    ellmer::type_enum(c("small", "medium", "large")),
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
  skip_if_not_installed("ellmer")
  ellmer_obj <- ellmer::type_object(
    name = ellmer::type_string(required = TRUE),
    age = ellmer::type_integer(required = FALSE)
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
  skip_if_not_installed("ellmer")
  # Original example from user
  ellmer_user <- ellmer::type_object(
    name = ellmer::type_string(required = TRUE),
    age = ellmer::type_integer(),
    status = ellmer::type_enum(c("active", "inactive"), required = TRUE)
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
  skip_if_not_installed("ellmer")
  ellmer_nested <- ellmer::type_object(
    user = ellmer::type_object(
      name = ellmer::type_string(required = TRUE),
      email = ellmer::type_string(required = TRUE)
    ),
    age = ellmer::type_integer()
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
  skip_if_not_installed("ellmer")
  ellmer_with_array <- ellmer::type_object(
    name = ellmer::type_string(required = TRUE),
    scores = ellmer::type_array(ellmer::type_number(), required = TRUE),
    tags = ellmer::type_array(ellmer::type_string())
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
  skip_if_not_installed("ellmer")
  ellmer_complex <- ellmer::type_object(
    id = ellmer::type_integer(required = TRUE),
    status = ellmer::type_enum(
      c("draft", "published", "archived"),
      required = TRUE
    ),
    metadata = ellmer::type_object(
      created_by = ellmer::type_string(),
      tags = ellmer::type_array(ellmer::type_string())
    ),
    scores = ellmer::type_array(ellmer::type_number(), required = TRUE)
  )

  llmjson_complex <- json_object(
    id = json_integer(.required = TRUE),
    status = json_enum(
      .values = c("draft", "published", "archived"),
      .required = TRUE
    ),
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
  skip_if_not_installed("ellmer")
  ellmer_with_ignore <- ellmer::type_object(
    name = ellmer::type_string(required = TRUE),
    internal = ellmer::type_ignore(),
    age = ellmer::type_integer()
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
  skip_if_not_installed("ellmer")
  expect_error(
    json_schema(ellmer::type_object(
      field1 = ellmer::type_ignore(),
      field2 = ellmer::type_ignore()
    )),
    "must have at least one non-ignored field"
  )
})

# Error handling tests
test_that("ellmer TypeArray with TypeIgnore items fails", {
  skip_if_not_installed("ellmer")
  expect_error(
    json_schema(ellmer::type_array(ellmer::type_ignore())),
    "Array items type cannot be TypeIgnore"
  )
})

# Integration tests with actual JSON repair
test_that("ellmer schema works with repair_json_str", {
  skip_if_not_installed("ellmer")
  ellmer_schema <- ellmer::type_object(
    name = ellmer::type_string(required = TRUE),
    age = ellmer::type_integer(),
    status = ellmer::type_enum(c("active", "inactive"), required = TRUE)
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
  skip_if_not_installed("ellmer")
  ellmer_schema <- ellmer::type_object(
    status = ellmer::type_enum(c("active", "inactive"), required = TRUE)
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
  skip_if_not_installed("ellmer")
  ellmer_schema <- ellmer::type_object(
    user = ellmer::type_object(
      name = ellmer::type_string(required = TRUE),
      roles = ellmer::type_array(ellmer::type_string(), required = TRUE)
    ),
    active = ellmer::type_boolean(required = TRUE)
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
  skip_if_not_installed("ellmer")
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"name": "Bob", "age": 25}', tmp_file)
  on.exit(unlink(tmp_file))

  ellmer_schema <- ellmer::type_object(
    name = ellmer::type_string(required = TRUE),
    age = ellmer::type_integer()
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
  skip_if_not_installed("ellmer")
  raw_data <- charToRaw('{"name": "Charlie", "active": true}')

  ellmer_schema <- ellmer::type_object(
    name = ellmer::type_string(required = TRUE),
    active = ellmer::type_boolean()
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
  skip_if_not_installed("ellmer")
  tmp_file <- tempfile(fileext = ".json")
  writeLines('{"name": "David", "score": 95}', tmp_file)
  on.exit(unlink(tmp_file))

  ellmer_schema <- ellmer::type_object(
    name = ellmer::type_string(required = TRUE),
    score = ellmer::type_integer()
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
  skip_if_not_installed("ellmer")
  ellmer_schema <- ellmer::type_object(
    name = ellmer::type_string(required = TRUE),
    value = ellmer::type_number()
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

  expect_equal(
    result1,
    list(
      name = "test1",
      value = 1.5
    )
  )
  expect_equal(
    result2,
    list(
      name = "test2",
      value = 2.5
    )
  )
  expect_equal(
    result3,
    list(
      name = "test3",
      value = 3.5
    )
  )
})

# Edge cases
test_that("ellmer deeply nested structure converts correctly", {
  ellmer_deep <- ellmer::type_object(
    level1 = ellmer::type_object(
      level2 = ellmer::type_object(
        level3 = ellmer::type_object(
          value = ellmer::type_string(required = TRUE)
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
  ellmer_array_obj <- ellmer::type_array(
    ellmer::type_object(
      id = ellmer::type_integer(required = TRUE),
      name = ellmer::type_string(required = TRUE)
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

  expect_equal(
    result,
    list(
      list(id = 1, name = "Alice"),
      list(id = 2, name = "Bob")
    )
  )
})
