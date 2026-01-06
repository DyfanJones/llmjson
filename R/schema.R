#' Schema builders for JSON repair and validation
#'
#' These functions create schema definitions that guide JSON repair and
#' conversion to R objects. Schemas ensure that the repaired JSON conforms
#' to expected types and structure.
#'
#' @param ... Named arguments defining the schema for each field (json_object only)
#' @param items Schema definition for array elements (json_array only)
#' @param .required Logical; if TRUE, field must be present (default FALSE)
#' @param .default Default value to use when field is missing. Only applies to
#'   required fields (.required = TRUE)
#' @param .format Format string(s) for parsing dates/timestamps (json_date/json_timestamp only)
#' @param .tz Timezone to use for parsing timestamps (json_timestamp only). Defaults to "UTC"
#' @return A schema definition object
#' @name schema
#' @rdname schema
#' @seealso [repair_json_str()], [repair_json_file()], [repair_json_raw()], [repair_json_conn()], [json_schema()]
#' @export
#' @examples
#' # Basic types
#' json_string()
#' json_integer()
#' json_number()
#' json_boolean()
#' json_any()
#'
#' # Object with fields
#' schema <- json_object(
#'   name = json_string(),
#'   age = json_integer(),
#'   email = json_string()
#' )
#'
#' # Array of integers
#' json_array(json_integer())
#'
#' # Enum with allowed values
#' json_enum(c("active", "inactive", "pending"))
#'
#' # Optional fields with defaults
#' json_object(
#'   name = json_string(.required = TRUE),
#'   age = json_integer(.default = 0L),
#'   active = json_boolean(.default = TRUE, .required = TRUE),
#'   status = json_enum(c("active", "inactive"), .required = TRUE)
#' )
#'
#' # Date and timestamp handling
#' json_object(
#'   birthday = json_date(.format = "us_date"),
#'   created_at = json_timestamp(.format = "iso8601z", .tz = "UTC")
#' )
json_object <- function(..., .required = FALSE) {
  fields <- list(...)

  if (length(fields) == 0) {
    stop("json_object requires at least one field")
  }

  if (is.null(names(fields)) || any(names(fields) == "")) {
    stop("All fields in json_object must be named")
  }

  structure(
    list(
      type = "object",
      fields = fields,
      required = .required
    ),
    class = "LLMJsonSchema"
  )
}

#' @rdname schema
#' @export
json_integer <- function(.default = 0L, .required = FALSE) {
  schema <- list(
    type = "integer",
    required = .required
  )

  if (!is.numeric(.default) || length(.default) != 1) {
    stop(".default for json_integer must be a single numeric value")
  }
  schema$default <- as.integer(.default)

  structure(schema, class = "LLMJsonSchema")
}

#' @rdname schema
#' @export
json_number <- function(.default = 0.0, .required = FALSE) {
  schema <- list(
    type = "number",
    required = .required
  )

  if (!is.numeric(.default) || length(.default) != 1) {
    stop(".default for json_number must be a single numeric value")
  }
  schema$default <- as.numeric(.default)

  structure(schema, class = "LLMJsonSchema")
}

#' @rdname schema
#' @export
json_string <- function(.default = "", .required = FALSE) {
  schema <- list(
    type = "string",
    required = .required
  )

  if (!is.character(.default) || length(.default) != 1) {
    stop(".default for json_string must be a single character value")
  }
  schema$default <- .default

  structure(schema, class = "LLMJsonSchema")
}

#' @rdname schema
#' @export
json_boolean <- function(.default = FALSE, .required = FALSE) {
  schema <- list(
    type = "boolean",
    required = .required
  )

  if (!is.logical(.default) || length(.default) != 1) {
    stop(".default for json_boolean must be a single logical value")
  }
  schema$default <- .default

  structure(schema, class = "LLMJsonSchema")
}

#' @rdname schema
#' @param .values Character vector of allowed values (json_enum only)
#' @export
json_enum <- function(.values, .default = .values[1], .required = FALSE) {
  if (!is.character(.values) || length(.values) == 0) {
    stop(".values must be a non-empty character vector")
  }

  schema <- list(
    type = "enum",
    values = .values,
    required = .required
  )

  if (!is.character(.default) || length(.default) != 1) {
    stop(".default for json_enum must be a single character value")
  }
  if (!.default %in% .values) {
    stop(".default must be one of the allowed values")
  }
  schema$default <- .default

  structure(schema, class = "LLMJsonSchema")
}

#' @rdname schema
#' @export
json_array <- function(items, .required = FALSE) {
  if (!inherits(items, "LLMJsonSchema")) {
    stop("items must be a schema definition created with json_* functions")
  }

  structure(
    list(
      type = "array",
      items = items,
      required = .required
    ),
    class = "LLMJsonSchema"
  )
}

#' @rdname schema
#' @export
json_any <- function(.required = FALSE) {
  structure(
    list(
      type = "any",
      required = .required
    ),
    class = "LLMJsonSchema"
  )
}

#' @rdname schema
#' @export
json_date <- function(.default = NULL, .format = "iso8601", .required = FALSE) {
  schema <- list(
    type = "date",
    required = .required
  )

  if (!is.null(.default)) {
    if (!inherits(.default, "Date") || length(.default) != 1) {
      stop(".default for json_date must be a single Date object")
    }
    # Convert to string in ISO format for storage
    schema$default <- as.character(.default)
  }

  if (!is.character(.format)) {
    stop(".format for json_date must be a character vector")
  }
  schema$format <- .format

  structure(schema, class = "LLMJsonSchema")
}

#' @rdname schema
#' @export
json_timestamp <- function(
  .default = NULL,
  .format = "iso8601",
  .tz = "UTC",
  .required = FALSE
) {
  schema <- list(
    type = "timestamp",
    required = .required
  )

  if (!is.null(.default)) {
    if (!inherits(.default, "POSIXct") || length(.default) != 1) {
      stop(".default for json_timestamp must be a single POSIXct object")
    }
    # Convert to numeric Unix timestamp for storage
    schema$default <- as.numeric(.default)
  }

  if (!is.character(.format)) {
    stop(".format for json_timestamp must be a character vector")
  }
  schema$format <- .format

  if (!is.character(.tz) || length(.tz) != 1) {
    stop(".tz for json_timestamp must be a single character value")
  }
  schema$tz <- .tz

  structure(schema, class = "LLMJsonSchema")
}

#' @export
print.LLMJsonSchema <- function(x, ...) {
  # Build schema and use Rust implementation for better performance
  built <- json_schema(x)
  cat(built$format(), "\n", sep = "")
  invisible(x)
}


#' Build a compiled schema for efficient reuse
#'
#' This function compiles a schema definition into an efficient internal
#' representation that can be reused across multiple JSON repair operations.
#' This dramatically improves performance when repairing many JSON strings
#' with the same schema, as the schema only needs to be parsed once.
#'
#' The function is a generic that supports:
#' - **LLMJsonSchema objects**: Created with `json_object()`,
#'   `json_integer()`, etc.
#' - **ellmer Type objects**: Automatically converted from ellmer's
#'   type system (requires ellmer package)
#'
#' @param schema A schema definition. Can be:
#'   - An LLMJsonSchema object created with json_object(), json_integer(), etc.
#'   - An ellmer Type object (TypeBasic, TypeEnum, TypeArray, TypeObject, etc.)
#' @param ... Additional arguments passed to methods
#' @return A LLMJsonSchemaBuilt object (external pointer) that can be passed to
#'   repair_json_str(), repair_json_file(), or repair_json_raw()
#' @seealso [repair_json_str()], [repair_json_file()],
#'   [repair_json_raw()], [repair_json_conn()], [schema()]
#' @export
#' @examples
#' # Create a schema using llmjson functions
#' schema <- json_object(
#'   name = json_string(),
#'   age = json_integer(),
#'   email = json_string()
#' )
#'
#' # Build it once
#' built_schema <- json_schema(schema)
#'
#' # Reuse many times - much faster than rebuilding each time!
#' repair_json_str('{"name": "Alice", "age": 30}', built_schema)
#' repair_json_str('{"name": "Bob", "age": 25}', built_schema)
#'
#' \dontrun{
#' # Convert from ellmer types (requires ellmer package)
#' library(ellmer)
#'
#' user_type <- type_object(
#'   name = type_string(required = TRUE),
#'   age = type_integer(),
#'   status = type_enum(c("active", "inactive"), required = TRUE)
#' )
#'
#' # Automatically converts ellmer type to llmjson schema
#' built_schema <- json_schema(user_type)
#'
#' repair_json_str(
#'   '{"name": "Alice", "age": 30, "status": "active"}',
#'   schema = built_schema,
#'   return_objects = TRUE
#' )
#' }
json_schema <- function(schema, ...) {
  UseMethod("json_schema")
}

#' @rdname json_schema
#' @export
json_schema.LLMJsonSchema <- function(schema, ...) {
  LLMJsonSchemaBuilt$new(schema)
}

#' @rdname json_schema
#' @export
`json_schema.ellmer::Type` <- function(schema, ...) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required to convert ellmer types")
  }

  # Convert ellmer Type to LLMJsonSchema
  llmjson_schema <- convert_ellmer_type(schema)

  # Build and return
  LLMJsonSchemaBuilt$new(llmjson_schema)
}

#' Convert ellmer Type to LLMJsonSchema
#' @noRd
convert_ellmer_type <- function(type) {
  required <- isTRUE(type@required)

  switch(
    class(type)[1],
    "ellmer::TypeBasic" = convert_type_basic(type, required),
    "ellmer::TypeEnum" = convert_type_enum(type, required),
    "ellmer::TypeArray" = convert_type_array(type, required),
    "ellmer::TypeObject" = convert_type_object(type, required),
    "ellmer::TypeJsonSchema" = convert_type_json_schema(type, required),
    "ellmer::TypeIgnore" = NULL,
    stop("Unsupported ellmer type: ", paste(class(type), collapse = ", "))
  )
}

#' @noRd
convert_type_basic <- function(type, required) {
  type_name <- type@type

  switch(
    type_name,
    "boolean" = json_boolean(.required = required),
    "integer" = json_integer(.required = required),
    "number" = json_number(.required = required),
    "string" = json_string(.required = required),
    stop("Unknown TypeBasic type: ", type_name)
  )
}

#' @noRd
convert_type_enum <- function(type, required) {
  values <- type@values

  if (!is.character(values) || length(values) == 0) {
    stop("TypeEnum must have character values")
  }

  json_enum(.values = values, .required = required)
}

#' @noRd
convert_type_array <- function(type, required) {
  items_type <- type@items

  # Recursively convert the items type
  items_schema <- convert_ellmer_type(items_type)

  if (is.null(items_schema)) {
    stop("Array items type cannot be TypeIgnore")
  }

  json_array(items = items_schema, .required = required)
}

#' @noRd
convert_type_object <- function(type, required) {
  properties <- type@properties

  if (!is.list(properties) || is.null(names(properties))) {
    stop("TypeObject must have named properties list")
  }

  # Convert each property, filtering out TypeIgnore
  fields <- list()
  for (name in names(properties)) {
    prop_schema <- convert_ellmer_type(properties[[name]])

    # Skip TypeIgnore fields
    if (!is.null(prop_schema)) {
      fields[[name]] <- prop_schema
    }
  }

  if (length(fields) == 0) {
    stop("TypeObject must have at least one non-ignored field")
  }

  do.call(json_object, c(fields, list(.required = required)))
}

#' @noRd
convert_type_json_schema <- function(type, required) {
  # TypeJsonSchema contains raw JSON schema
  # We could try to parse it, but for now just error
  stop(
    "TypeJsonSchema is not directly supported. ",
    "Please convert to specific ellmer types (TypeBasic, TypeEnum, TypeArray, TypeObject) first."
  )
}

#' @export
print.LLMJsonSchemaBuilt <- function(x, ...) {
  cat(x$format(), "\n", sep = "")
  invisible(x)
}
