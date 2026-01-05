#' Schema builders for JSON repair and validation
#'
#' These functions create schema definitions that guide JSON repair and
#' conversion to R objects. Schemas ensure that the repaired JSON conforms
#' to expected types and structure.
#'
#' @param ... Named arguments defining the schema for each field (json_object only)
#' @param items Schema definition for array elements (json_array only)
#' @param .optional Logical; if TRUE, will use default value.
#' @param .default Default value to use when field is missing. Only applies to
#'   required fields (.optional = FALSE)
#' @param .format Format string(s) for parsing dates/timestamps (json_date/json_timestamp only)
#' @param .tz Timezone to use for parsing timestamps (json_timestamp only). Defaults to "UTC"
#' @return A schema definition object
#' @name schema
#' @rdname schema
#' @seealso \code{\link{repair_json_str}}, \code{\link{repair_json_file}}, \code{\link{repair_json_raw}}, \code{\link{repair_json_conn}}, \code{\link{repair_json_raw}}, #' @seealso \code{\link{repair_json_str}}, \code{\link{repair_json_file}}, \code{\link{repair_json_raw}}, \code{\link{repair_json_conn}}, \code{\link{repair_json_raw}}, #' @seealso \code{\link{repair_json_str}}, \code{\link{repair_json_file}}, \code{\link{repair_json_raw}}, \code{\link{repair_json_conn}}, \code{\link{repair_json_raw}}, \code{\link{json_schema}}
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
#' # Optional fields with defaults
#' json_object(
#'   name = json_string(),
#'   age = json_integer(.default = 0L, .optional = TRUE),
#'   active = json_boolean(.default = TRUE)
#' )
#'
#' # Date and timestamp handling
#' json_object(
#'   birthday = json_date(.format = "us_date"),
#'   created_at = json_timestamp(.format = "iso8601z", .tz = "UTC")
#' )
json_object <- function(..., .optional = FALSE) {
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
      optional = .optional
    ),
    class = "LLMJsonSchema"
  )
}

#' @rdname schema
#' @export
json_integer <- function(.default = 0L, .optional = FALSE) {
  schema <- list(
    type = "integer",
    optional = .optional
  )

  if (!is.numeric(.default) || length(.default) != 1) {
    stop(".default for json_integer must be a single numeric value")
  }
  schema$default <- as.integer(.default)

  structure(schema, class = "LLMJsonSchema")
}

#' @rdname schema
#' @export
json_number <- function(.default = 0.0, .optional = FALSE) {
  schema <- list(
    type = "number",
    optional = .optional
  )

  if (!is.numeric(.default) || length(.default) != 1) {
    stop(".default for json_number must be a single numeric value")
  }
  schema$default <- as.numeric(.default)

  structure(schema, class = "LLMJsonSchema")
}

#' @rdname schema
#' @export
json_string <- function(.default = "", .optional = FALSE) {
  schema <- list(
    type = "string",
    optional = .optional
  )

  if (!is.character(.default) || length(.default) != 1) {
    stop(".default for json_string must be a single character value")
  }
  schema$default <- .default

  structure(schema, class = "LLMJsonSchema")
}

#' @rdname schema
#' @export
json_boolean <- function(.default = FALSE, .optional = FALSE) {
  schema <- list(
    type = "boolean",
    optional = .optional
  )

  if (!is.logical(.default) || length(.default) != 1) {
    stop(".default for json_boolean must be a single logical value")
  }
  schema$default <- .default

  structure(schema, class = "LLMJsonSchema")
}

#' @rdname schema
#' @export
json_array <- function(items, .optional = FALSE) {
  if (!inherits(items, "LLMJsonSchema")) {
    stop("items must be a schema definition created with json_* functions")
  }

  structure(
    list(
      type = "array",
      items = items,
      optional = .optional
    ),
    class = "LLMJsonSchema"
  )
}

#' @rdname schema
#' @export
json_any <- function(.optional = FALSE) {
  structure(
    list(
      type = "any",
      optional = .optional
    ),
    class = "LLMJsonSchema"
  )
}

#' @rdname schema
#' @export
json_date <- function(.default = NULL, .format = "iso8601", .optional = FALSE) {
  schema <- list(
    type = "date",
    optional = .optional
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
  .optional = FALSE
) {
  schema <- list(
    type = "timestamp",
    optional = .optional
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
#' @param schema A schema definition created with json_object(), json_integer(), etc.
#' @return A built schema object (external pointer) that can be passed to
#'   repair_json_str(), repair_json_file(), or repair_json_raw()
#' @seealso \code{\link{repair_json_str}}, \code{\link{repair_json_file}}, \code{\link{repair_json_raw}}, \code{\link{repair_json_conn}}, \code{\link{repair_json_raw}}, \code{\link{schema}}
#' @export
#' @examples
#' # Create a schema
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
json_schema <- function(schema) {
  if (!inherits(schema, "LLMJsonSchema")) {
    stop("schema must be a schema definition created with json_* functions")
  }
  LLMJsonSchemaBuilt$new(schema)
}

#' @export
print.LLMJsonSchemaBuilt <- function(x, ...) {
  cat(x$format(), "\n", sep = "")
  invisible(x)
}
