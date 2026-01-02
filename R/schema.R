#' Schema builders for JSON repair and validation
#'
#' These functions create schema definitions that guide JSON repair and
#' conversion to R objects. Schemas ensure that the repaired JSON conforms
#' to expected types and structure.
#'
#' @name schema_builders
#' @rdname schema_builders
NULL

#' Build a compiled schema for efficient reuse
#'
#' This function compiles a schema definition into an efficient internal
#' representation that can be reused across multiple JSON repair operations.
#' This dramatically improves performance when repairing many JSON strings
#' with the same schema, as the schema only needs to be parsed once.
#'
#' @param schema A schema definition created with s_map(), s_integer(), etc.
#' @return A built schema object (external pointer) that can be passed to
#'   repair_json_str(), repair_json_file(), or repair_json_raw()
#' @export
#' @examples
#' # Create a schema
#' schema <- s_map(
#'   name = s_string(),
#'   age = s_integer(),
#'   email = s_string()
#' )
#'
#' # Build it once
#' built_schema <- build_schema(schema)
#'
#' # Reuse many times - much faster than rebuilding each time!
#' repair_json_str('{"name": "Alice", "age": 30}', built_schema)
#' repair_json_str('{"name": "Bob", "age": 25}', built_schema)
build_schema <- function(schema) {
  if (!inherits(schema, "llmjson_schema")) {
    stop("schema must be a schema definition created with s_* functions")
  }
  BuiltSchema$new(schema)
}

#' Print method for built schema objects
#' @param x A built schema object
#' @param ... Additional arguments (ignored)
#' @export
print.BuiltSchema <- function(x, ...) {
  x$print()
  invisible(x)
}

#' Create a schema for a JSON object/map
#'
#' @param ... Named arguments defining the schema for each field.
#'   Each argument should be a schema builder (s_integer, s_double, s_string, etc.)
#' @param .optional Logical; if TRUE, this object can be null
#' @return A schema definition object
#' @export
#' @examples
#' # Define a schema for a user object
#' schema <- s_map(
#'   name = s_string(),
#'   age = s_integer(),
#'   email = s_string()
#' )
s_map <- function(..., .optional = FALSE) {
  fields <- list(...)

  if (length(fields) == 0) {
    stop("s_map requires at least one field")
  }

  if (is.null(names(fields)) || any(names(fields) == "")) {
    stop("All fields in s_map must be named")
  }

  structure(
    list(
      type = "map",
      fields = fields,
      optional = .optional
    ),
    class = "llmjson_schema"
  )
}

#' Create a schema for an integer value
#'
#' @param .optional Logical; if TRUE, this value can be null and a missing field
#'   will not cause an error
#' @param .default Default value to use when field is missing and .optional = TRUE.
#'   Must be a single integer value (e.g., 0L, 1L)
#' @return A schema definition object
#' @export
#' @examples
#' s_integer()
#' s_integer(.optional = TRUE)
#' s_integer(.optional = TRUE, .default = 0L)
s_integer <- function(.optional = FALSE, .default = NULL) {
  schema <- list(
    type = "integer",
    optional = .optional
  )

  if (!is.null(.default)) {
    if (!is.numeric(.default) || length(.default) != 1) {
      stop(".default for s_integer must be a single numeric value")
    }
    schema$default <- as.integer(.default)
  }

  structure(schema, class = "llmjson_schema")
}

#' Create a schema for a double/numeric value
#'
#' @param .optional Logical; if TRUE, this value can be null and a missing field
#'   will not cause an error
#' @param .default Default value to use when field is missing and .optional = TRUE.
#'   Must be a single numeric value (e.g., 0.0, 1.5)
#' @return A schema definition object
#' @export
#' @examples
#' s_double()
#' s_double(.optional = TRUE)
#' s_double(.optional = TRUE, .default = 0.0)
s_double <- function(.optional = FALSE, .default = NULL) {
  schema <- list(
    type = "double",
    optional = .optional
  )

  if (!is.null(.default)) {
    if (!is.numeric(.default) || length(.default) != 1) {
      stop(".default for s_double must be a single numeric value")
    }
    schema$default <- as.numeric(.default)
  }

  structure(schema, class = "llmjson_schema")
}

#' Create a schema for a string value
#'
#' @param .optional Logical; if TRUE, this value can be null and a missing field
#'   will not cause an error
#' @param .default Default value to use when field is missing and .optional = TRUE.
#'   Must be a single character value (e.g., "", "unknown")
#' @return A schema definition object
#' @export
#' @examples
#' s_string()
#' s_string(.optional = TRUE)
#' s_string(.optional = TRUE, .default = "")
s_string <- function(.optional = FALSE, .default = NULL) {
  schema <- list(
    type = "string",
    optional = .optional
  )

  if (!is.null(.default)) {
    if (!is.character(.default) || length(.default) != 1) {
      stop(".default for s_string must be a single character value")
    }
    schema$default <- .default
  }

  structure(schema, class = "llmjson_schema")
}

#' Create a schema for a logical/boolean value
#'
#' @param .optional Logical; if TRUE, this value can be null and a missing field
#'   will not cause an error
#' @param .default Default value to use when field is missing and .optional = TRUE.
#'   Must be a single logical value (TRUE or FALSE)
#' @return A schema definition object
#' @export
#' @examples
#' s_logical()
#' s_logical(.optional = TRUE)
#' s_logical(.optional = TRUE, .default = FALSE)
s_logical <- function(.optional = FALSE, .default = NULL) {
  schema <- list(
    type = "logical",
    optional = .optional
  )

  if (!is.null(.default)) {
    if (!is.logical(.default) || length(.default) != 1) {
      stop(".default for s_logical must be a single logical value")
    }
    schema$default <- .default
  }

  structure(schema, class = "llmjson_schema")
}

#' Create a schema for an array/vector
#'
#' @param items Schema definition for array elements
#' @param .optional Logical; if TRUE, this array can be null
#' @return A schema definition object
#' @export
#' @examples
#' # Array of integers
#' s_array(s_integer())
#'
#' # Array of user objects
#' s_array(s_map(
#'   name = s_string(),
#'   age = s_integer()
#' ))
s_array <- function(items, .optional = FALSE) {
  if (!inherits(items, "llmjson_schema")) {
    stop("items must be a schema definition created with s_* functions")
  }

  structure(
    list(
      type = "array",
      items = items,
      optional = .optional
    ),
    class = "llmjson_schema"
  )
}

#' Create a schema that allows any type
#'
#' @param .optional Logical; if TRUE, this value can be null
#' @return A schema definition object
#' @export
#' @examples
#' s_any()
s_any <- function(.optional = FALSE) {
  structure(
    list(
      type = "any",
      optional = .optional
    ),
    class = "llmjson_schema"
  )
}

#' Print method for schema objects
#' @param x A schema object
#' @param ... Additional arguments (ignored)
#' @param indent Current indentation level
#' @export
print.llmjson_schema <- function(x, ..., indent = 0) {
  padding <- strrep("  ", indent)

  if (x$type == "map") {
    cat(padding, "{\n", sep = "")
    field_names <- names(x$fields)
    for (i in seq_along(field_names)) {
      name <- field_names[i]
      field <- x$fields[[name]]
      cat(padding, '  "', name, '": ', sep = "")

      if (inherits(field, "llmjson_schema")) {
        if (field$type %in% c("map", "array")) {
          cat("\n")
          print(field, indent = indent + 1)
        } else {
          # Inline simple types
          type_str <- paste0('"', field$type, '"')
          if (field$optional) {
            type_str <- paste0(type_str, " (optional)")
          }
          if (!is.null(field$default)) {
            default_val <- if (is.character(field$default)) {
              paste0('"', field$default, '"')
            } else {
              as.character(field$default)
            }
            type_str <- paste0(type_str, " [default: ", default_val, "]")
          }
          cat(type_str)
        }
      }

      if (i < length(field_names)) {
        cat(",")
      }
      cat("\n")
    }
    cat(padding, "}", sep = "")
  } else if (x$type == "array") {
    cat(padding, "[\n", sep = "")
    print(x$items, indent = indent + 1)
    cat("\n", padding, "]", sep = "")
  } else {
    # Should not reach here in normal usage
    type_str <- paste0('"', x$type, '"')
    if (x$optional) {
      type_str <- paste0(type_str, " (optional)")
    }
    cat(padding, type_str, sep = "")
  }

  if (indent == 0) {
    cat("\n")
  }

  invisible(x)
}
