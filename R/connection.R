#' Repair malformed JSON from a connection
#'
#' This function reads JSON from an R connection (such as a file, URL, or pipe)
#' and repairs it. The connection is read and the content is passed to
#' \code{repair_json_str()} for repair.
#'
#' @param conn A connection object (e.g., from \code{file()}, \code{url()}, \code{gzfile()}, etc.)
#' @param schema Optional schema definition for validation and type conversion
#' @param return_objects Logical indicating whether to return R objects (TRUE) or JSON string (FALSE, default)
#' @param ensure_ascii Logical; if TRUE, escape non-ASCII characters
#' @param int64 Policy for handling 64-bit integers: "double" (default, may lose precision), "string" (preserves exact value), or "bit64" (requires bit64 package)
#' @return A character string containing the repaired JSON, or an R object if return_objects is TRUE
#' @seealso \code{\link{repair_json_str}}, \code{\link{repair_json_file}}, \code{\link{repair_json_raw}}, \code{\link{repair_json_conn}}, \code{\link{repair_json_raw}}, \code{\link{schema}}, \code{\link{json_schema}}
#' @export
#' @examples
#' \dontrun{
#' # Read from a file connection
#' conn <- file("malformed.json", "r")
#' result <- repair_json_conn(conn)
#' close(conn)
#'
#' # Read from a URL
#' conn <- url("https://example.com/data.json")
#' result <- repair_json_conn(conn, return_objects = TRUE)
#' close(conn)
#'
#' # Read from a compressed file
#' conn <- gzfile("data.json.gz", "r")
#' result <- repair_json_conn(conn, return_objects = TRUE, int64 = "string")
#' close(conn)
#'
#' # Or use with() to ensure connection is closed
#' result <- with(file("malformed.json", "r"), repair_json_conn(conn))
#' }
repair_json_conn <- function(
  conn,
  schema = NULL,
  return_objects = FALSE,
  ensure_ascii = TRUE,
  int64 = "double"
) {
  # Check if conn is actually a connection
  if (!inherits(conn, "connection")) {
    stop(
      "'conn' must be a connection object (e.g., from file(), url(), gzfile())"
    )
  }

  mode <- summary.connection(conn)$mode
  if (mode == "r") {
    return(repair_json_str(
      json_str = paste(readLines(conn, warn = FALSE), collapse = ""),
      schema = schema,
      return_objects = return_objects,
      ensure_ascii = ensure_ascii,
      int64 = int64
    ))
  }

  if (mode == "rb") {
    chunk_size <- 1048576L # 1MB initial chunk
    result <- raw()
    repeat {
      chunk <- readBin(conn, raw(), chunk_size)
      result <- c(result, chunk)
      if (length(chunk) < chunk_size) {
        break
      }
      chunk_size <- chunk_size * 2
    }

    return(repair_json_raw(
      raw_bytes = result,
      schema = schema,
      return_objects = return_objects,
      ensure_ascii = ensure_ascii,
      int64 = int64
    ))
  }

  stop("Connection must be opened in read ('r') or binary ('rb') mode")
}
