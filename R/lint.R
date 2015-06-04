#' @export
lint <- function(filename, line_number = 1L, column_number = NULL, type = "style", message = "", line = "", ranges = NULL) {
  structure(
    list(
      filename = filename,
      line_number = line_number,
      column_number = column_number %||% 1L,
      type = type,
      message = message,
      line = line,
      ranges = ranges
      ),
    class="lint")
}

#' @export
print.lint <- function(x, ...) {

  color <- switch(x$type,
    "warning" = magenta,
    "error" = red,
    "style" = blue,
    bold
  )

  message(
    bold(x$filename, ":",
    as.character(x$line_number), ":",
    as.character(x$column_number), ": ", sep = ""),
    color(x$type, ": ", sep = ""),
    bold(x$message), "\n",
    x$line, "\n",
    highlight_string(x$column_number, x$ranges)
    )
}

highlight_string <- function(column_number = NULL, ranges = NULL) {
  maximum = max(column_number, unlist(ranges))

  line <- fill_with(" ", maximum)

  lapply(ranges, function(range) {
    substr(line, range[1], range[2]) <<- fill_with("~", range[2] - range[1] + 1L)
    })

  substr(line, column_number, column_number + 1L) <- "^"

  line
}

fill_with <- function(character = " ", length = 1L) {
  paste0(collapse = "", rep.int(character, length))
}
