#' Are the rows of the input data.table unique?
#'
#' Checks to see if the rows of the input data.table are unique for each
#' combination of the id columns.
#'
#' @param dt input data.table to check.
#' @param id_cols character vector of id columns that uniquely identify each row
#' of `dt`.
#' @inheritParams assertive.types::assert_is_data.table
#'
#' @return
#' * `is_unique_dt` returns a logical.
#' * `assert_is_unique_dt` returns nothing but throws an error if `is_unique_dt`
#' returns FALSE.
#'
#' @export
#'
#' @examples
#' input_dt <- data.table::data.table(location = "France", year = 2010,
#'                                    sex = "female",
#'                                    age_start = 0:95,
#'                                    value1 = 2, value2 = 4)
#' id_cols <- c("location", "year", "sex", "age_start")
#' is_unique_dt(input_dt, id_cols)
#' assert_is_unique_dt(input_dt, id_cols)

assert_is_unique_dt <- function(dt, id_cols, severity = getOption("assertive.severity", "stop")) {
  assertive::assert_engine(is_unique_dt, dt, id_cols,
                           severity = severity)
}

#' @rdname assert_is_unique_dt
#' @export
is_unique_dt <- function(dt, id_cols) {
  # check `id_cols` argument
  assertive::assert_is_character(id_cols)

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  capture.output(assertable::assert_colnames(dt, id_cols, only_colnames = F))

  # count rows for each combination `id_cols`
  check_unique_dt <- dt[, list(check = .N), by = id_cols]
  check_unique_dt <- check_unique_dt[check > 1]

  return(!nrow(check_unique_dt) > 0)
}
