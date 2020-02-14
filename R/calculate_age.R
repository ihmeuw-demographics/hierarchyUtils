#' Calculates the end of each age group given a series of start values for each age group
#'
#' Creates a new column in the input data.table called 'age_end'.
#'
#' 'age_start' is inclusive while 'age_end' exclusive. So for example age group
#' \eqn{a} with 'age_start' equal to 5 and 'age_end' equal to 10 spans
#' \eqn{5 <= a < 10}.
#'
#' This function assumes that only the most detailed age groups are in the input
#' dataset.
#'
#' @param dt data.table with age-specific data.
#' * must contain columns specified in `id_cols`.
#' * must include a numeric column called 'age_start' that contains no missing
#' values.
#' * each combination of `id_cols` must uniquely identify each row.
#' @param id_cols character vector of id columns that uniquely identify each row
#' of `dt`.
#' * must include 'age_start'.
#' @param terminal_age_end the numeric 'age_end' value for the terminal age
#' group.
#'
#' @return modifies `dt` in place by creating a new numeric column called
#' 'age_end'.
#'
#' @export
#'
#' @examples
#' input_dt <- data.table::data.table(location = "France", year = 2010,
#'                                    sex = "female",
#'                                    age_start = 0:95,
#'                                    value1 = 1, value2 = 2)
#' id_cols <- c("location", "year", "sex", "age_start")
#' value_cols <- c("value1", "value2")
#' calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
calculate_age_end <- function(dt, id_cols, terminal_age_end = 125L) {

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  assertthat::assert_that("age_start" %in% id_cols, msg = "`id_cols` must include 'age_start'.")

  # check `terminal_age_end` argument
  assertive::assert_is_numeric(terminal_age_end)

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  capture.output(assertable::assert_colnames(dt, id_cols, only_colnames = F))
  assertive::assert_is_numeric(dt[["age_start"]])
  assertive::assert_all_are_not_na(dt[["age_start"]])
  assert_is_unique_dt(dt, id_cols)

  setkeyv(dt, id_cols)
  by_id_cols <- id_cols[!id_cols %in% "age_start"]
  dt[, age_end := data.table::shift(age_start, type = "lead", fill = terminal_age_end),
     by = by_id_cols]
  setkeyv(dt, c(id_cols, "age_end"))

  return(invisible(NULL))
}

#' Calculates the size of each age group given a start and end age for each age group
#'
#' @param dt data.table with age-specific data.
#' * must contain numeric columns called 'age_start' and 'age_end'.
#'
#' @return modifies `dt` in place by creating a new numeric column called
#' 'age_int'.
#'
#' @export
#'
#' @examples
#' input_dt <- data.table::data.table(location = "France", year = 2010,
#'                                    sex = "female",
#'                                    age_start = 0:95,
#'                                    age_end = c(1:95, 125),
#'                                    value1 = 1, value2 = 2)
#' calculate_age_int(input_dt)
calculate_age_int <- function(dt) {

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  capture.output(assertable::assert_colnames(dt, c("age_start", "age_end"), only_colnames = F))
  assertive::assert_is_numeric(dt[["age_start"]])
  assertive::assert_all_are_not_na(dt[["age_start"]])
  assertive::assert_is_numeric(dt[["age_end"]])

  dt[, age_int := age_end - age_start]

  return(invisible(NULL))
}
