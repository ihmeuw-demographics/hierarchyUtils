#' Are the rows of the input data.table unique?
#'
#' Checks to see if the rows of the input data.table are unique for each
#' combination of the id columns.
#'
#' @param dt input data.table to check.
#' * must contain columns specified in `id_cols`.
#' @param id_cols character vector of id columns that uniquely identify each row
#' of `dt`.
#'
#' @return
#' * `identify_non_unique_dt` returns a data.table with problematic rows only,
#' includes a 'count' column identifying how many times each combination of id
#' columns is in the dataset
#' * `assert_is_unique_dt` returns nothing but throws an error if `identify_non_unique_dt`
#' returns a non empty data.table.
#'
#' @export
#'
#' @seealso `assertable::assert_ids` to check that all unique combinations of
#' specified id variables list, this is appropriate when you expect your dataset
#' to be square.
#'
#' @examples
#' input_dt <- data.table::data.table(location = "France", year = 2010,
#'                                    sex = "female",
#'                                    age_start = 0:95,
#'                                    value1 = 2, value2 = 4)
#' id_cols <- c("location", "year", "sex", "age_start")
#' non_unique_dt <- identify_non_unique_dt(input_dt, id_cols)
#' assert_is_unique_dt(input_dt, id_cols)
assert_is_unique_dt <- function(dt, id_cols) {

  non_unique_dt <- identify_non_unique_dt(dt, id_cols)
  is_unique_dt <- nrow(non_unique_dt) == 0

  error_msg <- "Input data rows are not unique for each combination of the id columns. Use `identify_non_unique_dt` to see which data is problematic."
  assertthat::assert_that(is_unique_dt, msg = error_msg)
}

#' @rdname assert_is_unique_dt
#' @export
identify_non_unique_dt <- function(dt, id_cols) {

  # Validate arguments ------------------------------------------------------

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  capture.output(assertable::assert_colnames(dt, id_cols, only_colnames = F))

  # Count number of rows in each combination of `id_cols` -------------------

  check_unique_dt <- dt[, list(check = .N), by = id_cols]
  check_unique_dt <- check_unique_dt[check > 1]

  setcolorder(check_unique_dt, c(id_cols, "check"))
  setkeyv(check_unique_dt, c(id_cols, "check"))
  return(check_unique_dt)
}
