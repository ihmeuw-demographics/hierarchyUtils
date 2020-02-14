#' Are the age related columns formatted correctly?
#'
#' Checks to see that each set of age groups for each combination of the id
#' columns is formatted correctly. This includes checking that:
#' * the age groups are contiguous.
#' * there are no overlapping age groups.
#' * the youngest age group starts at age `first_age_start`.
#' * the oldest age group ends at `terminal_age_end`.
#'
#' If `assert_age_formatted_dt` throws an error indicating the dataset is not
#' formatted correctly, you can use `identify_age_misformatted_dt` to see the rows
#' of the dataset that are problematic.
#'
#' @param dt input data.table to check.
#' * must contain columns specified in `id_cols`.
#' * must include numeric columns called 'age_start' and 'age_end' that contain
#' no missing values.
#' * each combination of `id_cols` must uniquely identify each row.
#' @param id_cols character vector of id columns that uniquely identify each row
#' of `dt`.
#' * must include 'age_start' and 'age_end'.
#' @param first_age_start the numeric 'age_start' value for the first age group.
#' @inheritParams calculate_age_end
#'
#' @return
#' * `identify_age_misformatted_dt` returns a data.table with problematic age
#' groups only, includes a column 'error' that identifies 'missing' or
#' 'overlapping' age ranges.
#' * `assert_age_formatted_dt` returns nothing but throws an error if
#' `identify_age_misformatted_dt` returns a non empty data.table.
#'
#' @export
#'
#' @examples
#' # 5 year age groups with 7-9 age range accidentally included
#' input_dt <- data.table::data.table(year = 2016,
#'                                    age_start = seq(0, 95, 5),
#'                                    age_end = c(seq(5, 95, 5), 125),
#'                                    value = 1)
#' temp <- data.table::data.table(year = 2016, age_start = 7, age_end = 9, value = 1)
#' input_dt <- rbind(input_dt, temp, use.names = T)
#' id_cols <- c("year", "age_start", "age_end")
#' misformatted_dt <- identify_age_misformatted_dt(input_dt, id_cols)
#'
#' \dontrun{
#' assert_age_formatted_dt(input_dt, id_cols)}
#'
assert_age_formatted_dt <- function(dt, id_cols, first_age_start = 0, terminal_age_end = 125) {

  age_format_errors_dt <- identify_age_misformatted_dt(dt, id_cols, first_age_start, terminal_age_end)
  is_age_formatted <- nrow(age_format_errors_dt) == 0

  error_msg <- "Input data 'age_start' and 'age_end' columns are not properly formatted. Use `identify_age_misformatted_dt` to see which data is problematic."
  assertthat::assert_that(is_age_formatted, msg = error_msg)
}

#' @rdname assert_age_formatted_dt
#' @export
identify_age_misformatted_dt <- function(dt, id_cols, first_age_start = 0, terminal_age_end = 125) {

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  assertthat::assert_that("age_start" %in% id_cols, msg = "`id_cols` must include 'age_start'.")
  assertthat::assert_that("age_end" %in% id_cols, msg = "`id_cols` must include 'age_start'.")

  # check `first_age_start` argument
  assertive::assert_is_numeric(first_age_start)

  # check `terminal_age_end` argument
  assertive::assert_is_numeric(terminal_age_end)

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  capture.output(assertable::assert_colnames(dt, id_cols, only_colnames = F))
  assertive::assert_is_numeric(dt[["age_start"]])
  assertive::assert_all_are_not_na(dt[["age_start"]])
  assertive::assert_is_numeric(dt[["age_end"]])
  assertive::assert_all_are_not_na(dt[["age_end"]])
  assert_is_unique_dt(dt, id_cols)

  setkeyv(dt, id_cols)
  by_id_cols <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # determine if any age aggregate age groups are included in the dtset and drop them if they exist
  check_overlapping_ages <- dt[, list(check_ages = list(sort(unique(c(age_start, age_end))))), by = by_id_cols]
  dt <- merge(dt, check_overlapping_ages, by = by_id_cols, all = T)
  dt[, overlapping := any(between(check_ages[[1]], age_start, age_end, incbounds = F)), by = id_cols]
  overlapping_age_groups <- dt[(overlapping)]
  overlapping_age_groups <- overlapping_age_groups[, id_cols, with = F]
  overlapping_age_groups[, error := "overlapping"]
  dt[, c("overlapping", "check_ages") := NULL]

  # drop overlapping age groups
  dt <- merge(dt, overlapping_age_groups, by = id_cols, all = T)
  dt <- dt[is.na(error)]
  dt[, error := NULL]

  ## determine which age ranges are missing from datatset
  dt[, age_start_lead := data.table::shift(age_start, type = "lead", fill = terminal_age_end), by = by_id_cols] # this should be equal to original age_end
  dt[, age_end_lag := data.table::shift(age_end, type = "lag", fill = first_age_start), by = by_id_cols] # this should be equal to original age_start

  # age_start must be equal to the previous age_end
  dt[, missing_younger := age_start != age_end_lag]
  # age_end must be equal to the next age_start
  dt[, missing_older := age_end != age_start_lead]

  # isolate the age ranges that are missing
  missing_older <- dt[(missing_older), list(age_start = age_end, age_end = age_start_lead), by = by_id_cols]
  missing_younger <- dt[(missing_younger), list(age_start = age_end_lag, age_end = age_start), by = by_id_cols]

  # combine together missing age ranges
  missing_age_ranges <- unique(rbind(missing_older, missing_younger, use.names = T))
  missing_age_ranges <- missing_age_ranges[age_start < age_end] # drop these overlaps that are identified later instead
  missing_age_ranges[, error := "missing"]

  # combine together any overlapping age dt points
  age_format_errors <- rbind(missing_age_ranges, overlapping_age_groups, use.names = T)
  setcolorder(age_format_errors, c(id_cols, "error"))
  setkeyv(age_format_errors, c(id_cols, "error"))

  return(age_format_errors)
}
