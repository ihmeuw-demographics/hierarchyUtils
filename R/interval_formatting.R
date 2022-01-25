#' @title Generate columns to help describe numeric variable intervals
#'
#' @description hierarchyUtils assumes numeric interval variables are grouped
#' into left-closed, right-open intervals. \eqn{a <= x < b}. Each interval can
#' be described by their endpoints from which interval lengths and nicer
#' formatted interval names can be created.
#'
#' @param dt \[`data.table()`\]\cr
#'   `col_stem`-specific data.
#' @param id_cols \[`character()`\]\cr
#'   ID columns that uniquely identify each row of `dt`. This must include
#'   '`{col_stem}`_start'.
#' @param col_stem \[`character(1)`\]\cr
#'   Base name of the numeric variable column. Does not include the '_start',
#'   '_end' etc. suffix.
#' @param right_most_endpoint \[`numeric(1)`\]\cr
#'   Assumed right most endpoint of '`{col_stem}`_end'. Default is \code{Inf}.
#' @param format \[`character(1)`\]\cr
#'   Formatting style for the interval names. Default is 'to'; can also be
#'   'interval' or 'dash'.
#' @param format_infinite \[`character(1)`\]\cr
#'   Formatting style for infinite endpoint intervals. Default is 'plus'; can
#'   also be '+'. Ignored when `format = "interval"`.
#'
#' @return Invisibly returns reference to modified `dt`.
#'
#' @details `gen_end` generates a new column '`{col_stem}`_end' for the
#' right-open endpoint of each interval from a series of left-closed endpoints
#' '`{col_stem}`_start'.
#'
#' `gen_end` assumes that only the most detailed intervals are present in the
#' input dataset; including overlapping intervals will not return expected
#' results. For example if you had intervals of 0-5, 5-10, 2-7, 10+ in `dt`
#' (but only the start of each interval is provided to the dataset) then the
#' inferred intervals would be 0-2, 2-5, 5-7, 7-10, 10+.
#'
#' Input data `dt` for `gen_end` must:
#'   * Contain all columns specified in `id_cols`.
#'   * Have a column called '`{col_stem}`_start'.
#'   * Have each row uniquely identified by each combination of `id_cols`.
#'
#' `gen_length` generates a new column `{col_stem}_length` for the length of
#' each interval. Input data `dt` for `gen_length` must contain
#' '`{col_stem}`_start' and '`{col_stem}`_end' columns.
#'
#' `gen_name` generates a new column `{col_stem}_name` describing each interval.
#'
#' Formatting style for intervals:
#'   * \eqn{[a, b)} interval notation is used when `format = 'interval`.
#'   * `a to b` is used when `format = "to"`.
#'   * `a-b` is used when `format = "dash"`.
#'
#' Formatting style for infinite endpoint interval:
#'   * \eqn{[a, Inf)} interval notation is used when `format = 'interval`.
#'   * `a plus` is used when `format_infinite = "plus"`.
#'   * `a+` is used when `format = "+"`.
#'
#' @examples
#' input_dt <- data.table::data.table(location = "France", year = 2010,
#'                                    sex = "female",
#'                                    age_start = 0:95,
#'                                    value1 = 1, value2 = 2)
#' id_cols <- c("location", "year", "sex", "age_start")
#' gen_end(input_dt, id_cols, col_stem = "age")
#' gen_length(input_dt, col_stem = "age")
#' gen_name(input_dt, col_stem = "age")
#'
#' @export
#' @rdname gen_interval_cols
gen_end <- function(dt, id_cols, col_stem, right_most_endpoint = Inf) {

  # Validate arguments ------------------------------------------------------

  # check `col_stem` argument
  assertthat::assert_that(assertthat::is.string(col_stem),
                          !grepl("_(start|end)$", col_stem),
                          msg = "`col_stem` must be a string that does not
                          include the suffix '_start' or '_end'")
  start_col <- paste0(col_stem, "_start")
  end_col <- paste0(col_stem, "_end")

  # check extreme endpoints arguments
  assertthat::assert_that(assertthat::is.number(right_most_endpoint),
                          msg = "`right_most_endpoint` must be a length one
                          numeric")

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  assertthat::assert_that(start_col %in% id_cols,
                          msg = "`id_cols` must include '`{col_stem}`_start'")

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, id_cols, only_colnames = F, quiet = T)
  assertthat::assert_that(!end_col %in% names(dt),
                          msg = paste0("'", end_col,
                                       "' column already in `dt`"))
  assertthat::assert_that(
    assertive::is_numeric(dt[[start_col]]),
    all(!is.na(dt[[start_col]]))
  )
  demUtils::assert_is_unique_dt(dt, id_cols)

  # Calculate left-closed endpoints -----------------------------------------

  by_id_cols <- id_cols[!id_cols %in% start_col]
  original_col_order <- copy(names(dt))

  original_keys <- copy(key(dt))
  if (setequal(id_cols, original_keys)) {
    # if id_cols are already used as keys, also include new end_col
    original_keys <- c(original_keys, end_col)
  } else {
    # temporarily sort by id_cols
    data.table::setkeyv(dt, id_cols)
  }

  data.table::setnames(dt, start_col, "start_col")
  if (!assertive::is_integer(right_most_endpoint)) {
    dt[, start_col := as.numeric(start_col)]
  }
  dt[, end_col := data.table::shift(start_col, type = "lead",
                                    fill = right_most_endpoint),
     by = by_id_cols]

  # Format result -----------------------------------------------------------

  # clean up temporary column names
  data.table::setnames(dt, "end_col", end_col)
  data.table::setnames(dt, "start_col", start_col)

  # check new column
  assertthat::assert_that(
    assertive::is_numeric(dt[[end_col]]),
    all(!is.na(dt[[end_col]]))
  )

  # put end_col to the right of start_col
  new_col_order <- append(original_col_order, end_col,
                          match(start_col, original_col_order))
  data.table::setcolorder(dt, new_col_order)
  data.table::setkeyv(dt, original_keys)

  return(invisible(dt))
}

#' @export
#' @rdname gen_interval_cols
gen_length <- function(dt, col_stem) {

  # Validate arguments ------------------------------------------------------

  # check `col_stem` argument
  assertthat::assert_that(assertthat::is.string(col_stem),
                          !grepl("_(start|end|length)$", col_stem),
                          msg = "`col_stem` must be a string that does not
                          include the suffix '_start', '_end', '_length'.")
  start_col <- paste0(col_stem, "_start")
  end_col <- paste0(col_stem, "_end")
  length_col <- paste0(col_stem, "_length")

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c(start_col, end_col),
                              only_colnames = F, quiet = T)
  assertthat::assert_that(!length_col %in% names(dt),
                          msg = paste0("'", length_col,
                                       "' column already in `dt`"))
  assertthat::assert_that(
    assertive::is_numeric(dt[[start_col]]),
    all(!is.na(dt[[start_col]]))
  )
  assertthat::assert_that(
    assertive::is_numeric(dt[[end_col]]),
    all(!is.na(dt[[end_col]]))
  )

  # Calculate interval length column ----------------------------------------

  original_col_order <- copy(names(dt))

  dt[, length_col := get(end_col) - get(start_col)]

  # Format result -----------------------------------------------------------

  # clean up temporary column name
  data.table::setnames(dt, "length_col", length_col)

  # check new column
  assertthat::assert_that(
    assertive::is_numeric(dt[[length_col]]),
    all(!is.na(dt[[length_col]]))
  )

  # put length_col to the right of end_col
  new_col_order <- append(original_col_order, length_col,
                          match(end_col, original_col_order))
  data.table::setcolorder(dt, new_col_order)

  return(invisible(dt))
}

#' @export
#' @rdname gen_interval_cols
gen_name <- function(dt,
                     col_stem,
                     format = "to",
                     format_infinite = "plus",
                     right_most_endpoint = Inf) {

  # Validate arguments ------------------------------------------------------

  # check `col_stem` argument
  assertthat::assert_that(assertthat::is.string(col_stem),
                          !grepl("_(start|end|name)$", col_stem),
                          msg = "`col_stem` must be a string that does not
                          include the suffix '_start', '_end' or '_name'")
  start_col <- paste0(col_stem, "_start")
  end_col <- paste0(col_stem, "_end")
  name_col <- paste0(col_stem, "_name")

  # check `format` argument
  assertthat::assert_that(assertthat::is.string(format),
                          checkmate::checkChoice(format,
                                                 choices = c("interval", "to",
                                                             "dash")),
                          msg = "`format` must be a string and one of
                          'interval', 'to', or 'dash'")

  # check `format_infinite` argument
  assertthat::assert_that(assertthat::is.string(format_infinite),
                          checkmate::checkChoice(format_infinite,
                                                 choices = c("plus", "+")),
                          msg = "`format_infinite` must be a string and one of
                          'plus' or '+'")

  # check extreme endpoints arguments
  assertthat::assert_that(assertthat::is.number(right_most_endpoint),
                          msg = "`right_most_endpoint` must be a length one
                          numeric")

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c(start_col, end_col), only_colnames = F,
                              quiet = T)
  assertthat::assert_that(!name_col %in% names(dt),
                          msg = paste0("'", name_col,
                                       "' column already in `dt`"))
  assertthat::assert_that(assertive::is_numeric(dt[[start_col]]))
  assertthat::assert_that(assertive::is_numeric(dt[[end_col]]))

  # Calculate age name column -----------------------------------------------

  original_col_order <- copy(names(dt))

  if (format == "to") {
    dt[, name_col := paste0(get(start_col), " to ", get(end_col))]
  } else if (format == "dash") {
    dt[, name_col := paste0(get(start_col), "-", get(end_col))]
  } else if (format == "interval") {
    dt[, name_col := paste0("[", get(start_col), ", ", get(end_col), ")")]
  }

  if (format == "interval") {
    dt[get(end_col) == right_most_endpoint,
       name_col := paste0("[", get(start_col), ", Inf)")]
  } else {
    terminal_string <- ifelse(format_infinite == "plus", " plus", "+")
    dt[get(end_col) == right_most_endpoint,
       name_col := paste0(get(start_col), terminal_string)]
  }

  # Format result -----------------------------------------------------------

  # clean up temporary column name
  data.table::setnames(dt, "name_col", name_col)

  # check new column
  assertive::assert_is_character(dt[[name_col]])
  assertthat::assert_that(
    all(!is.na(dt[[name_col]]))
  )

  # put name_col to the right of end_col
  new_col_order <- append(original_col_order, name_col,
                          match(end_col, original_col_order))
  data.table::setcolorder(dt, new_col_order)

  return(invisible(dt))
}

#' @title Parse interval notation name to interval endpoints
#'
#' @param name \[`character()`\]\cr
#'   left-closed, right-open interval notation as described in `gen_name().
#'
#' @return \[`list()`\] with element for 'start' containing a numeric vector of
#'  left-closed endpoints and another element for 'end' containing a numeric
#'  vector of right-opens endpoints.
#'
#' @noRd
name_to_start_end <- function(name) {

  # Validate arguments ------------------------------------------------------

  # check `name` argument
  assertthat::assert_that(
    assertive::is_character(name),
    all(grepl("^\\[((NA)|(-Inf)|([-.0-9]+)),\\s((NA)|(Inf)|([-.0-9]+))\\)$",
              name)),
    msg = "`name` must be a character vector formatted in left-closed,
    right-open interval notation as described in `gen_name()`"
  )

  # Parse start and end of interval -----------------------------------------

  # remove left "["
  start <- gsub("^\\[", "", name)
  # remove ", ", right endpoint, right ")".
  # right endpoint can be positive infinity or any numeric
  start <- gsub(",\\s((NA)|(Inf)|([-.0-9]+))\\)$", "", start)
  start[start == "NA"] <- NA_character_
  start <- as.numeric(start)

  # remove right ")"
  end <- gsub("\\)$", "", name)
  # remove left "[", left endpoint, ", "
  # left endpoint can be negative infinity or any numeric
  end <- gsub("^\\[((NA)|(-Inf)|([-.0-9]+)),\\s", "", end)
  end[end == "NA"] <- NA_character_
  end <- as.numeric(end)

  result <- list(start = start,
                 end = end)
  return(result)
}
