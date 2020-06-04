#' @title Is the interval variable missing any expected intervals?
#'
#' @description Checks to see if the input interval variable is missing any
#' expected intervals.
#'
#' @param ints_dt \[`data.table()`\]\cr
#'   Unique intervals. The first column represents the start of each interval
#'   and the second column represents the end of each interval.
#' @param expected_ints_dt \[`data.table()`\]\cr
#'   The expected intervals that should be completely included in `ints_dt`.
#'   The first column represents the start of each interval and the second
#'   column represents the end of each interval.
#'
#' @return  `identify_missing_intervals` returns a \[`data.table()`\] with
#'   columns for the 'start' and 'end' of the missing intervals. If no intervals
#'   are missing then a zero-row \[`data.table()`\] is returned.
#'   `assert_no_missing_intervals` returns nothing but throws an error if
#'   `identify_missing_intervals` returns a non-empty data.table.
#'
#' @examples
#' ints_dt <- data.table::data.table(
#'   start = seq(0, 95, 5),
#'   end = c(seq(5, 95, 5), Inf)
#' )
#' missing_dt <- identify_missing_intervals(
#'   ints_dt = ints_dt[!start %in% c(0, 10, 95)],
#'   expected_ints_dt = data.table::data.table(start = 0, end = Inf)
#' )
#'
#' @export
#' @rdname missing_intervals
assert_no_missing_intervals <- function(ints_dt, expected_ints_dt) {

  missing_intervals <- identify_missing_intervals(ints_dt, expected_ints_dt)
  no_missing_intervals <- nrow(missing_intervals) == 0

  error_msg <-
    paste0("There are missing intervals in `ints_dt`",
           paste0(capture.output(missing_intervals), collapse = "\n"))
  assertthat::assert_that(no_missing_intervals, msg = error_msg)

}

#' @export
#' @rdname missing_intervals
identify_missing_intervals <- function(ints_dt, expected_ints_dt) {

  assertthat::assert_that(
    assertive::is_data.table(ints_dt),
    ncol(ints_dt) == 2,
    all(ints_dt[[1]] < ints_dt[[2]]),
    msg = paste("`ints_dt` must a 2-column data.table with the first column",
                "representing the start of the interval and the second column",
                "the end of each interval")
  )
  assertthat::assert_that(
    assertive::is_data.table(expected_ints_dt),
    ncol(ints_dt) == 2,
    all(ints_dt[[1]] < ints_dt[[2]]),
    msg = paste("`expected_ints_dt` must a 2-column data.table with the first",
                "column representing the start of the interval and the second",
                "column the end of each interval")
  )

  # create full interval that all sub intervals should span
  expected_ints <- intervals::Intervals_full(as.matrix(expected_ints_dt),
                                             closed = c(TRUE, FALSE))

  # create left-closed, right-open intervals
  ints <- intervals::Intervals_full(as.matrix(ints_dt), closed = c(TRUE, FALSE))

  # identify missing intervals
  missing_ints <- intervals::interval_difference(expected_ints, ints)

  missing_ints_dt <- data.table::as.data.table(missing_ints)
  data.table::setnames(missing_ints_dt, c("start", "end"))
  data.table::setkeyv(missing_ints_dt, c("start", "end"))
  return(missing_ints_dt)
}

#' @title Does the interval variable have any overlapping intervals?
#'
#' @description Checks to see if the input interval variable has any
#'   overlapping intervals.
#'
#' @inheritParams identify_missing_intervals
#'
#' @return  `identify_overlapping_intervals` returns a \[`data.table()`\] with
#'   columns for the 'start' and 'end' of the overlapping intervals. If no
#'   intervals are overlapping then a zero-row \[`data.table()`\] is returned.
#'   `assert_no_overlapping_intervals` returns nothing but throws an error if
#'   `identify_overlapping_intervals` returns a non-empty data.table.
#'
#' @examples
#' ints_dt <- data.table::data.table(
#'   start = c(seq(0, 95, 5), 0),
#'   end = c(seq(5, 95, 5), Inf, Inf)
#' )
#' overlapping_dt <- identify_overlapping_intervals(ints_dt)
#'
#'
#' @export
#' @rdname overlapping_intervals
assert_no_overlapping_intervals <- function(ints_dt) {

  overlapping_intervals <- identify_overlapping_intervals(ints_dt)
  no_overlapping_intervals <- nrow(overlapping_intervals) == 0

  error_msg <-
    paste0("There are overlapping intervals in `ints_dt`",
           paste0(capture.output(overlapping_intervals), collapse = "\n"))
  assertthat::assert_that(no_overlapping_intervals, msg = error_msg)

}

#' @export
#' @rdname overlapping_intervals
identify_overlapping_intervals <- function(ints_dt) {

  assertthat::assert_that(
    assertive::is_data.table(ints_dt),
    ncol(ints_dt) == 2,
    all(ints_dt[[1]] < ints_dt[[2]]),
    msg = paste("`ints_dt` must a 2-column data.table with the first column",
                "representing the start of the interval and the second column",
                "the end of each interval")
  )

  # create left-closed, right-open intervals
  ints <- intervals::Intervals_full(as.matrix(ints_dt), closed = c(TRUE, FALSE))

  # get list mapping between intervals if they overlap at all
  overlaps <- intervals::interval_overlap(ints, ints)
  names(overlaps) <- 1:length(overlaps)

  # sort by number of intervals that each interval overlaps with so that we can
  # identify the largest overlapping intervals first
  overlaps <- overlaps[order(sapply(overlaps, length), decreasing=T)]

  overlapping_indices <- c()
  for (i in names(overlaps)) {
    # remove match to itself
    overlaps[[i]] <- overlaps[[i]][overlaps[[i]] != i]

    # remove indices of overlapping intervals that have already been identified
    overlaps[[i]] <- overlaps[[i]][!overlaps[[i]] %in% overlapping_indices]

    if (length(overlaps[[i]]) > 0) {
      overlapping_indices <- c(overlapping_indices, i)
    }
  }
  overlapping_ints <- ints[as.integer(overlapping_indices)]

  overlapping_ints_dt <- data.table::as.data.table(overlapping_ints)
  data.table::setnames(overlapping_ints_dt, c("start", "end"))
  data.table::setkeyv(overlapping_ints_dt, c("start", "end"))
  return(overlapping_ints_dt)
}
