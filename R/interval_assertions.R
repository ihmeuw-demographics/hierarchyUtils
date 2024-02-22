#' @title Check if the interval column in a data.table missing any expected intervals
#'
#' @description Checks to see if the specified interval variable is missing any
#' expected intervals.
#'
#' @param dt \[`data.table()`\]\cr
#'   Data containing the interval variable to check. Should include all 'id_cols'.
#' @param id_cols \[`character()`\]\cr
#'   ID columns that uniquely identify each row of `dt`. Should include
#'   '{col_stem}_start' and '{col_stem}_end'.
#' @param col_stem \[`character(1)`\]\cr
#'   The name of the interval variable to check, should not include the
#'   '_start' or '_end' suffix.
#' @param expected_ints_dt \[`data.table()`\]\cr
#'   The expected intervals that should be completely included in `ints_dt`.
#'   Should include only '{col_stem}_start' and '{col_stem}_end' columns. Can
#'   also be `NULL` in which case `expected_ints_dt` will automatically be set
#'   to the minimum and maximum of each unique set of intervals in `dt`.
#' @param quiet \[`logical(1)`\]\cr
#'   Should progress messages be suppressed as the function is run? Default is
#'   False.
#'
#' @return  `identify_missing_intervals_dt` returns a \[`data.table()`\] with
#'   `id_cols` that are missing expected intervals. If no intervals are missing
#'   then a zero-row \[`data.table()`\] is returned.
#'   `assert_no_missing_intervals_dt` returns nothing but throws an error if
#'   `identify_missing_intervals` returns a non-empty data.table.
#'
#' @details
#' `identify_missing_intervals_dt` works by first identifying each unique set of
#' intervals in `dt`. Then checks one at a time the groups of rows of `dt`
#' that match each set of intervals.
#'
#' `expected_ints_dt = NULL` will automatically check that there are no missing
#' intervals between the minimum and maximum interval in each unique set. This
#' may miss identifying missing intervals at the beginning or end of the range.
#'
#' @examples
#' input_dt <- data.table::data.table(
#'   year = c(rep(2010, 20), rep(2015, 96)),
#'   age_start = c(seq(0, 95, 5), seq(0, 95, 1)),
#'   age_end = c(seq(5, 95, 5), Inf, seq(1, 95, 1), Inf),
#'   value = 1
#' )
#' input_dt <- input_dt[!age_start %in% c(0, 10, 95)]
#'
#' # expect intervals to cover the entire 0-Inf range
#' missing_dt <- identify_missing_intervals_dt(
#'   dt = input_dt,
#'   id_cols = c("year", "age_start", "age_end"),
#'   col_stem = "age",
#'   expected_ints_dt = data.table::data.table(age_start = 0, age_end = Inf)
#' )
#'
#' # expect intervals to cover between the minimum and maximum of each grouping
#' missing_dt <- identify_missing_intervals_dt(
#'   dt = input_dt,
#'   id_cols = c("year", "age_start", "age_end"),
#'   col_stem = "age",
#'   expected_ints_dt = NULL
#' )
#'
#' @export
#' @rdname missing_intervals_dt
assert_no_missing_intervals_dt <- function(dt,
                                           id_cols,
                                           col_stem,
                                           expected_ints_dt,
                                           quiet = FALSE) {

  missing_intervals <- identify_missing_intervals_dt(dt,
                                                     id_cols,
                                                     col_stem,
                                                     expected_ints_dt)
  no_missing_intervals <- nrow(missing_intervals) == 0

  error_msg <-
    paste0("There are missing intervals in `dt`",
           paste0(capture.output(missing_intervals), collapse = "\n"))
  assertthat::assert_that(no_missing_intervals, msg = error_msg)
}

#' @export
#' @rdname missing_intervals_dt
identify_missing_intervals_dt <- function(dt,
                                          id_cols,
                                          col_stem,
                                          expected_ints_dt,
                                          quiet = FALSE) {

  # Check inputs ------------------------------------------------------------

  checkmate::assert_character(col_stem, len = 1)
  cols <- paste0(col_stem, "_", c("start", "end"))

  checkmate::assert_logical(quiet, len = 1)
  checkmate::assert_character(id_cols)
  checkmate::assert_subset(cols, id_cols)
  checkmate::assert_data_table(dt)
  checkmate::assert_subset(id_cols, names(dt))

  checkmate::assert_data_table(expected_ints_dt, ncols = 2, null.ok = TRUE)
  if (!is.null(expected_ints_dt)) checkmate::assert_subset(cols, names(expected_ints_dt))

  original_col_order <- copy(names(dt))
  original_col_order <- original_col_order[original_col_order %in% id_cols]
  original_keys <- copy(key(dt))

  # Check for missing intervals ---------------------------------------------

  # create one column to describe each interval
  dt_intervals <- unique(dt[, .SD, .SDcols = cols])
  gen_name(dt_intervals, col_stem = col_stem, format = "interval")
  data.table::setnames(dt_intervals, paste0(col_stem, "_name"), col_stem)
  data.table::setkeyv(dt_intervals, cols)
  dt <- dt[dt_intervals, on = cols, nomatch = 0]

  # identify unique combinations of intervals to be aggregated
  groups <- dt[, list(col = paste(get(col_stem), collapse = ",")), by = setdiff(id_cols, c(col_stem, cols))]
  unique_groups <- unique(groups$col)

  missing_intervals_dt <- lapply(1:length(unique_groups), function(i) {
    g <- unique_groups[i]
    if (!quiet) message("Interval group ", i, " of ", length(unique_groups), ": ", g)

    group_dt <- groups[col == g]
    group_dt[, col := NULL]

    # get rows of data that have the same combination of intervals
    if (nrow(group_dt) == 0 & length(unique_groups) == 1) {
      # when only one group exists and the only id variables are `col`
      check_dt <- dt
    } else {
      check_dt <- dt[group_dt, on = names(group_dt), nomatch = 0]
    }

    # intervals in dt grouping
    dt_intervals <- unique(check_dt[, .SD, .SDcols = cols])

    # expected entire range of intervals in dt grouping
    if (is.null(expected_ints_dt)) {
      entire_interval <- data.table(
        start = min(dt_intervals, na.rm = TRUE),
        end = max(dt_intervals, na.rm = TRUE)
      )
      data.table::setnames(entire_interval, c("start", "end"), cols)
    } else {
      entire_interval <- expected_ints_dt
    }

    missing_ints <- identify_missing_intervals(
      ints_dt = dt_intervals,
      expected_ints_dt = entire_interval
    )

    # TODO: switch to official data.table CJ with data.table inputs once available
    # https://github.com/Rdatatable/data.table/issues/1717
    CJDT <- function(...)
      Reduce(function(DT1, DT2) cbind(DT1, DT2[rep(1:.N, each=nrow(DT1))]), list(...))

    # combine missing intervals with group dataset
    if (nrow(missing_ints) > 0) {
      data.table::setnames(missing_ints, c("start", "end"), cols)

      # as long as group dt includes other id_cols
      if (!(nrow(group_dt) == 0 & ncol(group_dt) == 0)) {
        missing_ints <- CJDT(group_dt, missing_ints)
      }

    } else {
      missing_ints <- dt[0, id_cols, with = FALSE]
    }

    return(missing_ints)
  })
  missing_intervals_dt <- rbindlist(missing_intervals_dt, use.names = TRUE)

  data.table::setcolorder(missing_intervals_dt, original_col_order)
  data.table::setkeyv(missing_intervals_dt, original_keys)
  return(missing_intervals_dt)
}

#' @title Helper function to check whether simple set of intervals is missing
#'   any expected intervals?
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
#' missing_dt <- hierarchyUtils:::identify_missing_intervals(
#'   ints_dt = ints_dt[!start %in% c(0, 10, 95)],
#'   expected_ints_dt = data.table::data.table(start = 0, end = Inf)
#' )
#'
#' @rdname missing_intervals
#'
#' @noRd
assert_no_missing_intervals <- function(ints_dt, expected_ints_dt) {

  missing_intervals <- identify_missing_intervals(ints_dt, expected_ints_dt)
  no_missing_intervals <- nrow(missing_intervals) == 0

  error_msg <-
    paste0("There are missing intervals in `ints_dt`",
           paste0(capture.output(missing_intervals), collapse = "\n"))
  assertthat::assert_that(no_missing_intervals, msg = error_msg)

}

#' @rdname missing_intervals
#'
#' @noRd
identify_missing_intervals <- function(ints_dt, expected_ints_dt) {

  assertthat::assert_that(
    is.data.table(ints_dt),
    ncol(ints_dt) == 2,
    is.numeric(ints_dt[[1]]),
    is.numeric(ints_dt[[2]]),
    all(ints_dt[[1]] < ints_dt[[2]], na.rm = TRUE),
    msg = paste("`ints_dt` must a 2-column data.table with the first column",
                "representing the start of the interval and the second column",
                "the end of each interval")
  )
  assertthat::assert_that(
    is.data.table(expected_ints_dt),
    ncol(expected_ints_dt) == 2,
    is.numeric(expected_ints_dt[[1]]),
    is.numeric(expected_ints_dt[[2]]),
    all(expected_ints_dt[[1]] < expected_ints_dt[[2]], na.rm = TRUE),
    msg = paste("`expected_ints_dt` must a 2-column data.table with the first",
                "column representing the start of the interval and the second",
                "column the end of each interval")
  )

  # create full interval that all sub intervals should span
  col_names <- names(expected_ints_dt)
  expected_ints_dt <- expected_ints_dt[!(is.na(get(col_names[1])) & is.na(get(col_names[2])))]
  expected_ints <- intervals::Intervals_full(as.matrix(expected_ints_dt),
                                             closed = c(TRUE, FALSE))

  # create left-closed, right-open intervals
  col_names <- names(ints_dt)
  ints_dt <- ints_dt[!(is.na(get(col_names[1])) & is.na(get(col_names[2])))]
  ints <- intervals::Intervals_full(as.matrix(ints_dt), closed = c(TRUE, FALSE))

  # identify missing intervals
  missing_ints <- intervals::interval_difference(expected_ints, ints)

  missing_ints_dt <- data.table::as.data.table(missing_ints)
  data.table::setnames(missing_ints_dt, c("start", "end"))
  data.table::setkeyv(missing_ints_dt, c("start", "end"))
  return(missing_ints_dt)
}

#' @title Check if the interval column in a data.table has overlapping intervals
#'
#' @description Checks to see if the specified interval variable contains
#'   overlapping intervals.
#'
#' @inheritParams identify_missing_intervals_dt
#' @param identify_all_possible \[`logical(1)`\]\cr
#'   Whether to return all overlapping intervals ('TRUE') or try to identify just
#'   the less granular interval ('FALSE'). Default is 'FALSE'. Useful when it may
#'   not be clear what is the less granular interval.
#' @param quiet \[`logical(1)`\]\cr
#'   Should progress messages be suppressed as the function is run? Default is
#'   False.
#'
#' @return  `identify_overlapping_intervals_dt` returns a \[`data.table()`\] with
#'   `id_cols` that have overlapping intervals. If no intervals are overlapping
#'   then a zero-row \[`data.table()`\] is returned.
#'   `assert_no_overlapping_intervals_dt` returns nothing but throws an error if
#'   `identify_overlapping_intervals_dt` returns a non-empty data.table.
#'
#' @details
#' `identify_overlapping_intervals_dt` works by first identifying each unique
#' set of intervals in `dt`. Then checks one at a time the groups of rows
#' of `dt` that match each set of intervals.
#'
#' @examples
#' input_dt <- data.table::data.table(
#'   age_start = seq(0, 95, 5),
#'   age_end = c(seq(5, 95, 5), Inf)
#' )
#' input_dt <- rbind(input_dt, data.table::data.table(age_start = c(15), age_end = c(60)))
#'
#' # identify everything that is overlapping
#' overlapping_dt <- identify_overlapping_intervals_dt(
#'   dt = input_dt,
#'   id_cols = c("age_start", "age_end"),
#'   col_stem = "age",
#'   identify_all_possible = TRUE
#' )
#'
#' # identify only the largest overlapping intervals
#' overlapping_dt <- identify_overlapping_intervals_dt(
#'   dt = input_dt,
#'   id_cols = c("age_start", "age_end"),
#'   col_stem = "age",
#'   identify_all_possible = FALSE
#' )
#'
#' @export
#' @rdname overlapping_intervals_dt
assert_no_overlapping_intervals_dt <- function(dt,
                                               id_cols,
                                               col_stem,
                                               identify_all_possible = FALSE,
                                               quiet = FALSE) {

  overlapping_intervals <- identify_overlapping_intervals_dt(
    dt,
    id_cols,
    col_stem,
    identify_all_possible,
    quiet
  )
  no_overlapping_intervals <- nrow(overlapping_intervals) == 0

  error_msg <-
    paste0("There are overlapping intervals in `dt`",
           paste0(capture.output(overlapping_intervals), collapse = "\n"))
  assertthat::assert_that(no_overlapping_intervals, msg = error_msg)

}

#' @export
#' @rdname overlapping_intervals_dt
identify_overlapping_intervals_dt <- function(dt,
                                              id_cols,
                                              col_stem,
                                              identify_all_possible = FALSE,
                                              quiet = FALSE) {

  # Check inputs ------------------------------------------------------------

  checkmate::assert_character(col_stem, len = 1)
  cols <- paste0(col_stem, "_", c("start", "end"))

  checkmate::assert_logical(quiet, len = 1)
  checkmate::assert_logical(identify_all_possible, len = 1)
  checkmate::assert_character(id_cols)
  checkmate::assert_subset(cols, id_cols)
  checkmate::assert_data_table(dt)
  checkmate::assert_subset(id_cols, names(dt))

  original_col_order <- copy(names(dt))
  original_col_order <- original_col_order[original_col_order %in% id_cols]
  original_keys <- copy(key(dt))

  # Check for missing intervals ---------------------------------------------

  # create one column to describe each interval
  dt_intervals <- unique(dt[, .SD, .SDcols = cols])
  gen_name(dt_intervals, col_stem = col_stem, format = "interval")
  data.table::setnames(dt_intervals, paste0(col_stem, "_name"), col_stem)
  data.table::setkeyv(dt_intervals, cols)
  dt <- dt[dt_intervals, on = cols, nomatch = 0]

  # identify unique combinations of intervals to be aggregated
  groups <- dt[, list(col = paste(get(col_stem), collapse = ",")), by = setdiff(id_cols, c(col_stem, cols))]
  unique_groups <- unique(groups$col)

  overlapping_intervals_dt <- lapply(1:length(unique_groups), function(i) {
    g <- unique_groups[i]
    if (!quiet) message("Interval group ", i, " of ", length(unique_groups), ": ", g)

    group_dt <- groups[col == g]
    group_dt[, col := NULL]

    # get rows of data that have the same combination of intervals
    if (nrow(group_dt) == 0 & length(unique_groups) == 1) {
      # when only one group exists and the only id variables are `col`
      check_dt <- dt
    } else {
      check_dt <- dt[group_dt, on = names(group_dt), nomatch = 0]
    }

    # intervals in dt grouping
    dt_intervals <- unique(check_dt[, .SD, .SDcols = cols])

    overlapping_ints <- identify_overlapping_intervals(
      ints_dt = dt_intervals,
      identify_all_possible = identify_all_possible
    )

    # TODO: switch to official data.table CJ with data.table inputs once available
    # https://github.com/Rdatatable/data.table/issues/1717
    CJDT <- function(...)
      Reduce(function(DT1, DT2) cbind(DT1, DT2[rep(1:.N, each=nrow(DT1))]), list(...))

    # combine missing intervals with group dataset
    if (nrow(overlapping_ints) > 0) {
      data.table::setnames(overlapping_ints, c("start", "end"), cols)

      # get rows of data that have the same combination of intervals
      if (nrow(group_dt) == 0 & length(unique_groups) == 1) {
        # when only one group exists and the only id variables are `col`
        agg_dt <- overlapping_ints
      } else {
        overlapping_ints <- CJDT(group_dt, overlapping_ints)
      }
    } else {
      overlapping_ints <- dt[0, id_cols, with = FALSE]
    }

    return(overlapping_ints)
  })
  overlapping_intervals_dt <- rbindlist(overlapping_intervals_dt, use.names = TRUE)

  data.table::setcolorder(overlapping_intervals_dt, original_col_order)
  data.table::setkeyv(overlapping_intervals_dt, original_keys)
  return(overlapping_intervals_dt)
}


#' @title Helper function to check if the interval variable has any overlapping
#'   intervals.
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
#'   start = c(seq(10, 50, 5), 0),
#'   end = c(seq(15, 55, 5), 11)
#' )
#' overlapping_dt <- hierarchyUtils:::identify_overlapping_intervals(
#'   ints_dt,
#'   identify_all_possible = FALSE
#' )
#' overlapping_dt <- hierarchyUtils:::identify_overlapping_intervals(
#'   ints_dt,
#'   identify_all_possible = TRUE
#' )
#'
#' @noRd
assert_no_overlapping_intervals <- function(ints_dt) {

  overlapping_intervals <- identify_overlapping_intervals(ints_dt)
  no_overlapping_intervals <- nrow(overlapping_intervals) == 0

  error_msg <-
    paste0("There are overlapping intervals in `ints_dt`",
           paste0(capture.output(overlapping_intervals), collapse = "\n"))
  assertthat::assert_that(no_overlapping_intervals, msg = error_msg)

}

#' @inheritParams assert_no_overlapping_intervals
identify_overlapping_intervals <- function(ints_dt, identify_all_possible = FALSE) {

  assertthat::assert_that(
    is.data.table(ints_dt),
    ncol(ints_dt) == 2,
    is.numeric(ints_dt[[1]]),
    is.numeric(ints_dt[[2]]),
    all(ints_dt[[1]] < ints_dt[[2]], na.rm = TRUE),
    msg = paste("`ints_dt` must a 2-column data.table with the first column",
                "representing the start of the interval and the second column",
                "the end of each interval")
  )

  # create left-closed, right-open intervals
  ints <- intervals::Intervals_full(as.matrix(ints_dt), closed = c(TRUE, FALSE))

  # get list mapping between intervals if they overlap at all
  overlaps <- intervals::interval_overlap(ints, ints)
  names(overlaps) <- 1:length(overlaps)

  # remove self match only
  overlaps <- overlaps[sapply(overlaps, function(i) length(i) > 1)]

  # sort by number of intervals that each interval overlaps with so that we can
  # identify the largest overlapping intervals first
  overlaps <- overlaps[order(sapply(overlaps, length), decreasing=T)]

  if (identify_all_possible) {
    overlapping_indices <- names(overlaps)
  } else {
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
  }
  overlapping_ints <- ints[as.integer(overlapping_indices)]

  overlapping_ints_dt <- data.table::as.data.table(overlapping_ints)
  data.table::setnames(overlapping_ints_dt, c("start", "end"))
  data.table::setkeyv(overlapping_ints_dt, c("start", "end"))
  return(overlapping_ints_dt)
}
