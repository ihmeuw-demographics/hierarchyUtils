#' @title Collapse an interval variable to the most detailed common set of
#'   intervals
#'
#' @description Collapse an interval variable to the most detailed common set of
#'   intervals available for each combination of `id_cols` in a dataset.
#'   Aggregates the collapsed dataset to the common set of intervals.
#'
#' @inheritParams agg
#' @param dt \[`data.table()`\]\cr
#'   Dataset containing the interval variable.
#' @param col_stem \[`character(1)`\]\cr
#'   The name of the variable to collapse, should not include the '_start' or
#'   '_end' suffix for the interval variable.
#' @param missing_dt_severity \[`character(1)`\]\cr
#'   How severe should the consequences of missing intervals that prevent
#'   collapsing to the most detailed common set of intervals be? Can be either
#'   'stop', 'warning', 'message', or 'none'. If not "stop", then only the
#'   intervals that can be correctly collapsed will be done.
#' @param include_missing \[`logical(1)`\]\cr
#'   Whether to include missing intervals in the identified most detailed common
#'   intervals. These missing intervals are not present in all combinations of
#'   `id_cols`. Default is "FALSE".
#'
#' @return \[`data.table()`\] with `id_cols` and `value_cols` columns but with
#'   the `col_stem` intervals reduced to only the most detailed common set of
#'   intervals.
#'
#' @examples
#' id_cols <- c("year_start", "year_end", "sex", "age_start", "age_end")
#' value_cols <- c("value")
#'
#' # set up test input data.table
#' input_dt_male <- data.table::CJ(year_start = 2005, year_end = 2010,
#'                                 sex = "male",
#'                                 age_start = seq(0, 95, 5),
#'                                 value = 25)
#' input_dt_male[age_start == 95, value := 5]
#' input_dt_female <- data.table::CJ(year_start = 2005:2009,
#'                                   sex = "female",
#'                                   age_start = seq(0, 95, 1),
#'                                   value = 1)
#' gen_end(input_dt_female, setdiff(id_cols, c("year_end", "age_end")),
#'         col_stem = "year", right_most_endpoint = 2010)
#' input_dt <- rbind(input_dt_male, input_dt_female)
#' gen_end(input_dt, setdiff(id_cols, "age_end"), col_stem = "age")
#' data.table::setkeyv(input_dt, id_cols)
#'
#'
#' collapsed_dt <- collapse_common_intervals(
#'   dt = input_dt,
#'   id_cols = id_cols,
#'   value_cols = value_cols,
#'   col_stem = "year"
#' )
#' collapsed_dt <- collapse_common_intervals(
#'   dt = collapsed_dt,
#'   id_cols = id_cols,
#'   value_cols = value_cols,
#'   col_stem = "age"
#' )
#'
#' @export
collapse_common_intervals <- function(dt,
                                      id_cols,
                                      value_cols,
                                      col_stem,
                                      agg_function = sum,
                                      missing_dt_severity = "stop",
                                      drop_present_aggs = FALSE,
                                      include_missing = FALSE) {

  # Validate arguments ------------------------------------------------------

  # check `col_stem` argument
  assertthat::assert_that(assertthat::is.string(col_stem),
                          msg = "`col_stem` must be a string")
  cols <- paste0(col_stem, "_", c("start", "end"))

  # check `agg_function` argument
  assertthat::assert_that(assertive::is_function(agg_function),
                          identical(agg_function, sum) |
                            identical(agg_function, prod),
                          msg = "`agg_function` must be either the 'sum' or
                          'prod' function")

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  error_msg <- paste0("`id_cols` must include '",
                      paste(cols, collapse = "', '"), "'")
  assertthat::assert_that(all(cols %in% id_cols), msg = error_msg)

  # check `value_cols` argument
  assertive::assert_is_character(value_cols)

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c(id_cols, value_cols), only_colnames = T,
                              quiet = T)
  for (value_col in value_cols) {
    assertive::assert_is_numeric(dt[[value_col]])
  }
  demUtils::assert_is_unique_dt(dt, id_cols)
  for (col in cols) {
    assertive::assert_is_numeric(dt[[col]])
  }

  # check `missing_dt_severity` argument
  assertthat::assert_that(assertthat::is.string(missing_dt_severity),
                          checkmate::checkChoice(missing_dt_severity,
                                                 c("stop", "warning",
                                                   "message", "none")),
                          msg = "`missing_dt_severity` must be one of
                          'stop', 'warning', 'message', or 'none'")

  # check `drop_present_aggs` argument
  assertthat::assert_that(assertthat::is.flag(drop_present_aggs),
                          msg = "`drop_present_aggs` must be a logical")

  # check `include_missing` argument
  assertthat::assert_that(assertthat::is.flag(include_missing),
                          msg = "`include_missing` must be a logical")

  # Identify and collapse to most detailed common intervals -----------------

  original_col_order <- copy(names(dt))
  original_keys <- copy(key(dt))

  interval_id_cols <- id_cols[grepl("_start$|_end$", id_cols)]
  categorical_id_cols <- id_cols[!id_cols %in% interval_id_cols]
  by_id_cols <- id_cols[!id_cols %in% cols]

  # check for overlapping intervals
  overlapping_dt <- dt[, identify_overlapping_intervals(.SD),
                       .SDcols = cols, by = by_id_cols]
  data.table::setnames(overlapping_dt, c("start", "end"), cols)
  overlapping_dt[, issue := "overlapping intervals present"]

  if (nrow(overlapping_dt) > 0) {
    if (drop_present_aggs) {
      dt <- merge(dt, overlapping_dt, by = id_cols, all = T)
      dt <- dt[is.na(issue)]
      dt[, issue := NULL]
    } else {
      error_msg <-
        paste0("Some overlapping intervals are already in `dt`.\n",
               "* See `drop_present_aggs` argument if it is okay to drop ",
               "these before collapsing intervals.\n",
               paste0(capture.output(overlapping_dt), collapse = "\n"))
      stop(error_msg)
    }
  }

  common_intervals <- identify_common_intervals(
    dt,
    id_cols,
    col_stem,
    include_missing = TRUE # these are identified below
  )
  collapsed_dt <- merge_common_intervals(dt, common_intervals, col_stem)

  # check for missing intervals
  missing_dt <- collapsed_dt[, identify_missing_intervals(.SD, common_intervals),
                             .SDcols = cols, by = by_id_cols]
  data.table::setnames(missing_dt, c("start", "end"), cols)
  empty_missing_dt <- function(dt) nrow(dt) == 0
  error_msg <-
    paste0("Some intervals in `dt` are missing making it impossible to collapse ",
           "the desired column.\n",
           paste0(capture.output(missing_dt), collapse = "\n"))
  assertive::assert_engine(empty_missing_dt, missing_dt,
                           msg = error_msg, severity = missing_dt_severity)

  # drop the common intervals that the missing intervals are part of
  if (nrow(missing_dt) > 0) {
    # determine the common intervals for the detailed missing dataset
    full_missing_dt <- merge_common_intervals(
      missing_dt,
      common_intervals,
      col_stem
    )
    full_missing_dt <- full_missing_dt[, c(if (include_missing) by_id_cols,
                                           c("common_start", "common_end")),
                                       with = F]
    full_missing_dt <- unique(full_missing_dt)
    # drop the common intervals that the missing intervals are part of
    full_missing_dt[, drop := TRUE]
    collapsed_dt <- merge(collapsed_dt, full_missing_dt, all = T,
                          by = c(if (include_missing) by_id_cols,
                                 c("common_start", "common_end")))
    collapsed_dt <- collapsed_dt[is.na(drop)]
  }

  # aggregate so that rows are all unique again
  collapsed_dt[, c(cols) := NULL]
  data.table::setnames(collapsed_dt, c("common_start", "common_end"), cols)
  collapsed_dt <- collapsed_dt[, lapply(.SD, agg_function),
                               .SDcols = value_cols,
                               by = id_cols]

  data.table::setcolorder(collapsed_dt, original_col_order)
  data.table::setkeyv(collapsed_dt, original_keys)
  return(collapsed_dt)
}

#' @title Helper functions for collapsing to the most detailed common intervals
#'
#' @description [`identify_common_intervals()`] identifies the most detailed
#'   common set of intervals for a given interval variable and
#'   [`merge_common_intervals()`] merges these on to the original dataset.
#'
#' @inheritParams collapse_common_intervals
#'
#' @return [`identify_common_intervals()`] returns a \[`data.table()`\] with two
#'   columns called 'common_start' and 'common_end' defining the most detailed
#'   common set of intervals for the `col_stem` interval variable.
#'
#' @examples
#' id_cols <- c("year_start", "year_end", "sex", "age_start", "age_end")
#'
#' # set up test input data.table
#' input_dt_male <- data.table::CJ(year_start = 2005, year_end = 2010,
#'                                 sex = "male",
#'                                 age_start = seq(0, 95, 5),
#'                                 value = 25)
#' input_dt_male[age_start == 95, value := 5]
#' input_dt_female <- data.table::CJ(year_start = 2005:2009,
#'                                   sex = "female",
#'                                   age_start = seq(0, 95, 1),
#'                                   value = 1)
#' gen_end(input_dt_female, setdiff(id_cols, c("year_end", "age_end")),
#'         col_stem = "year", right_most_endpoint = 2010)
#' input_dt <- rbind(input_dt_male, input_dt_female)
#' gen_end(input_dt, setdiff(id_cols, "age_end"), col_stem = "age")
#' data.table::setkeyv(input_dt, id_cols)
#'
#' common_intervals <- hierarchyUtils:::identify_common_intervals(
#'   dt = input_dt,
#'   id_cols = id_cols,
#'   col_stem = "year"
#' )
#'
#' result_dt <- hierarchyUtils:::merge_common_intervals(
#'   dt = input_dt,
#'   common_intervals = common_intervals,
#'   col_stem = "year"
#' )
#'
#' @rdname helper_common_intervals
identify_common_intervals <- function(dt,
                                      id_cols,
                                      col_stem,
                                      include_missing = FALSE) {

  cols <- paste0(col_stem, "_", c("start", "end"))
  by_id_cols <- id_cols[!id_cols %in% cols]

  # identify unique interval combinations in dataset
  intervals <- unname(split(dt, by = by_id_cols))
  intervals <- lapply(intervals, function(split_dt) {
    split_dt <- split_dt[, cols, with = F]
    data.table::setnames(split_dt, cols, c("start", "end"))
    return(split_dt)
  })
  intervals <- unique(intervals)
  intervals <- intervals[mapply(function(ints_dt) nrow(ints_dt) > 0, intervals)]

  check_each_pair <- function(ints_dt1, ints_dt2) {
    ints1 <- intervals::Intervals_full(as.matrix(ints_dt1),
                                          closed = c(TRUE, FALSE))
    ints2 <- intervals::Intervals_full(as.matrix(ints_dt2),
                                          closed = c(TRUE, FALSE))

    # reduce the intervals in each input by finding all the intervals that
    # overlap at all with each other and combining them
    common_ints <- unique(c(ints1, ints2))
    overlap_mapping <- intervals::interval_overlap(
      from = common_ints,
      to = common_ints
    )
    while (any(sapply(overlap_mapping, length) > 1)) {
      collapsed_ints_list <- lapply(1:length(overlap_mapping), function(i) {
        intervals::interval_union(
          from = common_ints[i],
          to = common_ints[overlap_mapping[[i]]]
        )
      })
      common_ints <- unique(Reduce(c, collapsed_ints_list))
      overlap_mapping <- intervals::interval_overlap(
        from = common_ints,
        to = common_ints
      )
    }

    # remove intervals that were missing from the original intervals
    if (!include_missing) {
      remove_intervals <- intervals::interval_union(
        intervals::interval_complement(ints1),
        intervals::interval_complement(ints2)
      )
      overlap_mapping <- intervals::interval_overlap(
        from = common_ints,
        to = remove_intervals
      )
      common_ints <- common_ints[sapply(overlap_mapping, length) == 0]
    }

    common_ints_dt <- data.table::as.data.table(common_ints)
    data.table::setnames(common_ints_dt, c("start", "end"))
    return(common_ints_dt)
  }

  # identify the most detailed common set of intervals
  common_intervals <- Reduce(check_each_pair, intervals)
  data.table::setnames(common_intervals, c("start", "end"),
                       c("common_start", "common_end"))
  data.table::setkeyv(common_intervals, c("common_start", "common_end"))
  return(common_intervals)
}

#' @inheritParams collapse_common_intervals
#' @param common_intervals \[`data.table()`\]\cr
#'   Common intervals returned by [`identify_common_intervals()`]
#'
#' @return [`identify_common_intervals()`] returns a \[`data.table()`\] with the
#'   same columns and rows as originally in `dt`, with two additional columns
#'   merged on from `common_intervals`. These new columns are called
#'   'common_start' and 'common_end' defining the most detailed common interval
#'   each row maps to.
#'
#' @rdname helper_common_intervals
merge_common_intervals <- function(dt, common_intervals, col_stem) {
  cols <- paste0(col_stem, "_", c("start", "end"))
  data.table::setkeyv(common_intervals, c("common_start", "common_end"))

  collapsed_dt <- data.table::foverlaps(
    dt,
    common_intervals,
    by.x = cols,
    by.y = c("common_start", "common_end")
  )
  collapsed_dt <- collapsed_dt[get(cols[1]) >= common_start & get(cols[2]) <= common_end]
  return(collapsed_dt)
}
