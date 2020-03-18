#' @title Aggregate/Scale a detailed level of a variable to an aggregate level
#'
#' @description Aggregate counts or probabilities from a detailed level of a
#' variable to an aggregate level or scale the detailed level values so that the
#' detailed level aggregated together equals the aggregate level.
#'
#' @param dt \[`data.table()`\]\cr
#'   Data to be aggregated or scaled.
#' @param id_cols \[`character()`\]\cr
#'   ID columns that uniquely identify each row of `dt`.
#' @param value_cols \[`character()`\]\cr
#'   Value columns that should be aggregated.
#' @param col_type \[`character(1)`\]\cr
#'   The type of variable that is being aggregated or scaled over. Can be either
#'   'categorical' or 'interval'.
#' @param col_stem \[`character(1)`\]\cr
#'   The name of the variable to be aggregated or scaled over. If aggregating an
#'   'interval' variable should not include the '_start' or '_end' suffix.
#' @param mapping \[`data.table()`\]\cr
#'   For 'categorical' variables, defines how different levels of the variable
#'   relate to each other. For aggregating 'interval' variables, it is used to
#'   specify intervals to aggregate to, while when scaling the mapping is
#'   inferred from the available intervals in `dt`.
#' @param agg_function \[`function()`\]\cr
#'   Function to use when aggregating, can be either `sum` (for counts) or
#'   `prod` (for probabilities).
#' @param missing_dt_severity \[`character(1)`\]\cr
#'   How severe should the consequences of missing data that prevents
#'   aggregation or scaling from occurring be? Can be either "stop", "warning",
#'   "message", or "none". If not "stop", then only the possible aggregations or
#'   scaling is done using the available data.
#' @param drop_present_aggs \[`logical(1)`\]\cr
#'   Whether to drop aggregates (or overlapping intervals) that are already
#'   present in `dt` before aggregating. Default is "False" and the function
#'   errors out.
#'
#' @return \[`data.table()`\] with `id_cols` and `value_cols` columns for
#'   requested aggregates or with scaled values.
#'
#' @details
#' The `agg` function can be used to aggregate over a 'categorical' variable
#' like location, from the country level to global level or over an 'interval'
#' variable like age from five year age-groups to all-ages combined.
#'
#' The `scale` function can be used to scale over a 'categorical' variable
#' like location, so that the sub-national level aggregated together equals the
#' national level. Similarly can be used to scale over an 'interval' variable
#' like age so that the five year age groups aggregated together equals the
#' all-ages value.
#'
#' If 'location' is the variable to be aggregated or scaled then
#' `col_stem = 'location'` and "location" must be included in `id_cols.` If
#' 'age' is the variable to be aggregated or scaled then `col_stem = 'age'` and
#' 'age_start' and 'age_end' must be included in `id_cols` since both variables
#' are needed to represent interval variables.
#'
#' The `mapping` argument for categorical variables must have columns called
#' 'parent' and 'child' that represent how each possible variable relates to
#' each other. For example if aggregating or scaling locations then mapping
#' needs to define how each child location relates to each parent location. It
#' is then assumed that each node in the `mapping` hierarchy will need to be
#' aggregated to.
#'
#' For numeric interval variables the `mapping` argument must have columns for
#' `{col_stem}_start` and `{col_stem}_end` defining the start and end of each
#' aggregate interval.
#'
#' These functions work even if `dt` is not a square dataset. Meaning it is okay
#' if different combinations of `id_vars` have different `col_stem` values. For
#' example if making age aggregates, it is okay if some location-years have
#' 5-year age groups while other location-years have 1-year age groups.
#'
#' @examples
#' # aggregate count data from present day Iran provinces to historical
#' # provinces and Iran as a whole
#' input_dt <- data.table::CJ(location = iran_mapping[!grepl("[0-9]+", child),
#'                                                    child],
#'                            year = 2011,
#'                            value = 1)
#' output_dt <- agg(dt = input_dt,
#'                  id_cols = c("location", "year"),
#'                  value_cols = "value",
#'                  col_stem = "location",
#'                  col_type = "categorical",
#'                  mapping = iran_mapping)
#'
#' # scale count data from present day Iran provinces to Iran national value
#' input_dt <- data.table::CJ(location = iran_mapping[!grepl("[0-9]+", child),
#'                                                    child],
#'                            year = 2011,
#'                            value = 1)
#' input_dt_agg <- data.table::data.table(
#'   location = "Iran (Islamic Republic of)",
#'   year = 2011, value = 62
#' )
#' input_dt <- rbind(input_dt, input_dt_agg)
#' output_dt <- scale(dt = input_dt,
#'                    id_cols = c("location", "year"),
#'                    value_cols = "value",
#'                    col_stem = "location",
#'                    col_type = "categorical",
#'                    mapping = iran_mapping,
#'                    collapse_missing = TRUE)
#'
#' # aggregate age-specific count data
#' input_dt <- data.table::data.table(year = 2010,
#'                         age_start = seq(0, 95, 1),
#'                         value1 = 1, value2 = 2)
#' gen_end(input_dt, id_cols = c("year", "age_start"), col_stem = "age")
#' age_mapping <- data.table::data.table(age_start = c(0, 15, 85),
#'                                       age_end = c(5, 60, Inf))
#' output_dt <- agg(dt = input_dt,
#'                  id_cols = c("year", "age_start", "age_end"),
#'                  value_cols = c("value1", "value2"),
#'                  col_stem = "age",
#'                  col_type = "interval",
#'                  mapping = age_mapping)
#'
#' # scale age-specific probability data
#'
#' @export
#' @rdname agg_scale
agg <- function(dt,
                id_cols,
                value_cols,
                col_stem,
                col_type,
                mapping,
                agg_function = sum,
                missing_dt_severity = "stop",
                drop_present_aggs = FALSE) {

  # Validate arguments ------------------------------------------------------

  assert_agg_scale_args(dt, id_cols, value_cols, col_stem,
                        col_type, mapping, agg_function, "agg")

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

  # identify any issues in input dataset
  dt_issues <- identify_agg_dt_issues(dt, id_cols, value_cols, col_stem,
                                      col_type, mapping, agg_function,
                                      drop_present_aggs)

  # check for aggregates or overlapping intervals in dataset
  present_agg_dt <- dt_issues[issue == "aggregate data present"]
  if (nrow(present_agg_dt) > 0) {
    if (drop_present_aggs) {
      dt <- merge(dt, present_agg_dt, by = id_cols, all = T)
      dt <- dt[is.na(issue)]
      dt[, issue := NULL]
    } else {
      type <- ifelse(col_type == "interval", "overlapping intervals",
                     "aggregates")
      error_msg <-
        paste0("Some ", type, " are already in `dt`.\n",
               "* See `drop_present_aggs` argument if it is okay to drop ",
               "these before re-aggregating.\n",
               "* See `?identify_agg_dt_issues()` to return the ", type,
               " present and printed below.\n",
               paste0(capture.output(present_agg_dt), collapse = "\n"))
      stop(error_msg)
    }
  }

  # check for missing data needed to make aggregates
  missing_dt <- dt_issues[issue == "missing data"]
  empty_missing_dt <- function(dt) nrow(dt) == 0
  error_msg <-
    paste0("Some aggregates in `mapping` cannot be made because input data is ",
           "missing in `dt`.\n",
           if (drop_present_aggs) "* This was checked after dropping aggregates
           or overlapping intervals in the data.\n",
           "* See `missing_dt_severity` argument if it is okay to only make ",
           "aggregates that are possible given available data.\n",
           "* See `?identify_agg_dt_issues()` to return missing data printed ",
           "below.\n",
           paste0(capture.output(missing_dt), collapse = "\n"))
  assertive::assert_engine(empty_missing_dt, missing_dt,
                           msg = error_msg, severity = missing_dt_severity)

  # Do aggregation ----------------------------------------------------------

  original_col_order <- copy(names(dt))
  original_keys <- copy(key(dt))

  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  by_id_cols <- id_cols[!id_cols %in% cols]

  groups <- identify_unique_groupings(dt, col_stem, col_type,
                                      by_id_cols)

  # create one column to describe each interval in mapping
  if (col_type == "interval") {
    mapping <- copy(mapping)
    gen_name(mapping, col_stem = col_stem, format = "interval")
    data.table::setnames(mapping, paste0(col_stem, "_name"), col_stem)
  }

  # aggregate each unique set of grouping separately
  result_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    grouping <- subset_unique_grouping(dt, groups, group_num, col_stem,
                                       col_type, by_id_cols)

    # create mapping from available interval variables and requested intervals
    if (col_type == "interval") {
      interval_tree <- create_agg_interval_tree(grouping$unique_cols,
                                                mapping,
                                                col_stem)
      group_mapping <- data.tree::ToDataFrameNetwork(interval_tree)
    } else {
      group_mapping <- copy(mapping)
    }

    # identify each nonleaf node where aggregation is possible and its subtree
    subtrees <- create_agg_subtrees(group_mapping,
                                    grouping$unique_cols[[col_stem]], col_type)

    # aggregate children to parents for sub-trees where aggregation possible
    target_dt <- NULL
    for (subtree in subtrees) {

      aggregated_same_groupings_dt <- agg_subtree(grouping$dt,
                                                  by_id_cols, value_cols,
                                                  col_stem, col_type,
                                                  agg_function, subtree)
      if (col_type == "interval") {
        gen_name(aggregated_same_groupings_dt, col_stem = col_stem,
                 format = "interval")
        setnames(aggregated_same_groupings_dt, paste0(col_stem, "_name"),
                 col_stem)
      }

      # append to grouping dt so that next subtree has access
      # TODO: can save memory by only saving aggregates to one data.table
      grouping$dt <- rbind(grouping$dt, aggregated_same_groupings_dt,
                           use.names = T)
      # append to final aggregated data to return
      target_dt <- rbind(target_dt, aggregated_same_groupings_dt,
                         use.names = T)
    }
    if (col_type == "interval") target_dt[, c(col_stem) := NULL]
    return(target_dt)
  })
  result_dt <- data.table::rbindlist(result_dt)

  data.table::setcolorder(result_dt, original_col_order)
  data.table::setkeyv(result_dt, original_keys)
  return(result_dt)
}

#' @param collapse_missing \[`logical(1)`\]\cr
#'   When scaling `categorical` data, whether to collapse missing intermediate
#'   levels in `mapping`. Default is "False" and the function errors out due to
#'   missing data.
#'
#' @export
#' @rdname agg_scale
scale <- function(dt,
                  id_cols,
                  value_cols,
                  col_stem,
                  col_type,
                  mapping = NULL,
                  agg_function = sum,
                  missing_dt_severity = "stop",
                  collapse_missing = FALSE) {

  # Validate arguments ------------------------------------------------------

  assert_agg_scale_args(dt, id_cols, value_cols, col_stem,
                        col_type, mapping, agg_function, "scale")

  # check `missing_dt_severity` argument
  assertthat::assert_that(assertthat::is.string(missing_dt_severity),
                          checkmate::checkChoice(missing_dt_severity,
                                                 c("stop", "warning",
                                                   "message", "none")),
                          msg = "`missing_dt_severity` must be one of
                          'stop', 'warning', 'message', or 'none'")

  # check `collapse_missing` argument
  assertthat::assert_that(assertthat::is.flag(collapse_missing),
                          msg = "`collapse_missing` must be a logical")

  # identify any issues in input dataset
  dt_issues <- identify_scale_dt_issues(dt, id_cols, value_cols, col_stem,
                                        col_type, mapping, agg_function,
                                        collapse_missing)

  # check for overlapping intervals in dataset
  present_agg_dt <- dt_issues[issue == "overlapping interval data"]
  if (nrow(present_agg_dt) > 0) {
    type <- "overlapping intervals"
    error_msg <-
      paste0("Some overlapping intervals are in `dt`.\n",
             "* See `?identify_scale_dt_issues()` to return the overlapping",
             " intervals printed below.\n",
             paste0(capture.output(present_agg_dt), collapse = "\n"))
    stop(error_msg)
  }

  # check for missing data needed for scaling
  missing_dt <- dt_issues[issue == "missing data"]
  empty_missing_dt <- function(dt) nrow(dt) == 0
  error_msg <-
    paste0("Some nodes in `mapping` cannot be scaled because input data is ",
           "missing in `dt`.\n",
           "* See `missing_dt_severity` argument if it is okay to only scale ",
           "nodes that are possible given available data.\n",
           "* See `?identify_scale_dt_issues()` to return missing data ",
           "printed below.\n",
           paste0(capture.output(missing_dt), collapse = "\n"))
  assertive::assert_engine(empty_missing_dt, missing_dt,
                           msg = error_msg, severity = missing_dt_severity)

  # Do scaling --------------------------------------------------------------

  original_col_order <- copy(names(dt))
  original_keys <- copy(key(dt))

  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  by_id_cols <- id_cols[!id_cols %in% cols]

  groups <- identify_unique_groupings(dt, col_stem, col_type,
                                      by_id_cols)

  # scale each unique set of grouping separately
  result_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    grouping <- subset_unique_grouping(dt, groups, group_num, col_stem,
                                       col_type, by_id_cols)

    # create mapping from available interval variables
    if (col_type == "interval") {
      interval_tree <- create_scale_interval_tree(grouping$unique_cols,
                                                  col_stem)
      mapping <- data.tree::ToDataFrameNetwork(interval_tree)
    }

    # identify each nonleaf node (and its subtree) where scaling of children
    # nodes is possible
    subtrees <- create_scale_subtrees(mapping,
                                      grouping$unique_cols[[col_stem]],
                                      col_type, collapse_missing)

    # scale children to parents for subtrees where scaling is possible
    for (subtree in subtrees) {
      scaled_same_groupings_dt <- scale_subtree(grouping$dt, by_id_cols,
                                                value_cols, col_stem,
                                                col_type, agg_function,
                                                subtree)

      # replace the unscaled children node values with the scaled values
      id_cols_with_stem <- c(id_cols, if (col_type == "interval") col_stem)
      grouping$dt <- merge(grouping$dt, scaled_same_groupings_dt,
                           by = id_cols_with_stem, all = T)
      children <- names(subtree$children)
      for (value_col in value_cols) {
        grouping$dt[get(col_stem) %in% children,
                    paste0(value_col) := get(paste0(value_col, "_scaled"))]
        grouping$dt[, paste0(value_col, "_scaled") := NULL]
      }
    }
    if (col_type == "interval") grouping$dt[, c(col_stem) := NULL]
    return(grouping$dt)
  })
  result_dt <- data.table::rbindlist(result_dt)

  data.table::setcolorder(result_dt, original_col_order)
  data.table::setkeyv(result_dt, original_keys)
  return(result_dt)
}

#' Check inputs to agg and scale functions
#'
#' @inheritParams agg
#' @param functionality whether `agg` or `scale` function inputs are being
#' checked.
#'
#' @return Returns nothing but throws error if inputs are not formatted
#'   correctly
assert_agg_scale_args <- function(dt,
                                  id_cols,
                                  value_cols,
                                  col_stem,
                                  col_type,
                                  mapping,
                                  agg_function,
                                  functionality) {

  # check `col_type` argument
  assertthat::assert_that(assertthat::is.string(col_type),
                          checkmate::checkChoice(col_type,
                                                 c("categorical", "interval")),
                          msg = "`col_type` must be one of 'categorical'
                                 or 'interval'")

  # check `col_stem` argument
  assertthat::assert_that(assertthat::is.string(col_stem),
                          msg = "`col_stem` must be a string")
  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }

  # check mapping argument
  if (!(functionality == "scale" & col_type == "interval")) {
    assertive::assert_is_data.table(mapping)
    expected_mapping_cols <- c("parent", "child")
    if (col_type == "interval") expected_mapping_cols <- cols
    assertable::assert_colnames(mapping, expected_mapping_cols, only_colnames = T,
                                quiet = T)
    assert_is_unique_dt(mapping, expected_mapping_cols)
  }

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
  assert_is_unique_dt(dt, id_cols)
  if (col_type == "interval") {
    for (col in cols) {
      assertive::assert_is_numeric(dt[[col]])
    }
  } else {
    assertthat::assert_that(!any(grepl(",", dt[[col_stem]])),
                            msg = "`col_stem` column in `dt` must not contain
                                   any commas")

    missing_mapping <- setdiff(dt[[col_stem]],
                               unique(c(mapping$parent, mapping$child)))
    error_msg <- paste0("Some variables are missing in the `mapping` argument.",
                        "\n\t\tMissing: '", paste(missing_mapping,
                                                  collapse = "', '"),
                        "'")
    assertthat::assert_that(length(missing_mapping) == 0, msg = error_msg)
  }
}

#' @title Determine unique sets of the agg/scale variable present in the input
#'   data.table
#'
#' @inheritParams agg
#' @param by_id_cols \[`character()`\]\cr
#'   The `id_cols` without the agg/scale variable(s) included.
#'
#' @return list with two named elements.
#'   1. *groupings*: \[`data.table()`\] with `by_id_cols` and a new column
#'   'available_vars' that is a ';' separated string for each unique variable
#'   present in each combination of `by_id_cols`. If `col_type` is 'interval'
#'   then each 'start' and 'end' variable is also ',' separated.
#'   2. *unique_groupings*: \[`data.table()`\] with only one common for the
#'   unique 'available_vars' in the `groupings`.
#'
#' @seealso [subset_unique_grouping()]
identify_unique_groupings <- function(dt,
                                      col_stem,
                                      col_type,
                                      by_id_cols) {

  # create a temporary column combining the start and end columns for intervals
  if (col_type == "interval") {
    cols <- col_stem
    if (col_type == "interval") {
      cols <- paste0(col_stem, "_", c("start", "end"))
    }
    dt[, c(col_stem) := paste(get(cols[1]), get(cols[2]),
                              sep = ",")]
  }

  # determine the combinations of available variables for each grouping
  groupings <- dt[, list(available_vars = paste(sort(unique(get(col_stem))),
                                                collapse = ";")),
                  by = by_id_cols]
  if (col_type == "interval") dt[, c(col_stem) := NULL]
  unique_groupings <- unique(groupings[, list(available_vars)])

  result <- list(groupings = groupings,
                 unique_groupings = unique_groupings)
  return(result)
}

#' Subset to data for one unique set of the agg/scale variable
#'
#' @inheritParams identify_unique_groupings
#' @param groups \[`list(2)`\]\cr
#'   Output of [identify_unique_groupings()].
#' @param group_num \[`integer(1)`\]\cr
#'   Integer row of `groups$unique_groupings` to subset data to.
#'
#' @return list with two named elements.
#'   1. *unique_cols*: \[`data.table()`\] with unique combinations of the
#'   variable being aggregated/scaled. If `col_type` is 'categorical' then just
#'   a column for `col_stem`. If `col_type` is interval' then a column for
#'   `{col_stem}_start` and `{col_stem}_end`
#'   then the 'start' and 'end' variable values are also ',' separated.
#'   2. *dt*: \[`data.table()`\] subset of the input `dt` that has a specific
#'   combination of the variable being aggregated or scaled over.
#'
#' @seealso [identify_unique_groupings()]
subset_unique_grouping <- function(dt,
                                   groups,
                                   group_num,
                                   col_stem,
                                   col_type,
                                   by_id_cols) {

  group_string <- groups$unique_groupings[group_num, available_vars]

  # identify unique col combinations in this grouping
  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  unique_cols <- data.table::data.table(group = strsplit(group_string,
                                                         split = ";")[[1]])
  unique_cols[, c(cols) := data.table::tstrsplit(group, split = ",")]
  if (col_type == "interval") {
    unique_cols <- unique_cols[, lapply(.SD, as.numeric),
                               .SDcols = cols]
  }
  setkeyv(unique_cols, cols)

  # subset to the data for this grouping
  same_groupings <- groups$groupings[available_vars == group_string]
  same_groupings[, available_vars := NULL]
  same_groupings_dt <- merge(same_groupings, dt, by = by_id_cols, all.x = T)

  # create a single column describing each interval
  if (col_type == "interval") {
    gen_name(unique_cols, col_stem = col_stem, format = "interval")
    data.table::setnames(unique_cols, paste0(col_stem, "_name"), col_stem)
    gen_name(same_groupings_dt, col_stem = col_stem, format = "interval")
    data.table::setnames(same_groupings_dt, paste0(col_stem, "_name"), col_stem)
  }

  result <- list(unique_cols = unique_cols,
                 dt = same_groupings_dt)
  return(result)
}


#' @title Identify issues in the input dataset for aggregation and scaling
#'   functions
#'
#' @inheritParams agg
#'
#' @return \[`data.table()`\] with problematic rows in `dt`. Only includes
#'   original `id_cols` columns and a new column "issue" describing each row's
#'   issue.
#'
#' @examples
#' ## Try to aggregate data with a location missing errors out
#' input_dt <- data.table::CJ(location = iran_mapping[!grepl("[0-9]+", child),
#'                                                    child],
#'                            year = 2011, value = 1)
#' input_dt <- input_dt[location != "Tehran"]
#' \dontrun{
#' output_dt <- agg(dt = input_dt,
#'                  id_cols = c("location", "year"),
#'                  value_cols = "value",
#'                  col_stem = "location",
#'                  col_type = "categorical",
#'                  mapping = iran_mapping)
#' }
#' issues_dt <- identify_agg_dt_issues(dt = input_dt,
#'                                     id_cols = c("location", "year"),
#'                                     value_cols = "value",
#'                                     col_stem = "location",
#'                                     col_type = "categorical",
#'                                     mapping = iran_mapping)
#'
#' ## Try to scale data with a location missing errors out
#' input_dt <- data.table::CJ(location = iran_mapping[!grepl("[0-9]+", child),
#'                                                    child],
#'                            year = 2011,
#'                            value = 1)
#' input_dt_agg <- data.table::data.table(
#'   location = "Iran (Islamic Republic of)",
#'   year = 2011, value = 62
#' )
#' input_dt <- rbind(input_dt, input_dt_agg)
#' \dontrun{
#' output_dt <- scale(dt = input_dt,
#'                    id_cols = c("location", "year"),
#'                    value_cols = "value",
#'                    col_stem = "location",
#'                    col_type = "categorical",
#'                    mapping = iran_mapping,
#'                    collapse_missing = TRUE)
#' }
#' issues_dt <- identify_scale_dt_issues(dt = input_dt,
#'                                       id_cols = c("location", "year"),
#'                                       value_cols = "value",
#'                                       col_stem = "location",
#'                                       col_type = "categorical",
#'                                       mapping = iran_mapping,
#'                                       collapse_missing = TRUE)
#'
#' @export
#' @rdname identify_agg_scale_dt_issues
identify_agg_dt_issues <- function(dt,
                                   id_cols,
                                   value_cols,
                                   col_stem,
                                   col_type,
                                   mapping,
                                   agg_function = sum,
                                   drop_present_aggs = FALSE) {

  # Validate arguments ------------------------------------------------------

  assert_agg_scale_args(dt, id_cols, value_cols, col_stem,
                        col_type, mapping, agg_function, "agg")

  # check `drop_present_aggs` argument
  assertthat::assert_that(assertthat::is.flag(drop_present_aggs),
                          msg = "`drop_present_aggs` must be a logical")

  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  by_id_cols <- id_cols[!id_cols %in% cols]

  groups <- identify_unique_groupings(dt, col_stem, col_type,
                                      by_id_cols)

  # Check for aggregates already present or overlapping intervals -----------

  aggs_present_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    grouping <- subset_unique_grouping(dt, groups, group_num, col_stem,
                                       col_type, by_id_cols)

    check_dt <- NULL
    if (col_type == "categorical") {
      agg_tree <- create_agg_tree(mapping, grouping$unique_cols[[cols]],
                                  col_type)
      present_nodes <- identify_present_agg(agg_tree)

      if (!is.null(present_nodes)) {
        # expand the groupings dataset to show the present aggregate nodes
        check_dt <- grouping$dt[, list(present = present_nodes),
                                by = by_id_cols]
        data.table::setnames(check_dt, "present", cols[1])
      }
    } else if (col_type == "interval") {
      present_nodes <- identify_overlapping_intervals(
        grouping$unique_cols[, c(cols), with = F],
        min(grouping$unique_cols[[cols[1]]]),
        max(grouping$unique_cols[[cols[2]]])
      )
      data.table::setnames(present_nodes, c("start", "end"), cols)
      check_dt <- grouping$dt[, data.table(present_nodes),
                              by = by_id_cols]
    } else {
      stop("can only aggregate 'categorical' or 'interval' data")
    }
    return(check_dt)
  })
  aggs_present_dt <- rbindlist(aggs_present_dt)

  if (nrow(aggs_present_dt) > 0) {
    aggs_present_dt[, issue := "aggregate data present"]
  }

  # drop present aggregates before checking for missingness
  if (nrow(aggs_present_dt) > 0 & drop_present_aggs) {
    dt <- merge(dt, aggs_present_dt, by = id_cols, all = T)
    dt <- dt[is.na(issue)]
    dt[, issue := NULL]

  }

  # Check for missing data or intervals -------------------------------------

  # create one column to describe each interval in mapping
  if (col_type == "interval") {
    mapping <- copy(mapping)
    gen_name(mapping, col_stem = col_stem, format = "interval")
    data.table::setnames(mapping, paste0(col_stem, "_name"), col_stem)
  }

  missing_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    grouping <- subset_unique_grouping(dt, groups, group_num, col_stem,
                                       col_type, by_id_cols)

    # create mapping from available interval variables and requested intervals
    if (col_type == "interval") {
      interval_tree <- create_agg_interval_tree(grouping$unique_cols,
                                                mapping,
                                                col_stem)
      group_mapping <- data.tree::ToDataFrameNetwork(interval_tree)
    } else {
      group_mapping <- copy(mapping)
    }

    agg_tree <- create_agg_tree(group_mapping,
                                grouping$unique_cols[[col_stem]], col_type)
    missing_nodes <- identify_missing_agg(agg_tree)

    check_dt <- NULL
    if (!is.null(missing_nodes)) {
      # expand the groupings dataset to show the missing nodes
      if (col_type == "categorical") {
        check_dt <- grouping$dt[, list(missing = missing_nodes), by = by_id_cols]
        data.table::setnames(check_dt, "missing", cols)
      } else {
        missing_nodes <- name_to_start_end(missing_nodes)
        setDT(missing_nodes)
        check_dt <- grouping$dt[, data.table(missing_nodes), by = by_id_cols]
        data.table::setnames(check_dt, c("start", "end"), cols)
      }
    }
    return(check_dt)
  })
  missing_dt <- rbindlist(missing_dt)

  if (nrow(missing_dt) > 0) {
    missing_dt[, issue := "missing data"]
  }

  # Combine together identified problems ------------------------------------

  problem_dt <- rbind(aggs_present_dt, missing_dt, use.names = T, fill = T)
  if (nrow(problem_dt) > 0) {
    data.table::setkeyv(problem_dt, c(by_id_cols, cols))
  } else {
    problem_dt <- data.table(issue = character())
  }
  return(problem_dt)
}

#' @export
#' @rdname identify_agg_scale_dt_issues
identify_scale_dt_issues <- function(dt,
                                     id_cols,
                                     value_cols,
                                     col_stem,
                                     col_type,
                                     mapping,
                                     agg_function = sum,
                                     collapse_missing = FALSE) {

  # Validate arguments ------------------------------------------------------

  assert_agg_scale_args(dt, id_cols, value_cols, col_stem,
                        col_type, mapping, agg_function, "scale")

  # check `collapse_missing` argument
  assertthat::assert_that(assertthat::is.flag(collapse_missing),
                          msg = "`collapse_missing` must be a logical")

  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  by_id_cols <- id_cols[!id_cols %in% cols]

  groups <- identify_unique_groupings(dt, col_stem, col_type,
                                      by_id_cols)

  # Check for aggregates already present or overlapping intervals -----------

  problem_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    grouping <- subset_unique_grouping(dt, groups, group_num, col_stem,
                                       col_type, by_id_cols)

    # create mapping from available interval variables and requested intervals
    if (col_type == "interval") {
      interval_tree <- create_scale_interval_tree(grouping$unique_cols,
                                                  col_stem)
      group_mapping <- data.tree::ToDataFrameNetwork(interval_tree)
    } else {
      group_mapping <- copy(mapping)
    }

    scale_tree <- create_scale_tree(group_mapping,
                                    grouping$unique_cols[[col_stem]], col_type,
                                    collapse_missing)

    ## check for overlapping intervals in dataset
    overlapping_dt <- NULL
    if (col_type == "interval") {

      # identify each nonleaf (and its subtree) where data exists
      check_subtrees <- data.tree::Traverse(
        scale_tree, filterFun = function(x) {
          data.tree::isNotLeaf(x) & data.tree::GetAttribute(x, "exists")
        }
      )

      # loop through each non-leaf node that exists and check if its leaf nodes
      # cover the entire interval
      overlapping_dt <- lapply(check_subtrees, function(subtree) {

        # get endpoints of subtree leaves
        start <- subtree$Get("left",
                             filterFun = function(x) data.tree::isLeaf(x))
        end <- subtree$Get("right",
                           filterFun = function(x) data.tree::isLeaf(x))

        overlapping_intervals <- identify_overlapping_intervals(
          data.table(start, end), subtree$left, subtree$right
        )
        data.table::setnames(overlapping_intervals, c("start", "end"), cols)
        overlapping_intervals_dt <- grouping$dt[, data.table(overlapping_intervals),
                                                by = by_id_cols]
        overlapping_intervals_dt[, issue := "overlapping interval data"]
        return(overlapping_intervals_dt)
      })
      overlapping_dt <- rbindlist(overlapping_dt)
    }

    ## Check for missing data or intervals
    missing_dt <- NULL
    missing_nodes <- identify_missing_scale(scale_tree)
    if (!is.null(missing_nodes)) {
      # expand the groupings dataset to show the missing nodes
      if (col_type == "categorical") {
        missing_dt <- grouping$dt[, list(missing = missing_nodes),
                                  by = by_id_cols]
        data.table::setnames(missing_dt, "missing", cols)
      } else {
        missing_nodes <- name_to_start_end(missing_nodes)
        setDT(missing_nodes)
        missing_dt <- grouping$dt[, data.table(missing_nodes), by = by_id_cols]
        data.table::setnames(missing_dt, c("start", "end"), cols)
      }
      missing_dt[, issue := "missing data"]
    }
    check_dt <- rbind(overlapping_dt, missing_dt)
    return(check_dt)
  })
  problem_dt <- rbindlist(problem_dt)

  if (nrow(problem_dt) > 0) {
    data.table::setkeyv(problem_dt, c(by_id_cols, cols))
  } else {
    problem_dt <- data.table(issue = character())
  }
  return(problem_dt)
}
