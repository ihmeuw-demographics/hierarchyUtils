#' @title Aggregate/Scale a detailed level of a hierarchical variable to an
#'   aggregate level
#'
#' @description Aggregate counts or probabilities from a detailed level of a
#'   hierarchical variable to an aggregate level or scale the detailed level
#'   values so that the detailed level aggregated together equals the aggregate
#'   level.
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
#'   For 'categorical' variables, defines how different levels of the
#'   hierarchical variable relate to each other. For aggregating 'interval'
#'   variables, it is used to specify intervals to aggregate to, while when
#'   scaling the mapping is inferred from the available intervals in `dt`.
#' @param agg_function \[`function()`\]\cr
#'   Function to use when aggregating, can be either `sum` (for counts) or
#'   `prod` (for probabilities).
#' @param missing_dt_severity \[`character(1)`\]\cr
#'   How severe should the consequences of missing data that prevents
#'   aggregation or scaling from occurring be? Can be either 'stop', 'warning',
#'   'message', or 'none'. If not "stop", then only the possible aggregations or
#'   scaling is done using the available data.
#' @param drop_present_aggs \[`logical(1)`\]\cr
#'   Whether to drop aggregates (or overlapping intervals) that are already
#'   present in `dt` before aggregating. Default is 'False' and the function
#'   errors out.
#' @param collapse_interval_cols \[`logical(1)`\]\cr
#'   Whether to collapse interval `id_cols` (not including `col_stem` if it is
#'   an interval variable). Default is 'False'. If set to 'True' the interval
#'   columns are collapsed to the most detailed common intervals and will error
#'   out if there are overlapping intervals. See details or vignettes for more
#'   information.
#'
#' @return \[`data.table()`\] with `id_cols` and `value_cols` columns for
#'   requested aggregates or with scaled values.
#'
#' @details
#' The `agg` function can be used to aggregate to different levels of a pre
#' defined hierarchy. For example a categorical variable like location you can
#' aggregate the country level to global level or for a numeric 'interval'
#' variable like age you can aggregate from  five year age-groups to all-ages
#' combined.
#'
#' The `scale` function can be used to scale different levels of hierarchical
#' variables like location, so that the sub-national level aggregated together
#' equals the national level. Similarly, it can be used to scale a numeric
#' 'interval' variable like age so that the five year age groups aggregated
#' together equals the all-ages value.
#'
#' If 'location' is the variable to be aggregated or scaled then
#' `col_stem = 'location'` and 'location' must be included in `id_cols.` If
#' 'age' is the variable to be aggregated or scaled then `col_stem = 'age'` and
#' 'age_start' and 'age_end' must be included in `id_cols` since both variables
#' are needed to represent interval variables.
#'
#' The `mapping` argument defines how different levels of the hierarchical
#' variable relate to each other. For numeric interval variables the hierarchy
#' can be inferred while for categorical variables the full hierarchy needs to
#' be provided.
#'
#' `mapping` for categorical variables must have columns called 'parent' and
#' 'child' that represent how each possible variable relates to each other.
#' For example if aggregating or scaling locations then mapping needs to define
#' how each child location relates to each parent location. It is then assumed
#' that each parent location in the `mapping` hierarchy will need to be
#' aggregated to.
#'
#' `mapping` for numeric interval variables is only needed when aggregating data
#' to define exactly which aggregates are needed. It must have columns for
#' '`{col_stem}`_start' and '`{col_stem}`_end' defining the start and end of each
#' aggregate interval that is need. When scaling data, `mapping` should be
#' `NULL` since the hierarchy can be inferred from the available intervals in
#' `dt`.
#'
#' `agg` and `scale` work even if `dt` is not a square dataset. Meaning it is
#' okay if different combinations of `id_vars` have different `col_stem` values
#' available. For example if making age aggregates, it is okay if some
#' location-years have 5-year age groups while other location-years have 1-year
#' age groups.
#'
#' If `collapse_interval_cols = TRUE` it is okay if the interval variables
#' included in `id_vars` are not all exactly the same, `agg` and `scale` will
#' collapse to the most detailed common intervals
#' [`collapse_common_intervals()`] prior to aggregation or scaling. An example
#' of this is when aggregating subnational data to the national level (so
#' `col_stem` is 'location' and `col_type` is 'categorical') but each
#' subnational location contains different age groups. [`agg()`] and [`scale()`]
#' first aggregate to the most detailed common age groups before making location
#' aggregates.
#'
#' The `agg` and `scale` functions currently only work when combining counts or
#' probabilities. If the data is in rate-space then you need to convert to count
#' space first, aggregate/scale, and then convert back.
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
                drop_present_aggs = FALSE,
                collapse_interval_cols = FALSE) {

  # Validate arguments ------------------------------------------------------

  assert_agg_scale_args(dt, id_cols, value_cols, col_stem,
                        col_type, mapping, agg_function,
                        collapse_interval_cols, "agg")

  original_col_order <- copy(names(dt))
  original_keys <- copy(key(dt))

  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  interval_id_cols <- id_cols[grepl("_start$|_end$", id_cols)]
  interval_id_cols_stems <- unique(gsub("_start$|_end$", "", interval_id_cols))
  categorical_id_cols <- id_cols[!id_cols %in% interval_id_cols]
  by_id_cols <- id_cols[!id_cols %in% cols]

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
                                      drop_present_aggs, collapse_interval_cols)

  # check for aggregates or overlapping intervals in dataset
  present_agg_dt <- dt_issues[grepl("aggregate|overlapping", issue)]
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
  missing_dt <- dt_issues[grepl("missing", issue)]
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

  # drop data that is missing id_col intervals that will be needed to make
  # aggregation. Missingness in the `col_stem` variable is handled within the
  # mapping/tree creation
  if (nrow(missing_dt) > 0) {
    # determine which common intervals the original data and missing data maps to
    common_id_cols <- copy(id_cols)
    for (stem in interval_id_cols_stems[interval_id_cols_stems != col_stem]) {
      common_intervals <- identify_common_intervals(
        dt,
        id_cols,
        stem,
        include_missing = TRUE
      )
      dt <- merge_common_intervals(dt, common_intervals, stem)
      data.table::setnames(dt, c("common_start", "common_end"),
                           paste0("common_", stem, "_", c("start", "end")))

      missing_dt <- merge_common_intervals(missing_dt, common_intervals, stem)
      data.table::setnames(missing_dt, c("common_start", "common_end"),
                           paste0("common_", stem, "_", c("start", "end")))
      missing_dt[, paste0(stem, "_", c("start", "end")) := NULL]

      common_id_cols[common_id_cols %in% paste0(stem, "_", c("start", "end"))] <-
        paste0("common_", stem, "_", c("start", "end"))
    }
    missing_dt <- unique(missing_dt)

    # drop missing data ranges
    dt <- merge(dt, missing_dt, by = common_id_cols, all = T)
    dt <- dt[is.na(issue)]

    dt[, issue := NULL]
    for (stem in interval_id_cols_stems[interval_id_cols_stems != col_stem]) {
      dt[, paste0("common_", stem, "_", c("start", "end")) := NULL]
    }
  }

  if (nrow(dt) == 0) {
    stop("`dt` is empty after dropping overlapping intervals and missing data")
  }

  # Do aggregation ----------------------------------------------------------

  groups <- identify_unique_groupings(dt, id_cols, col_stem, col_type,
                                      collapse_interval_cols)

  # create one column to describe each interval in mapping
  if (col_type == "interval") {
    mapping <- copy(mapping)
    gen_name(mapping, col_stem = col_stem, format = "interval")
    data.table::setnames(mapping, paste0(col_stem, "_name"), col_stem)
  }

  # aggregate each unique set of grouping separately
  result_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    grouping <- subset_unique_grouping(dt, id_cols, col_stem, col_type,
                                       collapse_interval_cols, groups,
                                       group_num)

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

      # append already aggregated data to grouping dt so that next subtree has
      # access
      agg_data <- unique(rbind(grouping$dt, target_dt, use.names= T))

      aggregated_same_groupings_dt <- agg_subtree(
        agg_data,
        id_cols,
        value_cols,
        col_stem,
        col_type,
        agg_function,
        subtree,
        missing_dt_severity,
        collapse_interval_cols
      )

      if (col_type == "interval") {
        gen_name(aggregated_same_groupings_dt, col_stem = col_stem,
                 format = "interval")
        setnames(aggregated_same_groupings_dt, paste0(col_stem, "_name"),
                 col_stem)
      }

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
#'   When scaling a `categorical` variable, whether to collapse missing
#'   intermediate levels in `mapping`. Default is 'False' and the function
#'   errors out due to missing data.
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
                  collapse_missing = FALSE,
                  collapse_interval_cols = FALSE) {

  # Validate arguments ------------------------------------------------------

  assert_agg_scale_args(dt, id_cols, value_cols, col_stem,
                        col_type, mapping, agg_function,
                        collapse_interval_cols, "scale")

  original_col_order <- copy(names(dt))
  original_keys <- copy(key(dt))

  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  interval_id_cols <- id_cols[grepl("_start$|_end$", id_cols)]
  interval_id_cols_stems <- unique(gsub("_start$|_end$", "", interval_id_cols))
  categorical_id_cols <- id_cols[!id_cols %in% interval_id_cols]
  by_id_cols <- id_cols[!id_cols %in% cols]

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
                                        collapse_missing, collapse_interval_cols)

  # check for overlapping intervals in dataset
  present_agg_dt <- dt_issues[grepl("aggregate|overlapping", issue)]
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
  missing_dt <- dt_issues[grepl("missing", issue)]
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

  non_hierarchical_data <- dt_issues[issue == "only one interval"]
  if (nrow(non_hierarchical_data) > 0) {
    error_msg <-
      paste0("Some combinations of `dt` do not have a hierarchical structure ",
             "in the `col_stem` columns.\n",
             "* See `collapse_interval_cols` argument if some of the ",
             "interval id variables need to be collapsed.\n")
    stop(error_msg)
  }

  # Do scaling --------------------------------------------------------------

  groups <- identify_unique_groupings(dt, id_cols, col_stem, col_type,
                                      collapse_interval_cols)

  # scale each unique set of grouping separately
  result_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    grouping <- subset_unique_grouping(dt, id_cols, col_stem, col_type,
                                       collapse_interval_cols, groups,
                                       group_num)

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

      scaled_same_groupings_dt <- scale_subtree(
        grouping$dt,
        id_cols,
        value_cols,
        col_stem,
        col_type,
        agg_function,
        subtree,
        missing_dt_severity,
        collapse_interval_cols
      )

      if (col_type == "interval") {
        gen_name(scaled_same_groupings_dt, col_stem = col_stem,
                 format = "interval")
        setnames(scaled_same_groupings_dt, paste0(col_stem, "_name"),
                 col_stem)
      }

      # replace the unscaled children node values with the scaled values where
      # scaling is possible
      id_cols_with_stem <- c(id_cols, if (col_type == "interval") col_stem)
      grouping$dt <- merge(grouping$dt, scaled_same_groupings_dt,
                           by = id_cols_with_stem, all = T)
      children <- names(subtree$children)
      for (value_col in value_cols) {
        grouping$dt[get(col_stem) %in% children &
                      !is.na(get(paste0(value_col, "_scaled"))),
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
#' @return Invisibly returns `dt` but throws error if inputs are not formatted
#'   correctly.
assert_agg_scale_args <- function(dt,
                                  id_cols,
                                  value_cols,
                                  col_stem,
                                  col_type,
                                  mapping,
                                  agg_function,
                                  collapse_interval_cols,
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
  } else {
    assertthat::assert_that(assertive::is_null(mapping),
                            msg = "When scaling an interval variable `mapping`
                                   must be Null. `mapping` is inferred from
                                   `dt`")
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

  # check `collapse_interval_cols` argument
  assertthat::assert_that(assertthat::is.flag(collapse_interval_cols),
                          msg = "`collapse_interval_cols` must be a logical")

  return(invisible(dt))
}
