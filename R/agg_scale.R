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

  dt <- copy(dt)
  mapping <- copy(mapping)

  # Do aggregation ----------------------------------------------------------

  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))

    # collapse entire dataset to most detailed common set of intervals for the
    # {col_stem} variable being aggregated over
    dt <- collapse_common_intervals(
      dt = dt[, .SD, .SDcols = c(id_cols, value_cols)],
      id_cols = id_cols,
      value_cols = value_cols,
      col_stem = col_stem,
      agg_function = agg_function,
      missing_dt_severity = missing_dt_severity,
      include_missing = TRUE
    )

    # create one column to describe each interval
    gen_name(mapping, col_stem = col_stem, format = "interval")
    data.table::setnames(mapping, paste0(col_stem, "_name"), col_stem)
    gen_name(dt, col_stem = col_stem, format = "interval")
    data.table::setnames(dt, paste0(col_stem, "_name"), col_stem)

    # create mapping from available interval variables and requested intervals
    dt_intervals <- unique(dt[, .SD, .SDcols = c(cols, col_stem)])
    interval_tree <- create_agg_interval_tree(
      data_intervals_dt = dt_intervals,
      agg_intervals_dt = mapping,
      col_stem = col_stem
    )
    mapping <- data.tree::ToDataFrameNetwork(interval_tree)
  }

  # identify each nonleaf node and its subtree
  subtrees <- create_agg_subtrees(
    mapping = mapping,
    exists = unique(dt[[col_stem]]),
    col_type = col_type
  )

  # aggregate children to parents for sub-trees where aggregation possible
  result_dt <- dt[0]
  for (subtree in subtrees) {

    # append already aggregated data to grouping dt so that next subtree has
    # access
    agg_data <- rbind(dt, result_dt, use.names= T)

    # check if aggregation is possible given available data
    if (!subtree$agg_possible) {
      missing_nodes <- subtree$Get("name", filterFun = function(x) {
        !x$exists & identical(x$parent, subtree)
      })
      missing_dt <- data.table(col_stem = missing_nodes)
      data.table::setnames(missing_dt, "col_stem", col_stem)
      empty_missing_dt <- function(dt) nrow(dt) == 0
      error_msg <-
        paste0("expected input data is missing.\n",
               "* See `missing_dt_severity` argument if it is okay to only make ",
               "aggregate/scale data that are possible given what is available.\n",
               paste0(capture.output(missing_dt), collapse = "\n"))
      assertive::assert_engine(empty_missing_dt, missing_dt,
                               msg = error_msg, severity = missing_dt_severity)

      # skip aggregation for this subtree
      next()
    }

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
    result_dt <- rbind(result_dt, aggregated_same_groupings_dt, use.names = T)
  }
  if (col_type == "interval") result_dt[, c(col_stem) := NULL]

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

  dt <- copy(dt)
  mapping <- copy(mapping)

  # Do scaling --------------------------------------------------------------

  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))

    # create one column to describe each interval
    gen_name(dt, col_stem = col_stem, format = "interval")
    data.table::setnames(dt, paste0(col_stem, "_name"), col_stem)

    # create mapping from available interval variables
    dt_intervals <- unique(dt[, .SD, .SDcols = c(cols, col_stem)])
    interval_tree <- create_scale_interval_tree(
      data_intervals_dt = dt_intervals,
      col_stem = col_stem
    )
    mapping <- data.tree::ToDataFrameNetwork(interval_tree)
  }

  # identify each nonleaf node (and its subtree)
  subtrees <- create_scale_subtrees(
    mapping = mapping,
    exists = unique(dt[[col_stem]]),
    col_type = col_type,
    collapse_missing = collapse_missing
  )

  # scale children to parents for subtrees where scaling is possible
  for (subtree in subtrees) {

    # check if aggregation is possible given available data
    if (!subtree$scale_children_possible) {
      missing_nodes <- subtree$Get("name", filterFun = function(x) {
        !x$exists
      })
      missing_dt <- data.table(col_stem = missing_nodes)
      data.table::setnames(missing_dt, "col_stem", col_stem)
      empty_missing_dt <- function(dt) nrow(dt) == 0
      error_msg <-
        paste0("expected input data is missing.\n",
               "* See `missing_dt_severity` argument if it is okay to only make ",
               "aggregate/scale data that are possible given what is available.\n",
               paste0(capture.output(missing_dt), collapse = "\n"))
      assertive::assert_engine(empty_missing_dt, missing_dt,
                               msg = error_msg, severity = missing_dt_severity)

      # skip aggregation for this subtree
      next()
    }

    scaled_same_groupings_dt <- scale_subtree(
      dt,
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
    dt <- merge(dt, scaled_same_groupings_dt,
                by = id_cols_with_stem, all = T)
    children <- names(subtree$children)
    for (value_col in value_cols) {
      dt[get(col_stem) %in% children & !is.na(get(paste0(value_col, "_scaled"))),
         paste0(value_col) := get(paste0(value_col, "_scaled"))]
      dt[, paste0(value_col, "_scaled") := NULL]
    }
  }
  if (col_type == "interval") dt[, c(col_stem) := NULL]

  data.table::setcolorder(dt, original_col_order)
  data.table::setkeyv(dt, original_keys)
  return(dt)
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
    demUtils::assert_is_unique_dt(mapping, expected_mapping_cols)
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
  demUtils::assert_is_unique_dt(dt, id_cols)
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
