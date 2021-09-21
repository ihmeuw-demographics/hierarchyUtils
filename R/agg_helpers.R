#' @title Aggregate categorical variables
#'
#' @inheritParams agg
#'
#' @return Invisibly returns `dt` but throws error if inputs are not formatted
#'   correctly.
agg_categorical <- function(dt,
                            id_cols,
                            value_cols,
                            col_stem,
                            col_type,
                            mapping,
                            agg_function = sum,
                            missing_dt_severity = "stop",
                            present_agg_severity = "stop",
                            overlapping_dt_severity = "stop",
                            na_value_severity = "stop",
                            collapse_interval_cols = FALSE,
                            quiet = FALSE) {

  # identify each nonleaf node and its subtree
  subtrees <- create_agg_subtrees(
    mapping = mapping,
    exists = unique(dt[[col_stem]]),
    col_type = col_type
  )

  # create object to collect aggregated results
  result_dt <- dt[0]
  if (col_type == "interval") result_dt <- lapply(1:length(subtrees), function(i) dt[0])

  # aggregate children to parents for sub-trees where aggregation possible
  for (i in 1:length(subtrees)) {

    subtree <- subtrees[[i]]
    if (!quiet) message("Aggregate ", i, " of ", length(subtrees), ": ", subtree$name)

    # categorical aggregation may depend on a previous subtree aggregation results
    # append already aggregated data to grouping dt so that next subtree has access
    agg_data <- dt
    if (col_type == "categorical") {
      agg_data <- rbind(dt, result_dt, use.names= T)
    }

    # check if aggregation is possible given available data
    if (!subtree$agg_possible) {
      if (missing_dt_severity != "skip") {
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
    }

    # check if aggregate is already present
    if (present_agg_severity != "skip") {
      parent <- subtree$name
      parent_dt <- dt[get(col_stem) %in% parent]
      empty_dt <- function(dt) nrow(dt) == 0
      error_msg <-
        paste0("aggregate data is already present.\n",
               "* See `present_agg_severity` argument if it is okay to aggregate
             multiple rows with the available data.\n",
               paste0(capture.output(parent_dt), collapse = "\n"))
      assertive::assert_engine(empty_dt, parent_dt,
                               msg = error_msg, severity = present_agg_severity)

      # drop parent data already present
      if (nrow(parent_dt) > 0) {
        parent_dt[, drop := TRUE]
        dt <- merge(
          dt, parent_dt[, .SD, .SDcols = c(id_cols, "drop")],
          all = TRUE, by = id_cols
        )
        dt <- dt[is.na(drop)]
        dt[, drop := NULL]
      }
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
      overlapping_dt_severity,
      collapse_interval_cols
    )

    if (col_type == "interval") {
      gen_name(aggregated_same_groupings_dt, col_stem = col_stem,
               format = "interval")
      setnames(aggregated_same_groupings_dt, paste0(col_stem, "_name"),
               col_stem)
    }

    # append to final aggregated data to return
    if (col_type == "categorical") {
      result_dt <- rbind(result_dt, aggregated_same_groupings_dt, use.names = T)
    } else if (col_type == "interval") {
      result_dt[[i]] <- aggregated_same_groupings_dt
    }
  }
  if (col_type == "interval") {
    result_dt <- rbindlist(result_dt)
    result_dt[, c(col_stem) := NULL]
  }
  return(result_dt)
}

#' @title Aggregate interval variables
#'
#' @inheritParams agg
#'
#' @return Invisibly returns `dt` but throws error if inputs are not formatted
#'   correctly.
agg_interval <- function(dt,
                         id_cols,
                         value_cols,
                         col_stem,
                         col_type,
                         mapping,
                         agg_function = sum,
                         missing_dt_severity = "stop",
                         present_agg_severity = "stop",
                         overlapping_dt_severity = "stop",
                         na_value_severity = "stop",
                         collapse_interval_cols = FALSE,
                         quiet = FALSE) {

  cols <- paste0(col_stem, "_", c("start", "end"))

  dt_intervals <- unique(dt[, .SD, .SDcols = cols])
  overlapping_dt <- identify_overlapping_intervals(dt_intervals)

  if (nrow(overlapping_dt) > 0) {
    if (!quiet) message("Collapsing ", col_stem, " to the most detailed common set of intervals")
    # collapse entire dataset to most detailed common set of intervals for the
    # {col_stem} variable being aggregated over
    dt <- collapse_common_intervals(
      dt = dt[, .SD, .SDcols = c(id_cols, value_cols)],
      id_cols = id_cols,
      value_cols = value_cols,
      col_stem = col_stem,
      agg_function = agg_function,
      missing_dt_severity = missing_dt_severity,
      overlapping_dt_severity = overlapping_dt_severity,
      include_missing = TRUE
    )
    dt_intervals <- unique(dt[, .SD, .SDcols = cols])
  }

  # create one column to describe each interval
  gen_name(mapping, col_stem = col_stem, format = "interval")
  data.table::setnames(mapping, paste0(col_stem, "_name"), col_stem)

  # create one column to describe each interval
  gen_name(dt_intervals, col_stem = col_stem, format = "interval")
  data.table::setnames(dt_intervals, paste0(col_stem, "_name"), col_stem)
  data.table::setkeyv(dt_intervals, cols)
  dt <- dt[dt_intervals, on = cols, nomatch = 0]

  # create mapping from available interval variables and requested intervals
  interval_tree <- create_agg_interval_tree(
    data_intervals_dt = dt_intervals,
    agg_intervals_dt = mapping,
    col_stem = col_stem
  )
  mapping <- data.tree::ToDataFrameNetwork(interval_tree)

  # identify each nonleaf node and its subtree
  subtrees <- create_agg_subtrees(
    mapping = mapping,
    exists = unique(dt[[col_stem]]),
    col_type = col_type
  )

  # create object to collect aggregated results
  result_dt <- dt[0]
  if (col_type == "interval") result_dt <- lapply(1:length(subtrees), function(i) dt[0])

  # aggregate children to parents for sub-trees where aggregation possible
  for (i in 1:length(subtrees)) {

    subtree <- subtrees[[i]]
    if (!quiet) message("Aggregate ", i, " of ", length(subtrees), ": ", subtree$name)

    # categorical aggregation may depend on a previous subtree aggregation results
    # append already aggregated data to grouping dt so that next subtree has access
    agg_data <- dt
    if (col_type == "categorical") {
      agg_data <- rbind(dt, result_dt, use.names= T)
    }

    # check if aggregation is possible given available data
    if (!subtree$agg_possible) {
      if (missing_dt_severity != "skip") {
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
    }

    # check if aggregate is already present
    if (present_agg_severity != "skip") {
      parent <- subtree$name
      parent_dt <- dt[get(col_stem) %in% parent]
      empty_dt <- function(dt) nrow(dt) == 0
      error_msg <-
        paste0("aggregate data is already present.\n",
               "* See `present_agg_severity` argument if it is okay to aggregate
             multiple rows with the available data.\n",
               paste0(capture.output(parent_dt), collapse = "\n"))
      assertive::assert_engine(empty_dt, parent_dt,
                               msg = error_msg, severity = present_agg_severity)

      # drop parent data already present
      if (nrow(parent_dt) > 0) {
        parent_dt[, drop := TRUE]
        dt <- merge(
          dt, parent_dt[, .SD, .SDcols = c(id_cols, "drop")],
          all = TRUE, by = id_cols
        )
        dt <- dt[is.na(drop)]
        dt[, drop := NULL]
      }
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
      overlapping_dt_severity,
      collapse_interval_cols
    )

    if (col_type == "interval") {
      gen_name(aggregated_same_groupings_dt, col_stem = col_stem,
               format = "interval")
      setnames(aggregated_same_groupings_dt, paste0(col_stem, "_name"),
               col_stem)
    }

    # append to final aggregated data to return
    if (col_type == "categorical") {
      result_dt <- rbind(result_dt, aggregated_same_groupings_dt, use.names = T)
    } else if (col_type == "interval") {
      result_dt[[i]] <- aggregated_same_groupings_dt
    }
  }
  if (col_type == "interval") {
    result_dt <- rbindlist(result_dt)
    result_dt[, c(col_stem) := NULL]
  }
  return(result_dt)
}
