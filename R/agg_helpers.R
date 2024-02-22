#' @title Helper function to aggregate categorical variables
#'
#' @description Helper function to aggregate categorical variables or square
#'   interval variables datasets
#'
#' @inheritParams agg
#'
#' @return Invisibly returns `dt` but throws error if inputs are not formatted
#'   correctly.
#'
#' @noRd
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
        assertive.base::assert_engine(empty_missing_dt, missing_dt,
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
      assertive.base::assert_engine(empty_dt, parent_dt,
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
    result_dt <- rbindlist(result_dt, use.names = TRUE)
    result_dt[, c(col_stem) := NULL]
  }
  return(result_dt)
}

#' @title Helper function to aggregate interval variables
#'
#' @description Helper function to aggregate interval variables by groups of data
#'   that have the same intervals. Calls `agg_categorical()` for each group.
#'
#' @inheritParams agg
#'
#' @return Invisibly returns `dt` but throws error if inputs are not formatted
#'   correctly.
#'
#' @noRd
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

  # create one column to describe each interval
  gen_name(mapping, col_stem = col_stem, format = "interval")
  data.table::setnames(mapping, paste0(col_stem, "_name"), col_stem)

  # create one column to describe each interval
  dt_intervals <- unique(dt[, .SD, .SDcols = cols])
  gen_name(dt_intervals, col_stem = col_stem, format = "interval")
  data.table::setnames(dt_intervals, paste0(col_stem, "_name"), col_stem)
  data.table::setkeyv(dt_intervals, cols)
  dt <- dt[dt_intervals, on = cols, nomatch = 0]

  # identify unique combinations of intervals to be aggregated
  groups <- dt[, list(col = paste(get(col_stem), collapse = ",")), by = setdiff(names(dt), c(col_stem, cols, value_cols))]
  unique_groups <- unique(groups$col)

  all_groups_dt <- lapply(1:length(unique_groups), function(i) {
    g <- unique_groups[i]
    if (!quiet) message("Interval group ", i, " of ", length(unique_groups), ": ", g)

    group_dt <- groups[col == g]
    group_dt[, col := NULL]

    # get rows of data that have the same combination of intervals
    if (nrow(group_dt) == 0 & length(unique_groups) == 1) {
      # when only one group exists and the only id variables are `col`
      agg_dt <- dt
    } else {
      agg_dt <- dt[group_dt, on = names(group_dt), nomatch = 0]
    }

    dt_intervals <- unique(agg_dt[, .SD, .SDcols = cols])

    # check for overlapping intervals
    if (overlapping_dt_severity != "skip") {
      overlapping_dt <- identify_overlapping_intervals(dt_intervals, identify_all_possible = overlapping_dt_severity != "none")
      data.table::setnames(overlapping_dt, c("start", "end"), cols)
      overlapping_dt[, issue := "overlapping intervals present"]

      empty_dt <- function(dt) nrow(dt) == 0
      error_msg <-
        paste0("Some overlapping intervals were identified in `dt`.\n",
               "Will attempt to drop the larger intervals.\n",
               paste0(capture.output(overlapping_dt), collapse = "\n"))
      assertive.base::assert_engine(empty_dt, overlapping_dt,
                               msg = error_msg, severity = overlapping_dt_severity)

      # drop overlapping intervals
      if (nrow(overlapping_dt) > 0) {
        overlapping_dt <- identify_overlapping_intervals(dt_intervals, identify_all_possible = FALSE)
        data.table::setnames(overlapping_dt, c("start", "end"), cols)
        overlapping_dt[, issue := "overlapping intervals identified"]
        if (!quiet) message(paste0("Dropping overlapping intervals\n",
                                   paste0(capture.output(overlapping_dt), collapse = "\n")))
        agg_dt <- merge(agg_dt, overlapping_dt, by = cols, all = T)
        agg_dt <- agg_dt[is.na(issue)]
        agg_dt[, issue := NULL]
      }
    }

    # create mapping from available interval variables and requested intervals
    dt_intervals <- unique(agg_dt[, .SD, .SDcols = cols])
    gen_name(dt_intervals, col_stem = col_stem, format = "interval")
    data.table::setnames(dt_intervals, paste0(col_stem, "_name"), col_stem)
    interval_tree <- create_agg_interval_tree(
      data_intervals_dt = dt_intervals,
      agg_intervals_dt = mapping,
      col_stem = col_stem
    )
    mapping <- data.tree::ToDataFrameNetwork(interval_tree)

    result_dt <- agg_categorical(
      agg_dt,
      id_cols,
      value_cols,
      col_stem,
      col_type,
      mapping,
      agg_function,
      missing_dt_severity,
      present_agg_severity,
      overlapping_dt_severity,
      na_value_severity,
      collapse_interval_cols,
      quiet
    )
    return(result_dt)
  })

  all_groups_dt <- rbindlist(all_groups_dt, use.names = TRUE)
  return(all_groups_dt)
}
