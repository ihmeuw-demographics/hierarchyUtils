#' @title Identify issues in the input dataset for aggregation and scaling
#'   functions
#'
#' @inheritParams agg
#'
#' @return \[`data.table()`\] with problematic rows in `dt`. Only includes
#'   original `id_cols` columns and a new column "issue" describing each row's
#'   issue.
#'
#' @details
#' [`identify_agg_dt_issues()`] steps:
#' 1. For interval id variables (not including `col_stem`) if
#' `collapse_interval_cols = TRUE`, checks for overlapping intervals and missing
#' intervals and then collapses these interval variables to the most detailed
#' common intervals.
#' 2  If `col_type = "interval"` then also checks for overlapping intervals and
#' missing intervals but doesn't collapse this variable.
#' 3. Creates data.tree from mappings to be used for aggregation.
#' 4. If `col_type = "categorical"` checks for aggregates already present.
#' 5. Identifies any missing nodes in data.tree needed to make aggregations.
#'
#' [`identify_scale_dt_issues()`] steps:
#' 1. For interval id variables (not including `col_stem`) if
#' `collapse_interval_cols = TRUE`, checks for overlapping intervals and missing
#' intervals and then collapses these interval variables to the most detailed
#' common intervals.
#' 2. Creates data.tree from mappings to be used for scaling.
#' 3. Identifies any missing nodes in data.tree needed to make aggregations.
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
                                   drop_present_aggs = FALSE,
                                   collapse_interval_cols = FALSE) {

  # Validate arguments ------------------------------------------------------

  assert_agg_scale_args(dt, id_cols, value_cols, col_stem,
                        col_type, mapping, agg_function,
                        collapse_interval_cols, "agg")

  # check `drop_present_aggs` argument
  assertthat::assert_that(assertthat::is.flag(drop_present_aggs),
                          msg = "`drop_present_aggs` must be a logical")

  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  interval_id_cols <- id_cols[grepl("_start$|_end$", id_cols)]
  interval_id_cols_stems <- unique(gsub("_start$|_end$", "", interval_id_cols))
  categorical_id_cols <- id_cols[!id_cols %in% interval_id_cols]
  by_id_cols <- id_cols[!id_cols %in% cols]

  groups <- identify_unique_groupings(dt, id_cols, col_stem, col_type,
                                      collapse_interval_cols)

  # create one column to describe each interval in mapping
  if (col_type == "interval") {
    mapping <- copy(mapping)
    gen_name(mapping, col_stem = col_stem, format = "interval")
    data.table::setnames(mapping, paste0(col_stem, "_name"), col_stem)
  }

  # Check for aggregates already present or overlapping intervals -----------

  problem_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    check_dt <- NULL
    grouping <- subset_unique_grouping(dt, id_cols, col_stem, col_type,
                                       collapse_interval_cols, groups,
                                       group_num)

    # check all interval variables for overlapping intervals when set to
    # collapse interval variables
    collapse_cols <- c(
      if (collapse_interval_cols) setdiff(interval_id_cols_stems, col_stem),
      if (col_type == "interval") col_stem
    )
    for (stem in collapse_cols) {
      stem_cols <- paste0(stem, "_", c("start", "end"))

      # identify overlapping intervals
      overlapping_dt <- grouping$dt[, identify_overlapping_intervals(
        .SD,
        min(.SD[[stem_cols[1]]]),
        max(.SD[[stem_cols[2]]])
      ), .SDcols = stem_cols, by = setdiff(id_cols, stem_cols)]
      data.table::setnames(overlapping_dt, c("start", "end"), stem_cols)
      overlapping_dt[, issue := "overlapping intervals present"]
      check_dt <- rbind(check_dt, overlapping_dt, use.names = T)

      # drop overlapping intervals
      if (nrow(overlapping_dt) > 0) {
        grouping$dt <- merge(grouping$dt, overlapping_dt, by = id_cols, all = T)
        grouping$dt <- grouping$dt[is.na(issue)]
        grouping$dt[, issue := NULL]
      }

      # identify missing intervals
      common_intervals <- identify_common_intervals(
        dt = grouping$dt,
        id_cols = id_cols,
        col_stem = stem,
        include_missing = TRUE
      )
      missing_dt <- grouping$dt[, identify_missing_intervals(
        .SD,
        common_intervals
      ), .SDcols = stem_cols, by = setdiff(id_cols, stem_cols)]
      data.table::setnames(missing_dt, c("start", "end"), stem_cols)
      missing_dt[, issue := "missing intervals"]
      check_dt <- rbindlist(list(check_dt, missing_dt), use.names = T)

      # collapse to common intervals
      if (stem != col_stem) {
        grouping$dt <- collapse_common_intervals(
          dt = grouping$dt,
          id_cols = c(id_cols, if (col_type == "interval") col_stem),
          value_cols = value_cols,
          col_stem = stem,
          agg_function = agg_function,
          missing_dt_severity = "none", # we've identified these above
          drop_present_aggs = FALSE, # these should already be dropped
          include_missing = TRUE
        )
      }
    }
    # TODO add check for when start of interval is exactly equivalent to end
    # of interval, this isn't possible with left closed right open intervals

    # create mapping from available interval variables and requested intervals
    if (col_type == "interval") {
      interval_tree <- create_agg_interval_tree(grouping$unique_cols,
                                                mapping,
                                                col_stem)
      group_mapping <- data.tree::ToDataFrameNetwork(interval_tree)
    } else {
      group_mapping <- copy(mapping)
    }

    agg_tree <- create_agg_tree(
      mapping = group_mapping,
      exists = grouping$unique_cols[[col_stem]],
      col_type = col_type
    )

    # check for categorical aggregates that already exist
    if (col_type == "categorical") {
      present_nodes <- identify_present_agg(agg_tree)

      if (!is.null(present_nodes)) {
        # expand the groupings dataset to show the present aggregate nodes
        present_dt <- grouping$dt[, list(present = present_nodes),
                                  by = by_id_cols]
        data.table::setnames(present_dt, "present", cols[1])
        present_dt[, issue := "aggregate data present"]
        check_dt <- rbind(check_dt, present_dt, use.names = T)

        # drop present aggregates before checking for missingness
        grouping$dt <- merge(grouping$dt, present_dt, by = id_cols, all = T)
        grouping$dt <- grouping$dt[is.na(issue)]
        grouping$dt[, issue := NULL]
      }
    }

    # check that the variable to be aggregated has all needed nodes
    missing_nodes <- identify_missing_agg(agg_tree)
    if (!is.null(missing_nodes)) {
      # expand the groupings dataset to show the missing nodes
      if (col_type == "categorical") {
        missing_dt <- grouping$dt[, list(missing = missing_nodes), by = by_id_cols]
        data.table::setnames(missing_dt, "missing", cols)
      } else {
        missing_nodes <- name_to_start_end(missing_nodes)
        setDT(missing_nodes)
        missing_dt <- grouping$dt[, data.table(missing_nodes), by = by_id_cols]
        data.table::setnames(missing_dt, c("start", "end"), cols)
      }
      missing_dt[, issue := "missing data"]
      check_dt <- rbind(check_dt, missing_dt, use.names = T)
    }
    return(check_dt)
  })
  problem_dt <- rbindlist(problem_dt, use.names = T)

  if (nrow(problem_dt) > 0) {
    problem_dt <- unique(problem_dt)
    data.table::setcolorder(problem_dt, c(id_cols, "issue"))
    data.table::setkeyv(problem_dt, c(id_cols, "issue"))
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
                                     collapse_missing = FALSE,
                                     collapse_interval_cols = FALSE) {

  # Validate arguments ------------------------------------------------------

  assert_agg_scale_args(dt, id_cols, value_cols, col_stem,
                        col_type, mapping, agg_function,
                        collapse_interval_cols, "scale")

  # check `collapse_missing` argument
  assertthat::assert_that(assertthat::is.flag(collapse_missing),
                          msg = "`collapse_missing` must be a logical")

  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  interval_id_cols <- id_cols[grepl("_start$|_end$", id_cols)]
  interval_id_cols_stems <- unique(gsub("_start$|_end$", "", interval_id_cols))
  categorical_id_cols <- id_cols[!id_cols %in% interval_id_cols]
  by_id_cols <- id_cols[!id_cols %in% cols]

  groups <- identify_unique_groupings(dt, id_cols, col_stem, col_type,
                                      collapse_interval_cols)

  # Check for aggregates already present or overlapping intervals -----------

  problem_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    check_dt <- NULL
    grouping <- subset_unique_grouping(dt, id_cols, col_stem, col_type,
                                       collapse_interval_cols, groups,
                                       group_num)

    # check all interval variables for overlapping intervals when set to
    # collapse interval variables
    collapse_cols <- c(
      if (collapse_interval_cols) setdiff(interval_id_cols_stems, col_stem)
    )
    for (stem in collapse_cols) {
      stem_cols <- paste0(stem, "_", c("start", "end"))

      # identify overlapping intervals
      overlapping_dt <- grouping$dt[, identify_overlapping_intervals(
        .SD,
        min(.SD[[stem_cols[1]]]),
        max(.SD[[stem_cols[2]]])
      ), .SDcols = stem_cols, by = setdiff(id_cols, stem_cols)]
      data.table::setnames(overlapping_dt, c("start", "end"), stem_cols)
      overlapping_dt[, issue := "overlapping intervals present"]
      check_dt <- rbind(check_dt, overlapping_dt, use.names = T)

      # drop overlapping intervals
      if (nrow(overlapping_dt) > 0) {
        grouping$dt <- merge(grouping$dt, overlapping_dt, by = id_cols, all = T)
        grouping$dt <- grouping$dt[is.na(issue)]
        grouping$dt[, issue := NULL]
      }

      # identify missing intervals
      common_intervals <- identify_common_intervals(
        dt = grouping$dt,
        id_cols = id_cols,
        col_stem = stem,
        include_missing = TRUE
      )
      missing_dt <- grouping$dt[, identify_missing_intervals(
        .SD,
        common_intervals
      ), .SDcols = stem_cols, by = setdiff(id_cols, stem_cols)]
      data.table::setnames(missing_dt, c("start", "end"), stem_cols)
      missing_dt[, issue := "missing intervals"]
      check_dt <- rbindlist(list(check_dt, missing_dt), use.names = T)

      # collapse to common intervals
      if (stem != col_stem) {
        grouping$dt <- collapse_common_intervals(
          dt = grouping$dt,
          id_cols = c(id_cols, if (col_type == "interval") col_stem),
          value_cols = value_cols,
          col_stem = stem,
          agg_function = agg_function,
          missing_dt_severity = "none", # we've identified these above
          drop_present_aggs = FALSE, # these should already be dropped
          include_missing = TRUE
        )
      }
    }

    # create mapping from available interval variables and requested intervals
    if (col_type == "interval") {
      interval_tree <- create_scale_interval_tree(grouping$unique_cols,
                                                  col_stem)
      group_mapping <- data.tree::ToDataFrameNetwork(interval_tree)
    } else {
      group_mapping <- copy(mapping)
    }

    if (nrow(group_mapping) > 0 | col_type == "categorical") {
      scale_tree <- create_scale_tree(
        mapping = group_mapping,
        exists = grouping$unique_cols[[col_stem]],
        col_type = col_type,
        collapse_missing = collapse_missing
      )

      # check that the variable to be aggregated has all needed nodes
      missing_nodes <- identify_missing_scale(scale_tree)
      if (!is.null(missing_nodes)) {
        # expand the groupings dataset to show the missing nodes
        if (col_type == "categorical") {
          missing_dt <- grouping$dt[, list(missing = missing_nodes), by = by_id_cols]
          data.table::setnames(missing_dt, "missing", cols)
        } else {
          missing_nodes <- name_to_start_end(missing_nodes)
          setDT(missing_nodes)
          missing_dt <- grouping$dt[, data.table(missing_nodes), by = by_id_cols]
          data.table::setnames(missing_dt, c("start", "end"), cols)
        }
        missing_dt[, issue := "missing data"]
        check_dt <- rbind(check_dt, missing_dt, use.names = T)
      }

      ## check for overlapping intervals in dataset
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

          # get endpoints of all nodes one level below
          start <- subtree$Get("left",
                               filterFun = function(x) subtree$level == x$level - 1)
          end <- subtree$Get("right",
                             filterFun = function(x) subtree$level == x$level - 1)

          overlapping_intervals <- identify_overlapping_intervals(
            data.table(start, end), subtree$left, subtree$right
          )
          data.table::setnames(overlapping_intervals, c("start", "end"), cols)
          overlapping_intervals_dt <- grouping$dt[, data.table(overlapping_intervals),
                                                  by = by_id_cols]
          overlapping_intervals_dt[, issue := "overlapping interval data"]
          return(overlapping_intervals_dt)
        })
        overlapping_dt <- rbindlist(overlapping_dt, use.names = T)
        check_dt <- rbind(check_dt, overlapping_dt, use.names = T)
      }
    } else {
      one_level_dt <- copy(grouping$dt)
      one_level_dt <- one_level_dt[, id_cols, with = F]
      one_level_dt[, issue := "only one interval"]
      check_dt <- rbind(check_dt, one_level_dt, use.names = T)
    }
    return(check_dt)
  })
  problem_dt <- rbindlist(problem_dt, use.names = T, fill = T)

  if (nrow(problem_dt) > 0) {
    problem_dt <- unique(problem_dt)
    data.table::setcolorder(problem_dt, c(id_cols, "issue"))
    data.table::setkeyv(problem_dt, c(id_cols, "issue"))
  } else {
    problem_dt <- data.table(issue = character())
  }
  return(problem_dt)
}
