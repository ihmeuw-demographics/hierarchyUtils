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
      # TODO add check for when start of interval is exactly equivalent to end
      # of interval, this isn't possible with left closed right open intervals
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
