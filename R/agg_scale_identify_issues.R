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
  interval_id_cols <- id_cols[grepl("_start$|_end$", id_cols)]
  interval_id_cols_stems <- unique(gsub("_start$|_end$", "", interval_id_cols))
  categorical_id_cols <- id_cols[!id_cols %in% interval_id_cols]
  by_id_cols <- id_cols[!id_cols %in% cols]

  groups <- identify_unique_groupings(dt, id_cols, col_stem, col_type)

  # Check for aggregates already present or overlapping intervals -----------

  aggs_present_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    check_dt <- NULL
    grouping <- subset_unique_grouping(dt, id_cols, col_stem, col_type, groups,
                                       group_num)

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
    } else {
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
    }
    return(check_dt)
  })
  aggs_present_dt <- rbindlist(aggs_present_dt, use.names = T)

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

    check_dt <- NULL
    grouping <- subset_unique_grouping(dt, id_cols, col_stem, col_type, groups,
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

    # check that interval id variables cover same range in each subtree
    subtrees <- create_agg_subtrees(group_mapping,
                                    grouping$unique_cols[[col_stem]], col_type)
    for (subtree in subtrees) {

      children <- names(subtree$children)
      subtree_dt <- grouping$dt[get(col_stem) %in% children]

      for (stem in interval_id_cols_stems[interval_id_cols_stems != col_stem]) {

        common_intervals <- identify_common_intervals(subtree_dt, id_cols, stem)
        collapsed_dt <- merge_common_intervals(subtree_dt, common_intervals, stem)

        # check for missing intervals when attempting to collapse common intervals
        cols_stem <- paste0(stem, "_", c("start", "end"))
        by_id_cols_stem <- id_cols[!id_cols %in% cols_stem]
        missing <- collapsed_dt[, identify_missing_intervals(.SD, common_intervals),
                                .SDcols = cols_stem,
                                by = by_id_cols_stem]
        data.table::setnames(missing, c("start", "end"), cols_stem)
        check_dt <- rbindlist(list(check_dt, missing), use.names = T)
      }
    }

    # check that the variable to be aggregated has all needed nodes
    agg_tree <- create_agg_tree(group_mapping,
                                grouping$unique_cols[[col_stem]], col_type)
    missing_nodes <- identify_missing_agg(agg_tree)

    if (!is.null(missing_nodes)) {
      # expand the groupings dataset to show the missing nodes
      if (col_type == "categorical") {
        missing <- grouping$dt[, list(missing = missing_nodes), by = by_id_cols]
        data.table::setnames(missing, "missing", cols)
      } else {
        missing_nodes <- name_to_start_end(missing_nodes)
        setDT(missing_nodes)
        missing <- grouping$dt[, data.table(missing_nodes), by = by_id_cols]
        data.table::setnames(missing, c("start", "end"), cols)
      }
      check_dt <- rbindlist(list(check_dt, missing), use.names = T)
    }
    return(check_dt)
  })
  missing_dt <- rbindlist(missing_dt, use.names = T)

  if (nrow(missing_dt) > 0) {
    missing_dt[, issue := "missing data"]
  }

  # Combine together identified problems ------------------------------------

  problem_dt <- rbindlist(list(aggs_present_dt, missing_dt), use.names = T, fill = T)
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
  interval_id_cols <- id_cols[grepl("_start$|_end$", id_cols)]
  interval_id_cols_stems <- unique(gsub("_start$|_end$", "", interval_id_cols))
  categorical_id_cols <- id_cols[!id_cols %in% interval_id_cols]
  by_id_cols <- id_cols[!id_cols %in% cols]

  groups <- identify_unique_groupings(dt, id_cols, col_stem, col_type)

  # Check for aggregates already present or overlapping intervals -----------

  problem_dt <- lapply(1:nrow(groups$unique_groupings), function(group_num) {

    grouping <- subset_unique_grouping(dt, id_cols, col_stem, col_type, groups,
                                       group_num)

    # create mapping from available interval variables and requested intervals
    if (col_type == "interval") {
      interval_tree <- create_scale_interval_tree(grouping$unique_cols,
                                                  col_stem)
      group_mapping <- data.tree::ToDataFrameNetwork(interval_tree)
    } else {
      group_mapping <- copy(mapping)
    }

    # check that interval id variables cover same range in each subtree
    missing_intervals_ids_dt <- NULL
    subtrees <- create_scale_subtrees(group_mapping,
                                      grouping$unique_cols[[col_stem]],
                                      col_type, collapse_missing)
    for (subtree in subtrees) {

      parent <- subtree$name
      children <- names(subtree$children)

      parent_dt <- grouping$dt[get(col_stem) %in% parent]
      children_dt <- grouping$dt[get(col_stem) %in% children]
      subtree_dt <- rbind(parent_dt, children_dt, use.names = T)

      for (stem in interval_id_cols_stems) {

        common_intervals <- identify_common_intervals(subtree_dt, id_cols, stem)
        collapsed_parent_dt <- merge_common_intervals(parent_dt, common_intervals, stem)
        collapsed_children_dt <- merge_common_intervals(children_dt, common_intervals, stem)

        # check for missing intervals when attempting to collapse common intervals
        cols_stem <- paste0(stem, "_", c("start", "end"))
        by_id_cols_stem <- id_cols[!id_cols %in% cols_stem]

        missing_parent <- collapsed_parent_dt[, identify_missing_intervals(.SD, common_intervals),
                                              .SDcols = cols_stem,
                                              by = by_id_cols_stem]
        data.table::setnames(missing_parent, c("start", "end"), cols_stem)
        missing_parent[, issue := "missing data"]

        missing_children <- collapsed_children_dt[, identify_missing_intervals(.SD, common_intervals),
                                                  .SDcols = cols_stem,
                                                  by = by_id_cols_stem]
        data.table::setnames(missing_children, c("start", "end"), cols_stem)
        missing_children[, issue := "missing data"]

        missing_intervals_ids_dt <- rbindlist(list(missing_intervals_ids_dt,
                                                   missing_parent,
                                                   missing_children),
                                              use.names = T)
      }
    }
    missing_intervals_ids_dt <- unique(missing_intervals_ids_dt)

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
      overlapping_dt <- rbindlist(overlapping_dt, use.names = T)
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
    check_dt <- rbindlist(list(overlapping_dt, missing_dt, missing_intervals_ids_dt),
                          use.names = T)
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
