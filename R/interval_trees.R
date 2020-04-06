#' @title Create a data.tree object to be used for aggregating or scaling
#'   interval data
#'
#' @param data_intervals_dt \[`data.table()`\]\cr
#'   Describes each interval present in the data. Includes a column for
#'   '`{col_stem}`_start', '`{col_stem}`_end', and `col_stem`.
#' @param agg_intervals_dt \[`data.table()`\]\cr
#'   Describes each interval that needs to be aggregated to. Includes a column
#'   for '`{col_stem}`_start', '`{col_stem}`_end', and `col_stem`.
#' @inheritParams agg
#'
#' @details
#' `create_agg_interval_tree()` returns a `data.tree()` with three levels. The
#' root node is a place holder that covers the entire data interval. The second
#' level contains each aggregate node that needs to be made and each of these
#' aggregate node's have children nodes for the data intervals that are needed
#' for aggregation.
#'
#' `create_scale_interval_tree()` returns a `data.tree()` with a variable number
#' of levels dependent on the intervals available in the data. The root node
#' also covers the entire data interval, this node may or may not actually be in
#' `data_intervals_dt`. Each interval in `data_intervals_dt` is then positioned
#' in the tree so that it is a sub-interval of its parent interval node.
#'
#' @return \[`data.tree()`\] where the 'name' field of each node is in interval
#'   notation and describes each left-closed, right-open interval. Each node
#'   also includes a field for the 'left' and 'right' endpoint.
#'
#' @rdname create_interval_tree
create_agg_interval_tree <- function(data_intervals_dt,
                                     agg_intervals_dt,
                                     col_stem) {

  cols <- paste0(col_stem, "_", c("start", "end"))

  # create the root of the interval tree that covers the full interval
  full_int_start <- min(data_intervals_dt[[cols[1]]],
                        agg_intervals_dt[[cols[1]]])
  full_int_end <- max(data_intervals_dt[[cols[2]]],
                      agg_intervals_dt[[cols[2]]])
  full_int_name <- paste0("[", full_int_start, ", ", full_int_end, ")")
  interval_tree <- create_interval_node(full_int_start, full_int_end,
                                        full_int_name)

  # create each interval node and place in the full interval tree
  for (i_agg in 1:nrow(agg_intervals_dt)) {
    new_agg_node <- create_interval_node(agg_intervals_dt[i_agg, get(cols[1])],
                                         agg_intervals_dt[i_agg, get(cols[2])],
                                         agg_intervals_dt[i_agg, get(col_stem)])
    # subset to data intervals that are in the aggregate interval
    child_ints <- data_intervals_dt[get(cols[1]) >= new_agg_node$left &
                                      get(cols[2]) <= new_agg_node$right]
    if (nrow(child_ints) > 0) {
      for (i_sub in 1:nrow(child_ints)) {
        new_child_node <- create_interval_node(child_ints[i_sub, get(cols[1])],
                                               child_ints[i_sub, get(cols[2])],
                                               child_ints[i_sub, get(col_stem)])
        new_agg_node$AddChildNode(new_child_node)
      }
    }
    interval_tree$AddChildNode(new_agg_node)
  }

  # check each aggregate node (level 2) and fill in any missing child intervals
  subtrees <- data.tree::Traverse(interval_tree,
                                  filterFun = function(x) x$level == 2)
  fill_missing_intervals(interval_tree, subtrees, col_stem)

  return(interval_tree)
}

#' @rdname create_interval_tree
create_scale_interval_tree <- function(data_intervals_dt, col_stem) {

  cols <- paste0(col_stem, "_", c("start", "end"))

  # create the root of the interval tree that covers the full interval
  full_int_start <- min(data_intervals_dt[[cols[1]]])
  full_int_end <- max(data_intervals_dt[[cols[2]]])
  full_int_name <- paste0("[", full_int_start, ", ", full_int_end, ")")
  interval_tree <- create_interval_node(full_int_start, full_int_end,
                                        full_int_name)

  # if the full interval is included in `data_intervals_dt` then drop it
  # since the root interval node is already made above
  if (nrow(data_intervals_dt[get(cols[1]) == full_int_start &
                             get(cols[2]) == full_int_end]) > 0) {
    data_intervals_dt <- data_intervals_dt[!(get(cols[1]) == full_int_start &
                                               get(cols[2]) == full_int_end)]
  }

  # create each interval node and place in the full interval tree
  for (i in 1:nrow(data_intervals_dt)) {
    new_node <- create_interval_node(data_intervals_dt[i, get(cols[1])],
                                     data_intervals_dt[i, get(cols[2])],
                                     data_intervals_dt[i, get(col_stem)])
    place_new_interval_node(interval_tree, new_node)
  }

  # check each non-leaf node and fill in any missing child intervals
  subtrees <- data.tree::Traverse(
    interval_tree, filterFun = function(x) data.tree::isNotLeaf(x)
  )
  fill_missing_intervals(interval_tree, subtrees, col_stem)

  return(interval_tree)
}

#' @title Create a node for an interval tree
#'
#' @param start  \[`numeric(1)`\]\cr
#'   the left endpoint of the interval tree node.
#' @param end  \[`numeric(1)`\]\cr
#'   the right endpoint of the interval tree node.
#' @param name \[`character(1)`\]\cr
#'   name of the node in interval notation.
#'
#' @return \[`data.tree()`\] node with 'name', 'left', 'right' fields.
create_interval_node <- function(start, end, name) {
  new_node <- data.tree::Node$new(name)
  new_node$Set(left = start)
  new_node$Set(right = end)
  return(new_node)
}

#' @title Place a new interval node in an interval tree
#'
#' @description Recursive function to place a new interval node in an interval
#'   tree.
#'
#' @param current_node \[`data.tree()`\]\cr
#'   node of interval tree to put the `new_node` in.
#' @param new_node \[`data.tree()`\]\cr
#'   new interval node to place somewhere below `current_node`.
#'
#' @details
#' Assumption is that `new_node` is a sub interval of `current_node` and
#' this is double checked.
#'
#' `new_node` can be placed below `current_node` as:
#' * another child node of `current_node`.
#' * another child node of `current_node` but with one of `current_node`'s
#'   children placed as a child of `new_node`.
#' * somewhere below one of `current_node`'s children.
#'
#' @return Invisibly returns reference to modified `current_node` with
#'   `new_node` placed as part of subtree.
place_new_interval_node <- function(current_node, new_node) {
  if (data.tree::isLeaf(current_node)) {
    current_node$AddChildNode(new_node)

  } else {

    # identify any children nodes that are sub intervals of the new node
    sub_interval_nodes <- data.tree::Traverse(
      current_node,
      filterFun = function(x) current_node$level == x$level - 1 &
        new_node$left <= x$left & new_node$right >= x$right
    )

    # identify child node that is parent interval of the new node
    parent_interval_node <- data.tree::Traverse(
      current_node,
      filterFun = function(x) current_node$level == x$level - 1 &
        x$left <= new_node$left & x$right >= new_node$right
    )

    if (length(sub_interval_nodes) > 0) {
      # insert new node in between current node and each of the sub interval
      # child nodes
      for (child_node in sub_interval_nodes) {
        current_node$RemoveChild(name = child_node$name)
        new_node$AddChildNode(child_node)
        current_node$AddChildNode(new_node)
      }
    } else if (length(parent_interval_node) > 0) {
      # place new interval node somewhere below the current child node
      for (child_node in parent_interval_node) {
        place_new_interval_node(child_node, new_node)
      }
    } else {
      # if new node didn't overlap with any other interval nodes then add a new
      # child interval node
      current_node$AddChildNode(new_node)
    }
  }
  return(invisible(current_node))
}

#' Identify missing or overlapping intervals
#'
#' @param ints_dt \[`data.table()`\]\cr
#'   Unique intervals to check as returned by [subset_unique_grouping()].
#'   Includes a column for the start of the interval and the end of the
#'   interval.
#' @param expected_ints_dt \[`data.table()`\]\cr
#'   Expected intervals that should be completely included in `ints_dt`.
#'   Includes a column for the start of the interval and the end of the
#'   interval.
#'
#' @return  \[`data.table()`\] with columns for 'start' and 'end' of intervals
#'   where intervals in `ints_dt` are missing or overlapping. If no intervals
#'   are missing or overlapping then zero-row \[`data.table()`\] is returned.
#'
#' @rdname problematic_intervals
identify_missing_intervals <- function(ints_dt, expected_ints_dt) {

  assertive::is_data.table(ints_dt)
  assertive::is_data.table(expected_ints_dt)

  # create full interval that all sub intervals should span
  expected_ints <- intervals::Intervals_full(as.matrix(expected_ints_dt),
                                             closed = c(TRUE, FALSE))

  # create left-closed, right-open intervals
  ints <- intervals::Intervals_full(as.matrix(ints_dt), closed = c(TRUE, FALSE))

  # identify missing intervals
  missing_ints <- intervals::interval_difference(expected_ints, ints)

  missing_ints_dt <- data.table::as.data.table(missing_ints)
  data.table::setnames(missing_ints_dt, c("start", "end"))
  data.table::setkeyv(missing_ints_dt, c("start", "end"))
  return(missing_ints_dt)
}

#' @rdname problematic_intervals
identify_overlapping_intervals <- function(ints_dt,
                                           full_int_start = 0,
                                           full_int_end = Inf) {

  assertive::is_data.table(ints_dt)
  assertthat::is.number(full_int_start)
  assertthat::is.number(full_int_end)

  # create left-closed, right-open intervals
  ints <- intervals::Intervals_full(as.matrix(ints_dt), closed = c(TRUE, FALSE))

  # get list mapping between intervals if they overlap at all
  overlaps <- intervals::interval_overlap(ints, ints)
  names(overlaps) <- 1:length(overlaps)

  # sort by number of intervals that each interval overlaps with so that we can
  # identify the largest overlapping intervals first
  overlaps <- overlaps[order(sapply(overlaps, length), decreasing=T)]

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
  overlapping_ints <- ints[as.integer(overlapping_indices)]

  overlapping_ints_dt <- data.table::as.data.table(overlapping_ints)
  data.table::setnames(overlapping_ints_dt, c("start", "end"))
  data.table::setkeyv(overlapping_ints_dt, c("start", "end"))
  return(overlapping_ints_dt)
}

#' @title create new interval nodes for any missing interval nodes
#'
#' @description Check that the children nodes cover the entire parent interval node
#'   and create new interval nodes for any missing ranges.
#'
#' @param interval_tree \[`data.tree()`\]\cr
#'   interval tree containing the subtree to be modified
#' @param subtrees \[`list(data.tree())`\]\cr
#'   non-leaf subtrees to check and fill any missing intervals.
#' @inheritParams agg
#'
#' @return invisibly return reference to modified subtrees.
fill_missing_intervals <- function(interval_tree, subtrees, col_stem) {

  cols <- paste0(col_stem, "_", c("start", "end"))

  for (agg_node in subtrees) {

    # get endpoints of subtree leaves
    start <- agg_node$Get("left",
                          filterFun = function(x) data.tree::isLeaf(x))
    end <- agg_node$Get("right",
                        filterFun = function(x) data.tree::isLeaf(x))

    missing_intervals <- identify_missing_intervals(
      data.table(start, end), data.table(agg_node$left, agg_node$right)
    )
    data.table::setnames(missing_intervals, c("start", "end"), cols)
    gen_name(missing_intervals, col_stem = col_stem, format = "interval")
    data.table::setnames(missing_intervals, paste0(col_stem, "_name"), col_stem)

    # add any missing interval nodes
    if (nrow(missing_intervals) > 0) {
      for (i_missing in 1:nrow(missing_intervals)) {
        missing_child_node <- create_interval_node(missing_intervals[i_missing, get(cols[1])],
                                                   missing_intervals[i_missing, get(cols[2])],
                                                   missing_intervals[i_missing, get(col_stem)])
        agg_node$AddChildNode(missing_child_node)
      }
    }
  }
  data.tree::Sort(interval_tree, attribute = "left")
  return(invisible(interval_tree))
}
