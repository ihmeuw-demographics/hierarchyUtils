#' @title Generate columns to help describe numeric variable intervals
#'
#' @description demUtils assumes numeric interval variables are grouped into
#' left-closed, right-open intervals. \eqn{a <= x < b}. Each interval can be
#' described by their endpoints from which interval lengths and nicer formatted
#' interval names can be created.
#'
#' @param dt \[`data.table()`\]\cr
#'   `col_stem`-specific data.
#' @param id_cols \[`character()`\]\cr
#'   ID columns that uniquely identify each row of `dt`. This must include
#'   `{col_stem}_start`.
#' @param col_stem \[`character(1)`\]\cr
#'   Base name of the numeric variable column. Does not include the '_start',
#'   '_end' etc. suffix.
#' @param right_most_endpoint \[`numeric(1)`\]\cr
#'   Assumed right most endpoint of `{col_stem}_end`. Default is \code{Inf}.
#' @param format \[`character(1)`\]\cr
#'   Formatting style for the interval names. Default is 'to'; can also be
#'   'interval' or 'dash'.
#' @param format_infinite \[`character(1)`\]\cr
#'   Formatting style for infinite endpoint intervals. Default is 'plus'; can
#'   also be '+'. Ignored when `format = "interval"`.
#'
#' @return Invisibly returns reference to modified `dt`.
#'
#' @details `gen_end` generates a new column `{col_stem}_end` for the
#' right-open endpoint of each interval from a series of left-closed endpoints
#' `{col_stem}_start`.
#'
#' `gen_end` assumes that only the most detailed intervals are present in the
#' input dataset; including overlapping intervals will not return expected
#' results.
#'
#' Input data `dt` for `gen_end` must:
#'   * Contain all columns specified in `id_cols`.
#'   * Have a column called `{col_stem}_start`.
#'   * Have each row uniquely identified by each combination of `id_cols`.
#'
#' `gen_length` generates a new column `{col_stem}_length` for the length of
#' each interval. Input data `dt` for `gen_length` must contain
#' `{col_stem}_start` and `{col_stem}_end` columns.
#'
#' `gen_name` generates a new column `{col_stem}_name` describing each interval.
#'
#' Formatting style for intervals:
#'   * \eqn{[a, b)} interval notation is used when `format = 'interval`.
#'   * `a to b` is used when `format = "to"`.
#'   * `a-b` is used when `format = "dash"`.
#'
#' Formatting style for infinite endpoint interval:
#'   * \eqn{[a, Inf)} interval notation is used when `format = 'interval`.
#'   * `a plus` is used when `format_infinite = "plus"`.
#'   * `a+` is used when `format = "+"`.
#'
#' @examples
#' input_dt <- data.table::data.table(location = "France", year = 2010,
#'                                    sex = "female",
#'                                    age_start = 0:95,
#'                                    value1 = 1, value2 = 2)
#' id_cols <- c("location", "year", "sex", "age_start")
#' gen_end(input_dt, id_cols, col_stem = "age")
#' gen_length(input_dt, col_stem = "age")
#' gen_name(input_dt, col_stem = "age")
#'
#' @export
#' @rdname gen_interval_cols
gen_end <- function(dt, id_cols, col_stem, right_most_endpoint = Inf) {

  # Validate arguments ------------------------------------------------------

  # check `col_stem` argument
  assertthat::assert_that(assertthat::is.string(col_stem),
                          !grepl("_(start|end)$", col_stem),
                          msg = "`col_stem` must be a string that does not
                          include the suffix '_start' or '_end'")
  start_col <- paste0(col_stem, "_start")
  end_col <- paste0(col_stem, "_end")

  # check extreme endpoints arguments
  assertthat::assert_that(assertthat::is.number(right_most_endpoint),
                          msg = "`right_most_endpoint` must be a length one
                          numeric")

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  assertthat::assert_that(start_col %in% id_cols,
                          msg = "`id_cols` must include `{col_stem}_start`")

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, id_cols, only_colnames = F, quiet = T)
  assertthat::assert_that(!end_col %in% names(dt),
                          msg = paste0("'", end_col,
                                       "' column already in `dt`"))
  assertive::assert_is_numeric(dt[[start_col]])
  assertive::assert_all_are_not_na(dt[[start_col]])
  assert_is_unique_dt(dt, id_cols)

  # Calculate left-closed endpoints -----------------------------------------

  by_id_cols <- id_cols[!id_cols %in% start_col]
  original_col_order <- copy(names(dt))

  original_keys <- copy(key(dt))
  if (setequal(id_cols, original_keys)) {
    # if id_cols are already used as keys, also include new end_col
    original_keys <- c(original_keys, end_col)
  } else {
    # temporarily sort by id_cols
    data.table::setkeyv(dt, id_cols)
  }

  data.table::setnames(dt, start_col, "start_col")
  if (!assertive::is_integer(right_most_endpoint)) {
    dt[, start_col := as.numeric(start_col)]
  }
  dt[, end_col := data.table::shift(start_col, type = "lead",
                                    fill = right_most_endpoint),
     by = by_id_cols]

  # Format result -----------------------------------------------------------

  # clean up temporary column names
  data.table::setnames(dt, "end_col", end_col)
  data.table::setnames(dt, "start_col", start_col)

  # check new column
  assertive::assert_is_numeric(dt[[end_col]])
  assertive::assert_all_are_not_na(dt[[end_col]])

  # put end_col to the right of start_col
  new_col_order <- append(original_col_order, end_col,
                          match(start_col, original_col_order))
  data.table::setcolorder(dt, new_col_order)
  data.table::setkeyv(dt, original_keys)

  return(invisible(dt))
}

#' @export
#' @rdname gen_interval_cols
gen_length <- function(dt, col_stem) {

  # Validate arguments ------------------------------------------------------

  # check `col_stem` argument
  assertthat::assert_that(assertthat::is.string(col_stem),
                          !grepl("_(start|end|length)$", col_stem),
                          msg = "`col_stem` must be a string that does not
                          include the suffix '_start', '_end', '_length'.")
  start_col <- paste0(col_stem, "_start")
  end_col <- paste0(col_stem, "_end")
  length_col <- paste0(col_stem, "_length")

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c(start_col, end_col),
                              only_colnames = F, quiet = T)
  assertthat::assert_that(!length_col %in% names(dt),
                          msg = paste0("'", length_col,
                                       "' column already in `dt`"))
  assertive::assert_is_numeric(dt[[start_col]])
  assertive::assert_all_are_not_na(dt[[start_col]])
  assertive::assert_is_numeric(dt[[end_col]])
  assertive::assert_all_are_not_na(dt[[end_col]])

  # Calculate age interval column -------------------------------------------

  original_col_order <- copy(names(dt))

  dt[, length_col := get(end_col) - get(start_col)]

  # Format result -----------------------------------------------------------

  # clean up temporary column name
  data.table::setnames(dt, "length_col", length_col)

  # check new column
  assertive::assert_is_numeric(dt[[length_col]])
  assertive::assert_all_are_not_na(dt[[length_col]])

  # put length_col to the right of end_col
  new_col_order <- append(original_col_order, length_col,
                          match(end_col, original_col_order))
  data.table::setcolorder(dt, new_col_order)

  return(invisible(dt))
}

#' @export
#' @rdname gen_interval_cols
gen_name <- function(dt,
                     col_stem,
                     format = "to",
                     format_infinite = "plus",
                     right_most_endpoint = Inf) {

  # Validate arguments ------------------------------------------------------

  # check `col_stem` argument
  assertthat::assert_that(assertthat::is.string(col_stem),
                          !grepl("_(start|end|name)$", col_stem),
                          msg = "`col_stem` must be a string that does not
                          include the suffix '_start', '_end' or '_name'")
  start_col <- paste0(col_stem, "_start")
  end_col <- paste0(col_stem, "_end")
  name_col <- paste0(col_stem, "_name")

  # check `format` argument
  assertthat::assert_that(assertthat::is.string(format),
                          checkmate::checkChoice(format,
                                                 choices = c("interval", "to",
                                                             "dash")),
                          msg = "`format` must be a string and one of
                          'interval', 'to', or 'dash'")

  # check `format_infinite` argument
  assertthat::assert_that(assertthat::is.string(format_infinite),
                          checkmate::checkChoice(format_infinite,
                                                 choices = c("plus", "+")),
                          msg = "`format_infinite` must be a string and one of
                          'plus' or '+'")

  # check extreme endpoints arguments
  assertthat::assert_that(assertthat::is.number(right_most_endpoint),
                          msg = "`right_most_endpoint` must be a length one
                          numeric")

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c(start_col, end_col), only_colnames = F,
                              quiet = T)
  assertthat::assert_that(!name_col %in% names(dt),
                          msg = paste0("'", name_col,
                                       "' column already in `dt`"))
  assertive::assert_is_numeric(dt[[start_col]])
  assertive::assert_all_are_not_na(dt[[start_col]])
  assertive::assert_is_numeric(dt[[end_col]])
  assertive::assert_all_are_not_na(dt[[end_col]])

  # Calculate age name column -----------------------------------------------

  original_col_order <- copy(names(dt))

  if (format == "to") {
    dt[, name_col := paste0(get(start_col), " to ", get(end_col))]
  } else if (format == "dash") {
    dt[, name_col := paste0(get(start_col), "-", get(end_col))]
  } else if (format == "interval") {
    dt[, name_col := paste0("[", get(start_col), ", ", get(end_col), ")")]
  }

  if (format == "interval") {
    dt[get(end_col) == right_most_endpoint,
       name_col := paste0("[", get(start_col), ", Inf)")]
  } else {
    terminal_string <- ifelse(format_infinite == "plus", " plus", "+")
    dt[get(end_col) == right_most_endpoint,
       name_col := paste0(get(start_col), terminal_string)]
  }

  # Format result -----------------------------------------------------------

  # clean up temporary column name
  data.table::setnames(dt, "name_col", name_col)

  # check new column
  assertive::assert_is_character(dt[[name_col]])
  assertive::assert_all_are_not_na(dt[[name_col]])

  # put name_col to the right of end_col
  new_col_order <- append(original_col_order, name_col,
                          match(end_col, original_col_order))
  data.table::setcolorder(dt, new_col_order)

  return(invisible(dt))
}

#' Parse interval notation name to interval endpoints
#'
#' @param name \[`character()`\]\cr
#'   left-closed, right-open interval notation as described in `gen_name().
#'
#' @return \[`list()`\] with element for 'start' containing a numeric vector of
#'  left-closed endpoints and another element for 'end' containing a numeric
#'  vector of right-opens endpoints.
#'
#' @export
name_to_start_end <- function(name) {

  # Validate arguments ------------------------------------------------------

  # check `name` argument
  assertthat::assert_that(assertive::is_character(name),
                          all(grepl("^\\[[0-9]+,\\s[Inf|0-9]+\\)", name)),
                          msg = "`name` must be a character vector formatted in
                          left-closed, right-open interval notation as described
                          in `gen_name()`")

  # Parse start and end of interval -----------------------------------------

  start <- as.numeric(gsub("^\\[|,\\s\\w+\\)", "", name))
  end <- as.numeric(gsub("^\\[[0-9]+,\\s|\\)", "", name))
  result <- list(start = start,
                 end = end)
  return(result)
}

#' @title Create a data.tree object to be used for aggregating or scaling
#'   interval data
#'
#' @param data_intervals_dt \[`data.table()`\]\cr
#'   Describes each interval present in the data. Includes a column for
#'   `{col_stem}_start`, `{col_stem}_end`, and `col_stem`.
#' @param agg_intervals_dt \[`data.table()`\]\cr
#'   Describes each interval that needs to be aggregated to. Includes a column
#'   for `{col_stem}_start`, `{col_stem}_end`, and `col_stem`.
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
  full_int_start <- min(data_intervals_dt[[cols[1]]])
  full_int_end <- max(data_intervals_dt[[cols[2]]])
  full_int_name <- paste0("[", full_int_start, ", ", full_int_end, ")")
  interval_tree <- create_interval_node(full_int_start, full_int_end,
                                        full_int_name)

  # subset to aggregate intervals that can be made given available intervals
  data_ints <- intervals::Intervals_full(
    as.matrix(data_intervals_dt[, cols, with = F]),
    closed = c(TRUE, FALSE)
  )
  data_ints <- intervals::interval_intersection(data_ints)
  agg_ints <- intervals::Intervals_full(
    as.matrix(agg_intervals_dt[, cols, with = F]),
    closed = c(TRUE, FALSE)
  )
  included_ints <- intervals::interval_included(data_ints, agg_ints)
  included_ints_rows <- included_ints[, 1]
  agg_intervals_dt <- agg_intervals_dt[included_ints_rows]

  # create each interval node and place in the full interval tree
  for (i_agg in 1:nrow(agg_intervals_dt)) {
    new_agg_node <- create_interval_node(agg_intervals_dt[i_agg, get(cols[1])],
                                         agg_intervals_dt[i_agg, get(cols[2])],
                                         agg_intervals_dt[i_agg, get(col_stem)])
    # subset to data intervals that are in the aggregate interval
    child_ints <- data_intervals_dt[get(cols[1]) >= new_agg_node$left &
                                      get(cols[2]) <= new_agg_node$right]
    for (i_sub in 1:nrow(child_ints)) {
      new_child_node <- create_interval_node(child_ints[i_sub, get(cols[1])],
                                             child_ints[i_sub, get(cols[2])],
                                             child_ints[i_sub, get(col_stem)])
      new_agg_node$AddChildNode(new_child_node)
    }
    interval_tree$AddChildNode(new_agg_node)
  }
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

  current_interval <- intervals::Intervals(c(current_node$left,
                                             current_node$right),
                                           closed = c(TRUE, FALSE))
  new_interval <- intervals::Intervals(c(new_node$left,
                                         new_node$right),
                                       closed = c(TRUE, FALSE))

  # check if new node is it part of current node
  check_new_included <- intervals::interval_included(current_interval,
                                                     new_interval)[[1]]
  if (length(check_new_included) != 0) {

    if (data.tree::isLeaf(current_node)) {
      current_node$AddChildNode(new_node)

    } else {
      part_of_child_node <- FALSE
      # TODO faster way to identify children nodes to check

      # check if new node is a sub interval of any of current node's children
      # or if new node is in between current node and current node's children
      for (current_child_node in current_node$children) {
        current_child_interval <- intervals::Intervals(
          c(current_child_node$left,current_child_node$right),
          closed = c(TRUE, FALSE)
        )

        # check if new node is parent interval of current child node
        new_parent_interval <- intervals::interval_included(
          new_interval,
          current_child_interval
        )
        new_parent_interval <- length(new_parent_interval[[1]]) != 0

        # check if new node is sub interval of current child node
        new_sub_interval <- intervals::interval_included(
          current_child_interval,
          new_interval
        )
        new_sub_interval <- length(new_sub_interval[[1]]) != 0

        if (new_parent_interval) {
          # insert new node in between current node and current child node
          current_node$RemoveChild(name = current_child_node$name)
          new_node$AddChildNode(current_child_node)
          current_node$AddChildNode(new_node)
          part_of_child_node <- TRUE
        } else if (new_sub_interval) {
          # place new interval node somewhere below the current child node
          place_new_interval_node(current_child_node, new_node)
          part_of_child_node <- TRUE
        }
      }

      # if wasn't part of any of the current children nodes then add a new child
      if (!part_of_child_node) {
        current_node$AddChildNode(new_node)
      }
    }
  } else {
    stop("`new_node` can't be placed in interval tree")
  }
  return(invisible(current_node))
}

#' Identify missing or overlapping intervals
#'
#' @param ints_dt \[`data.table()`\]\cr
#'   Unique intervals to check as returned by [subset_unique_grouping()].
#'   Includes a column for the start of the interval and the end of the
#'   interval.
#' @param full_int_start \[`numeric(1)`\]\cr
#'   Start of the complete interval that should be covered by the detailed
#'   intervals in `ints_dt`.
#' @param full_int_end \[`numeric(1)`\]\cr
#'   End of the complete interval that should be covered by the detailed
#'   intervals in `ints_dt`.
#'
#' @return  \[`data.table()`\] with columns for 'start' and 'end' of intervals
#'   where intervals in `ints_dt` are missing or overlapping. If no intervals
#'   are missing or overlapping then zero-row \[`data.table()`\] is returned.
#'
#' @rdname problematic_intervals
identify_missing_intervals <- function(ints_dt,
                                       full_int_start = 0,
                                       full_int_end = Inf) {

  assertive::is_data.table(ints_dt)
  assertthat::is.number(full_int_start)
  assertthat::is.number(full_int_end)

  # create full interval that all sub intervals should span
  full_int <- intervals::Intervals_full(
    matrix(c(full_int_start, full_int_end), ncol = 2)
  )

  # create left-closed, right-open intervals
  ints <- intervals::Intervals_full(as.matrix(ints_dt), closed = c(TRUE, FALSE))

  # identify missing intervals
  missing_ints <- intervals::interval_difference(full_int, ints)

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
