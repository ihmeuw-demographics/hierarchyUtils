#' @title Determine unique sets of the agg/scale variable present in the input
#'   data.table
#'
#' @inheritParams agg
#'
#' @return list with two named elements.
#'   1. *groupings*: \[`data.table()`\] with `id_cols` (except `col_stem`
#'   columns) and a new column 'available_vars' that is a ';' separated string
#'   for each unique variable present in each combination of `id_cols` (except
#'   `col_stem` columns). If `col_type` is 'interval' then each 'start' and
#'   'end' variable is also ',' separated.
#'   2. *unique_groupings*: \[`data.table()`\] with only one common for the
#'   unique 'available_vars' in the `groupings`.
#'
#' @seealso [subset_unique_grouping()]
identify_unique_groupings <- function(dt,
                                      id_cols,
                                      col_stem,
                                      col_type,
                                      collapse_interval_cols) {

  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  interval_id_cols <- id_cols[grepl("_start$|_end$", id_cols)]
  categorical_id_cols <- id_cols[!id_cols %in% interval_id_cols]
  by_id_cols <- id_cols[!id_cols %in% cols]
  if (collapse_interval_cols) {
    by_id_cols <- by_id_cols[by_id_cols %in% categorical_id_cols]
  }

  # create a temporary column combining the start and end columns for intervals
  if (col_type == "interval") {
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
#'   '`{col_stem}`_start' and '`{col_stem}`_end'
#'   then the 'start' and 'end' variable values are also ',' separated.
#'   2. *dt*: \[`data.table()`\] subset of the input `dt` that has a specific
#'   combination of the variable being aggregated or scaled over.
#'
#' @seealso [identify_unique_groupings()]
subset_unique_grouping <- function(dt,
                                   id_cols,
                                   col_stem,
                                   col_type,
                                   collapse_interval_cols,
                                   groups,
                                   group_num) {

  cols <- col_stem
  if (col_type == "interval") {
    cols <- paste0(col_stem, "_", c("start", "end"))
  }
  interval_id_cols <- id_cols[grepl("_start$|_end$", id_cols)]
  categorical_id_cols <- id_cols[!id_cols %in% interval_id_cols]
  by_id_cols <- id_cols[!id_cols %in% cols]
  if (collapse_interval_cols) {
    by_id_cols <- by_id_cols[by_id_cols %in% categorical_id_cols]
  }
  # identify unique col combinations in this grouping
  group_string <- groups$unique_groupings[group_num, available_vars]
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
  if (nrow(same_groupings) == 0) {
    same_groupings_dt <- copy(dt)
  } else {
    same_groupings_dt <- merge(same_groupings, dt, by = by_id_cols, all.x = T)
  }

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
