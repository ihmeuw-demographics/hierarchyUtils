#' Aggregate age-specific counts to aggregate age groups
#'
#' @param dt data.table with data to be aggregated.
#' * must only contain columns specified in `id_cols` and `value_cols`.
#' * must include 'age_start' and 'age_end' columns.
#' * each combination of `id_cols` must uniquely identify each row.
#' @param id_cols character vector of id columns that uniquely identify each row
#' of `dt`.
#' * must include 'age_start' and 'age_end'.
#' @param value_cols character vector of value columns to be aggregated.
#' @param target_ages_dt data.table defining age groups to aggregate to.
#' * must include 'age_start' and 'age_end' columns.
#'
#' @return data.table with `id_cols` and `value_cols` columns, includes
#' additional rows for aggregate age groups combined.
#'
#' @export
#'
#' @examples
#' input_dt <- data.table::data.table(year = 2010,
#'                                    age_start = seq(0, 95, 1),
#'                                    age_end = c(seq(1, 95, 1), 125),
#'                                    value1 = 1, value2 = 2)
#' target_dt <- data.table::data.table(age_start = c(0, 15, 85),
#'                                     age_end = c(5, 60, 125))
#' output_dt <- aggregate_age(input_dt,
#'                            id_cols = c("year", "age_start", "age_end"),
#'                            value_cols = c("value1", "value2"),
#'                            target_ages_dt = target_dt)
#'
aggregate_age <- function(dt, id_cols, value_cols, target_ages_dt) {

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  assertthat::assert_that("age_start" %in% id_cols & "age_end" %in% id_cols,
                          msg = "`id_cols` must include 'sex'.")

  # check `value_cols` argument
  assertive::assert_is_character(value_cols)

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  for (value_col in value_cols) {
    assertive::assert_is_numeric(dt[[value_col]])
  }
  capture.output(assertable::assert_colnames(dt, c(id_cols, value_cols), only_colnames = T))
  assert_is_unique_dt(dt, id_cols)

  # check `target_ages_dt` argument
  assertive::assert_is_data.table(target_ages_dt)
  capture.output(assertable::assert_colnames(dt, c("age_start", "age_end"), only_colnames = F))
  assert_is_unique_dt(target_ages_dt, id_cols = c("age_start", "age_end"))

  setkeyv(dt, id_cols)
  by_id_cols <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # rather than aggregating each id_col grouping separately,
  # faster to aggregate by each unique set of groups in the dt

  # determine unique sets of age groups present in dt
  age_groups <- dt[, list(available_age_starts = paste(sort(unique(age_start)), collapse = ",")), by = by_id_cols]
  unique_age_groupings <- unique(age_groups[, c("available_age_starts"), with = F])

  # aggregate each unique age grouping separately
  aggregated_dt <- lapply(1:nrow(unique_age_groupings), function(i) {
    target_age_grouping_string <- unique_age_groupings[i, available_age_starts]
    target_age_grouping <- data.table(age_start = as.numeric(strsplit(target_age_grouping_string, split = ",")[[1]]))
    calculate_age_end(target_age_grouping, id_cols = "age_start")

    # subset to the data for this age grouping
    same_age_groupings <- age_groups[available_age_starts == target_age_grouping_string]
    same_age_groupings[, available_age_starts := NULL]
    same_age_groupings_dt <- merge(same_age_groupings, dt, by = by_id_cols, all.x = T)

    assert_age_formatted_dt(same_age_groupings_dt, id_cols,
                            first_age_start = min(target_age_grouping$age_start),
                            terminal_age_end = max(target_age_grouping$age_end))

    # aggregate to all the target age groups
    target_dt <- lapply(1:nrow(target_ages_dt), function(i) {
      target_age_start <- target_ages_dt[i, age_start]
      target_age_end <- target_ages_dt[i, age_end]

      # subset to age groups for the given target age group
      subset_dt <- same_age_groupings_dt[age_start >= target_age_start & age_start < target_age_end &
                                           age_end > target_age_start & age_end <= target_age_end]

      # aggregate remaining rows
      subset_dt <- subset_dt[, lapply(.SD, sum), .SDcols = value_cols, by = by_id_cols]

      # assign target age groups 'age_start' and 'age_end' columns
      subset_dt[, age_start := target_age_start]
      subset_dt[, age_end := target_age_end]

      return(subset_dt)
    })
    target_dt <- rbindlist(target_dt)
    return(target_dt)
  })
  aggregated_dt <- rbindlist(aggregated_dt)
  dt <- rbind(dt, aggregated_dt, use.names = T)
  setkeyv(dt, id_cols)
  return(dt)
}
