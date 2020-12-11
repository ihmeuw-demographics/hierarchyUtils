#' @title Split unknown groupings
#'
#' @description Split unknown groupings according to observed proportion
#'   and redistribute.
#'
#' @param dt \[`data.table()`\]\cr
#'   Data which includes both known and unknown groupings. Unknown groupings
#'   should be indicated with NA entries for columns defined by `col_stem`.
#' @param value_cols \[`character(1)`\]\cr
#'   Value columns that should be split and distributed.
#' @param col_stem \[`character(1)`\]\cr
#'   The name of the variable that defines the groupings being split. If an
#'   'interval' variable should not include the '_start' or '_end' suffix.
#' @param col_type \[`character(1)`\]\cr
#'   The type of variable that defines the groupings being split. Can be either
#'   'categorical' or 'interval'. Ex: sex is 'categorical' and typically 'age'
#'   is 'interval'.
#' @param mapping \[`character(1)`\]\cr
#'   For 'categorical' variables, defines how different levels of the
#'   hierarchical variable relate to each other.
#' @inheritParams agg
#'
#' @return \[`data.table()`\]\cr
#'   `dt` with unknown groupings split across known groupings, and then removed.
#'
#' @examples
#' # interval
#' dt <- data.table(
#'   age_start = c(0, 1, 2, NA),
#'   age_end = c(1, 2, 3, NA),
#'   population = c(20, 30, 50, 10)
#' )
#' dt <- split_unknown(
#'   dt,
#'   id_cols = c("age_start", "age_end"),
#'   value_cols = "population",
#'   col_stem = "age",
#'   col_type = "interval",
#'   mapping = data.table(age_start = c(0), age_end = c(3))
#' )
#'
#' # categorical
#' dt <- data.table(
#'   sex = c("male", "female", NA),
#'   population = c(25, 75, 10)
#' )
#' dt <- split_unknown(
#'   dt,
#'   id_cols = "sex",
#'   value_cols = "population",
#'   col_stem = "sex",
#'   col_type = "categorical",
#'   mapping = data.table(parent = c("all", "all"), child = c("male", "female")
#' )
#'
#' @export
split_unknown <- function(dt, id_cols, value_cols, col_stem, col_type, mapping) {

  # validate ----------------------------------------------------------------

  # currently unsupported args required for assertion function
  agg_function <- sum
  collapse_interval_cols <- F

  col_stem_var <-
    ifelse(col_type == "interval", paste0(col_stem, "_start"), col_stem)

  assert_agg_scale_args(
    dt[!is.na(get(col_stem_var))],
    id_cols, value_cols, col_stem,
    col_type, mapping, agg_function,
    collapse_interval_cols, "agg"
  )

  demUtils::assert_is_unique_dt(dt, id_cols)


  # prep --------------------------------------------------------------------

  dt <- copy(dt)

  if (col_type == "interval") {

    id_cols_no_col_stem <-
      setdiff(id_cols, c(paste0(col_stem, "_start"), paste0(col_stem, "_end")))
    dt[, unknown := is.na(get(paste0(col_stem, "_start")))]

    assert_no_overlapping_intervals(
      dt[!(unknown), (paste0(col_stem, c("_start", "_end"))), with = F]
    )

  } else if (col_type == "categorical") {

    id_cols_no_col_stem <- setdiff(id_cols, col_stem)
    dt[, unknown := is.na(get(col_stem))]

  }

  assertthat::assert_that(
    nrow(dt[(unknown)]) > 0,
    msg = "No rows with NA values for grouping variable(s)."
  )


  # distribute --------------------------------------------------------------

  # TODO: do aggregation using mapping input?

  # calculate weights
  # TODO: perhaps add option to pass in custom weights
  dt[,
    weight := get(value_cols) / sum(get(value_cols)[!(unknown)]),
    by = id_cols_no_col_stem
  ]

  # distribute unknown according to weights
  dt[,
    (value_cols) := get(value_cols) + sum(get(value_cols)[(unknown)]) * weight,
    by = id_cols_no_col_stem
  ]


  # clean up and return -----------------------------------------------------

  dt <- dt[!(unknown)]
  dt[, c("unknown", "weight") := NULL]

  return(dt)

}
