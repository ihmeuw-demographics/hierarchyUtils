#' Aggregate sex-specific counts to both sexes combined
#'
#' @param dt data.table with data to be aggregated.
#'   * must only contain columns specified in `id_cols` and `value_cols`.
#'   * must include a column called 'sex', and only include observations for
#'   'female' and 'male' but not 'both'.
#'   * each combination of `id_cols` (not including 'sex') must include a row for 'female' and 'male'.
#' @param id_cols character vector of id columns that uniquely identify each row
#'   of `dt`.
#'   * must include 'sex'.
#' @param value_cols character vector of value columns to be aggregated.
#'
#' @return data.table with `id_cols` and `value_cols` columns, includes additional
#'   rows for both sexes combined.
#'
#' @export
#'
#' @examples
#' input_dt <- data.table::CJ(year = 2010:2020, sex = c("female", "male"),
#'                            value1 = 1, value2 = 2)
#' output_dt <- aggregate_sex(dt = input_dt, id_cols = c("year", "sex"),
#'                            value_cols = c("value1", "value2"))
#'
aggregate_sex <- function(dt, id_cols, value_cols) {

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  assertthat::assert_that("sex" %in% id_cols, msg = "`id_cols` must include 'sex'.")
  sex_collapse_cols <- id_cols[!id_cols %in% "sex"]

  # check `value_cols` argument
  assertive::assert_is_character(value_cols)

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  for (value_col in value_cols) {
    assertive::assert_is_numeric(dt[[value_col]])
  }
  capture.output(assertable::assert_colnames(dt, c(id_cols, value_cols), only_colnames = T))
  expected_sexes <- c("female", "male")
  assertable::assert_values(dt, colnames = "sex", test = "in",
                            test_val = expected_sexes, quiet = T)

  # check that each combination of `id_cols` (not including 'sex') includes a row for 'female' and 'male'
  dt[, check := identical(sort(sex), sort(expected_sexes)), by = sex_collapse_cols]
  if(!all(dt$check)) {
    message(
      "There are combinations of `id_cols` (not including 'sex') in `dt` that do not include a row for 'female' and 'male':\n",
      paste0(capture.output(dt[!(check)]), collapse = "\n")
    )
    stop("each combination of `id_cols` (not including 'sex') must include a row for 'female' and 'male'.")
  }
  dt[, check := NULL]

  original_keys <- key(dt)
  if (is.null(original_keys)) original_keys <- id_cols

  # calculate both sexes combined aggregate
  both_sexes_dt <- dt[, lapply(.SD, sum), .SDcols = value_cols, by = sex_collapse_cols]
  both_sexes_dt[, sex := "both"]
  dt <- rbind(dt, both_sexes_dt, use.names = T)
  setkeyv(dt, original_keys)

  return(dt)
}
