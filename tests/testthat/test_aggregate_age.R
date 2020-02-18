testthat::context("aggregate_age() tests")

library(data.table)

id_cols <- c("year", "age_start", "age_end")
value_cols <- c("value1", "value2")

# set up standard input data.table
input_dt1 <- data.table::data.table(year = 2010,
                                    age_start = seq(0, 95, 1),
                                    age_end = c(seq(1, 95, 1), 125),
                                    value1 = 1, value2 = 2)
input_dt2 <- data.table::data.table(year = 2011,
                                    age_start = seq(0, 95, 5),
                                    age_end = c(seq(5, 95, 5), 125),
                                    value1 = 1, value2= 2)
input_dt <- rbind(input_dt1, input_dt2, use.names = T)

help_test_aggregate_age <- function(input_dt, expected_dt, target_age_dt) {
  # do aggregation
  output_dt <- aggregate_age(input_dt, id_cols, value_cols, target_age_dt)
  # set up expected output table
  setkeyv(expected_dt, id_cols)

  testthat::expect_equal(output_dt, expected_dt)
}

testthat::test_that("check `aggregate_age()` basic functionality works", {
  # test aggregation of single year age groups
  expected_dt1 <- data.table(year = 2010,
                               age_start = c(0, 15, 85),
                               age_end = c(5, 60, 125),
                               value1 = c(5, 45, 11))
  expected_dt1[, value2 := value1 * 2]
  target_age_dt1 <- unique(expected_dt1[, list(age_start, age_end)])
  help_test_aggregate_age(input_dt1, expected_dt1, target_age_dt1)

  # test aggregation of five year age groups
  expected_dt2 <- data.table(year = 2011,
                               age_start = c(0, 15, 85),
                               age_end = c(5, 60, 125),
                               value1 = c(1, 9, 3))
  expected_dt2[, value2 := value1 * 2]
  target_age_dt2 <- unique(expected_dt2[, list(age_start, age_end)])
  help_test_aggregate_age(input_dt2, expected_dt2, target_age_dt2)

  # test aggregation of both single and five year age groups at same time
  expected_dt <- rbind(expected_dt1, expected_dt2, use.names = T)
  target_age_dt <- unique(expected_dt[, list(age_start, age_end)])
  help_test_aggregate_age(input_dt, expected_dt, target_age_dt)
})
