id_cols <- c("year", "age_start", "age_end")

## correctly formatted data
input_dt1 <- data.table::data.table(year = 2010,
                                    age_start = seq(0, 95, 1),
                                    age_end = c(seq(1, 95, 1), 125),
                                    value = 1)
expected_dt1 <- input_dt1[, list(year, age_start, age_end, error = "expected")]
expected_dt1 <- expected_dt1[year != 2010]
data.table::setkeyv(expected_dt1, c(id_cols, "error"))
input_dt2 <- data.table::data.table(year = 2011,
                                    age_start = seq(0, 95, 5),
                                    age_end = c(seq(5, 95, 5), 125),
                                    value = 1)
expected_dt2 <- copy(expected_dt1)

## incorrectly formatted data
# missing 45-50 age range
input_dt3 <- data.table::data.table(year = 2012,
                                    age_start = c(seq(0, 40, 5), seq(50, 95, 5)),
                                    age_end = c(seq(5, 45, 5), seq(55, 95, 5), 125),
                                    value = 1)
expected_dt3 <- data.table::data.table(year = 2012, age_start = 45,
                                       age_end = 50, error = "missing")
data.table::setkeyv(expected_dt3, c(id_cols, "error"))
# missing 0-5 age range
input_dt4 <- data.table::data.table(year = 2013,
                                    age_start = seq(5, 95, 5),
                                    age_end = c(seq(10, 95, 5), 125),
                                    value = 1)
expected_dt4 <- data.table::data.table(year = 2013, age_start = 0,
                                       age_end = 5, error = "missing")
data.table::setkeyv(expected_dt4, c(id_cols, "error"))
# doesn't go up to 125
input_dt5 <- data.table::data.table(year = 2014,
                                    age_start = seq(0, 95, 5),
                                    age_end = c(seq(5, 95, 5), 100),
                                    value = 1)
expected_dt5 <- data.table::data.table(year = 2014, age_start = 100,
                                       age_end = 125, error = "missing")
data.table::setkeyv(expected_dt5, c(id_cols, "error"))
# includes 15-60 aggregate
input_dt6 <- data.table::data.table(year = 2015,
                                    age_start = seq(0, 95, 5),
                                    age_end = c(seq(5, 95, 5), 125),
                                    value = 1)
temp <- data.table::data.table(year = 2015, age_start = 15, age_end = 60, value = 1)
input_dt6 <- rbind(input_dt6, temp, use.names = T)
expected_dt6 <- data.table::data.table(year = 2015, age_start = 15,
                                       age_end = 60, error = "overlapping")
data.table::setkeyv(expected_dt6, c(id_cols, "error"))
# includes 7-9 age range
input_dt7 <- data.table::data.table(year = 2016,
                                    age_start = seq(0, 95, 5),
                                    age_end = c(seq(5, 95, 5), 125),
                                    value = 1)
temp <- data.table::data.table(year = 2016, age_start = 7, age_end = 9, value = 1)
input_dt7 <- rbind(input_dt7, temp, use.names = T)
expected_dt7 <- data.table::data.table(year = 2016, age_start = c(5, 5, 9),
                                       age_end = c(7, 10, 10),
                                       error = c("missing", "overlapping", "missing"))
data.table::setkeyv(expected_dt7, c(id_cols, "error"))

## combine all together
input_dt <- data.table::rbindlist(list(input_dt1, input_dt2, input_dt3,
                                       input_dt4, input_dt5, input_dt6,
                                       input_dt7), use.names = T)

testthat::test_that("check `assert_age_formatted_dt()` basic functionality works", {
  testthat::expect_silent(assert_age_formatted_dt(input_dt1, id_cols))
  testthat::expect_silent(assert_age_formatted_dt(input_dt2, id_cols))
  testthat::expect_error(assert_age_formatted_dt(input_dt3, id_cols))
  testthat::expect_error(assert_age_formatted_dt(input_dt4, id_cols))
  testthat::expect_error(assert_age_formatted_dt(input_dt5, id_cols))
  testthat::expect_error(assert_age_formatted_dt(input_dt6, id_cols))
  testthat::expect_error(assert_age_formatted_dt(input_dt7, id_cols))
  testthat::expect_error(assert_age_formatted_dt(input_dt, id_cols))
})

testthat::test_that("check `identify_age_misformatted_dt()` basic functionality works", {
  output_dt1 <- identify_age_misformatted_dt(input_dt1, id_cols)
  testthat::expect_identical(expected_dt1, output_dt1)
  output_dt2 <- identify_age_misformatted_dt(input_dt2, id_cols)
  testthat::expect_identical(expected_dt2, output_dt2)
  output_dt3 <- identify_age_misformatted_dt(input_dt3, id_cols)
  testthat::expect_identical(expected_dt3, output_dt3)
  output_dt4 <- identify_age_misformatted_dt(input_dt4, id_cols)
  testthat::expect_identical(expected_dt4, output_dt4)
  output_dt5 <- identify_age_misformatted_dt(input_dt5, id_cols)
  testthat::expect_identical(expected_dt5, output_dt5)
  output_dt6 <- identify_age_misformatted_dt(input_dt6, id_cols)
  testthat::expect_identical(expected_dt6, output_dt6)
  output_dt7 <- identify_age_misformatted_dt(input_dt7, id_cols)
  testthat::expect_identical(expected_dt7, output_dt7)
})
