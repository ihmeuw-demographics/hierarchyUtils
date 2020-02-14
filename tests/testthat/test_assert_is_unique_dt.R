testthat::context("assert_is_unique_dt() tests")

input_dt1 <- data.table::data.table(location = "France", year = 2010,
                                    sex = "female",
                                    age_start = 0:95,
                                    value1 = 1, value2 = 2)
input_dt2 <- data.table::data.table(location = "France", year = 2010,
                                    sex = "female",
                                    age_start = 0:95,
                                    value1 = 2, value2 = 4)
input_dt <- rbind(input_dt1, input_dt2)
id_cols <- c("location", "year", "sex", "age_start")

testthat::test_that("check `assert_is_unique_dt()` basic functionality works", {
  testthat::expect_silent(assert_is_unique_dt(input_dt1, id_cols))
  testthat::expect_silent(assert_is_unique_dt(input_dt2, id_cols))
  testthat::expect_error(assert_is_unique_dt(input_dt, id_cols))
})
