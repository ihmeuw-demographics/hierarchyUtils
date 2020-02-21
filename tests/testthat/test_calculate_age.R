# set up standard input data.table
create_input_dt <- function() {
  input_dt1 <- data.table::data.table(location = "France", year = 2010,
                                      sex = "female",
                                      age_start = 0:95,
                                      value1 = 1, value2 = 2)
  input_dt2 <- data.table::data.table(location = "Ethiopia", year = 2010,
                                      sex = "female",
                                      age_start = seq(0, 95, 5),
                                      value1 = 1, value2 = 2)
  input_dt <- rbind(input_dt1, input_dt2, use.names = T)
  return(input_dt)
}
id_cols <- c("location", "year", "sex", "age_start")
value_cols <- c("value1", "value2")

# set up expected output table
expected_dt1 <- data.table::data.table(location = "France", year = 2010,
                                       sex = "female",
                                       age_start = 0:95,
                                       value1 = 1, value2 = 2,
                                       age_end = c(1:95, 125))
expected_dt2 <- data.table::data.table(location = "Ethiopia", year = 2010,
                                       sex = "female",
                                       age_start = seq(0, 95, 5),
                                       value1 = 1, value2 = 2,
                                       age_end = c(seq(5, 95, 5), 125))
expected_dt <- rbind(expected_dt1, expected_dt2, use.names = T)
setkeyv(expected_dt, c(id_cols, "age_end"))

test_that("check `calculate_age_end()` basic functionality works", {
  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
  testthat::expect_identical(input_dt, expected_dt)
})

test_that("check `calculate_age_end()` errors are thrown for different cases", {
  input_dt <- create_input_dt()

  # Check error thrown when wrong argument types are given
  testthat::expect_error(calculate_age_end(data.frame(input_dt), id_cols))
  testthat::expect_error(calculate_age_end(input_dt, 5))
  testthat::expect_error(calculate_age_end(input_dt, id_cols, terminal_age_end = "test"))

  # check error thrown when id_cols does not include 'age_start'
  testthat::expect_error(calculate_age_end(input_dt, setdiff(id_cols, "age_start")))

  # check error thrown when id_cols does not uniquely identify all rows
  testthat::expect_error(calculate_age_end(input_dt, setdiff(id_cols, "location")))

  # check error thrown when 'age_start' column is incorrectly formatted
  input_dt <- create_input_dt()
  input_dt[, age_start := as.character(age_start)]
  testthat::expect_error(calculate_age_end(input_dt, id_cols))
  input_dt <- create_input_dt()
  input_dt[, age_start := NA]
  testthat::expect_error(calculate_age_end(input_dt, id_cols))
})

# set up expected output table
expected_dt1[, age_int := 1]
expected_dt1[age_end == 125, age_int := 30]
expected_dt2[, age_int := 5]
expected_dt2[age_end == 125, age_int := 30]
expected_dt <- rbind(expected_dt1, expected_dt2, use.names = T)
setkeyv(expected_dt, c(id_cols, "age_end"))

test_that("check `calculate_age_int()` basic functionality works", {
  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
  calculate_age_int(input_dt)
  testthat::expect_identical(input_dt, expected_dt)
})

test_that("check `calculate_age_end()` errors are thrown for different cases", {
  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)

  # Check error thrown when wrong argument types are given
  testthat::expect_error(calculate_age_int(data.frame(input_dt)))

  # check error thrown when 'age_start' or 'age_end columns are incorrectly formatted
  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
  input_dt[, age_start := as.character(age_start)]
  testthat::expect_error(calculate_age_int(input_dt))
  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
  input_dt[, age_end := as.character(age_end)]
  testthat::expect_error(calculate_age_int(input_dt))
  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
  input_dt[, age_start := NA]
  testthat::expect_error(calculate_age_int(input_dt))
})

test_that("check `calculate_age_name()` basic functionality works", {
  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
  testthat::expect_silent(calculate_age_name(input_dt, format = "interval"))

  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
  testthat::expect_silent(calculate_age_name(input_dt, format = "to", terminal_format = "plus"))

  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
  testthat::expect_silent(calculate_age_name(input_dt, format = "to", terminal_format = "+"))

  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
  testthat::expect_silent(calculate_age_name(input_dt, format = "dash", terminal_format = "plus"))

  input_dt <- create_input_dt()
  calculate_age_end(input_dt, id_cols, terminal_age_end = 125)
  testthat::expect_silent(calculate_age_name(input_dt, format = "dash", terminal_format = "+"))

})
