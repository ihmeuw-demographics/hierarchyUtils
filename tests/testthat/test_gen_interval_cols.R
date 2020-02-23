# set up standard input data.table
create_input_dt <- function() {
  input_dt1 <- data.table::data.table(year = 2010,
                                      age_start = 0:95,
                                      value1 = 1, value2 = 2)
  input_dt2 <- data.table::data.table(year = 2011,
                                      age_start = seq(0, 95, 5),
                                      value1 = 1, value2 = 2)
  input_dt <- rbind(input_dt1, input_dt2, use.names = T)
  data.table::setkeyv(input_dt, c("year", "age_start"))
  return(input_dt)
}
id_cols <- c("year", "age_start")
value_cols <- c("value1", "value2")

# set up expected output table
create_expected_dt <- function() {
  expected_dt1 <- data.table::data.table(year = 2010,
                                         age_start = 0:95,
                                         age_end = c(1:95, Inf),
                                         value1 = 1, value2 = 2)
  expected_dt2 <- data.table::data.table(year = 2011,
                                         age_start = seq(0, 95, 5),
                                         age_end = c(seq(5, 95, 5), Inf),
                                         value1 = 1, value2 = 2)
  expected_dt <- rbind(expected_dt1, expected_dt2, use.names = T)
  data.table::setkeyv(expected_dt, c("year", "age_start", "age_end"))
  return(expected_dt)
}

testthat::test_that("check `gen_end()` basic functionality works", {
  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")

  expected_dt <- create_expected_dt()

  testthat::expect_identical(input_dt, expected_dt)
})

testthat::test_that("check `calculate_age_end()` errors are thrown for different cases", {
  input_dt <- create_input_dt()

  # Check error thrown when wrong argument types are given
  testthat::expect_error(gen_end(data.frame(input_dt), id_cols))
  testthat::expect_error(gen_end(input_dt, 5))
  testthat::expect_error(gen_end(input_dt, id_cols, terminal_age_end = "test"))

  # check error thrown when id_cols does not include 'age_start'
  testthat::expect_error(gen_end(input_dt, setdiff(id_cols, "age_start")))

  # check error thrown when id_cols does not uniquely identify all rows
  testthat::expect_error(gen_end(input_dt, setdiff(id_cols, "year")))

  # check error thrown when 'age_start' column is incorrectly formatted
  input_dt <- create_input_dt()
  input_dt[, age_start := as.character(age_start)]
  testthat::expect_error(calculate_age_end(input_dt, id_cols))
  input_dt <- create_input_dt()
  input_dt[, age_start := NA]
  testthat::expect_error(calculate_age_end(input_dt, id_cols))
})

# set up expected output table
modify_expected_dt <- function(dt) {
  dt[year == 2010, age_length := 1]
  dt[year == 2011, age_length := 5]
  dt[is.infinite(age_end), age_length := Inf]
  data.table::setcolorder(dt, c(id_cols, "age_end", "age_length", value_cols))
}

testthat::test_that("check `gen_length()` basic functionality works", {
  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")
  gen_length(input_dt, base_col = "age")

  expected_dt <- create_expected_dt()
  modify_expected_dt(expected_dt)

  testthat::expect_identical(input_dt, expected_dt)
})

testthat::test_that("check `gen_length()` errors are thrown for different cases", {
  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")

  # Check error thrown when wrong argument types are given
  testthat::expect_error(gen_length(data.frame(input_dt)))

  # check error thrown when 'age_start' or 'age_end columns are incorrectly formatted
  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")
  input_dt[, age_start := as.character(age_start)]
  testthat::expect_error(gen_length(input_dt))
  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")
  input_dt[, age_end := as.character(age_end)]
  testthat::expect_error(gen_length(input_dt))
  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")
  input_dt[, age_start := NA]
  testthat::expect_error(gen_length(input_dt))
})

testthat::test_that("check `gen_name()` basic functionality works", {
  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")
  testthat::expect_silent(gen_name(input_dt, base_col = "age",
                                   format = "interval"))

  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")
  testthat::expect_silent(gen_name(input_dt, base_col = "age",
                                   format = "to", format_infinite = "plus"))

  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")
  testthat::expect_silent(gen_name(input_dt, base_col = "age",
                                   format = "to", format_infinite = "+"))

  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")
  testthat::expect_silent(gen_name(input_dt, base_col = "age",
                                   format = "dash", format_infinite = "plus"))

  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, base_col = "age")
  testthat::expect_silent(gen_name(input_dt, base_col = "age",
                                   format = "dash", format_infinite = "+"))
})
