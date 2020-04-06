library(data.table)
library(testthat)

# Test generating `_end` column -------------------------------------------

id_cols <- c("year", "age_start")
value_cols <- c("value1", "value2")

# set up test input data.table
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

testthat::test_that("generating the age end interval column works", {
  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, col_stem = "age")

  expected_dt <- create_expected_dt()

  testthat::expect_identical(input_dt, expected_dt)
})

# Test generating `_length` column ----------------------------------------

# set up expected output table
modify_expected_dt <- function(dt) {
  dt[year == 2010, age_length := 1]
  dt[year == 2011, age_length := 5]
  dt[is.infinite(age_end), age_length := Inf]
  data.table::setcolorder(dt, c(id_cols, "age_end", "age_length", value_cols))
}

testthat::test_that("generating the age interval length column works", {
  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, col_stem = "age")
  gen_length(input_dt, col_stem = "age")

  expected_dt <- create_expected_dt()
  modify_expected_dt(expected_dt)

  testthat::expect_identical(input_dt, expected_dt)
})

# Test generating `_name` column ------------------------------------------

testthat::test_that("generating the age interval name column doesn't error", {
  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, col_stem = "age")
  testthat::expect_silent(gen_name(input_dt, col_stem = "age",
                                   format = "interval"))

  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, col_stem = "age")
  testthat::expect_silent(gen_name(input_dt, col_stem = "age",
                                   format = "to", format_infinite = "plus"))

  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, col_stem = "age")
  testthat::expect_silent(gen_name(input_dt, col_stem = "age",
                                   format = "to", format_infinite = "+"))

  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, col_stem = "age")
  testthat::expect_silent(gen_name(input_dt, col_stem = "age",
                                   format = "dash", format_infinite = "plus"))

  input_dt <- create_input_dt()
  gen_end(input_dt, id_cols, col_stem = "age")
  testthat::expect_silent(gen_name(input_dt, col_stem = "age",
                                   format = "dash", format_infinite = "+"))
})

# Test that missing intervals are correctly identified --------------------

id_cols <- c("year", "age_start", "age_end")
value_cols <- c("value1", "value2")

# set up standard input data.table
dt <- data.table::data.table(year = 2010,
                             age_start = seq(0, 95, 1),
                             value1 = 1, value2 = 2)
gen_end(dt, id_cols = setdiff(id_cols, "age_end"), col_stem = "age")

# subset to age columns that need to be checked
check_ages <- dt[, list(age_start, age_end)]

test_that("missing age intervals are correctly identified", {
  output <- identify_missing_intervals(check_ages[!age_start %in%
                                                    c(0:4, 85:89, 95)],
                                       data.table(0, Inf))
  expected <- data.table(start = c(0, 85, 95), end = c(5, 90, Inf))
  setkeyv(expected, c("start", "end"))

  expect_identical(output, expected)
})

# Test that overlapping intervals are correctly identified ----------------

# set up standard input data.table
input_dt1 <- data.table::data.table(year = 2010,
                                    age_start = seq(0, 95, 1),
                                    value1 = 1, value2 = 2)
gen_end(input_dt1, id_cols = setdiff(id_cols, "age_end"), col_stem = "age")
input_dt2 <- data.table::data.table(year = 2010,
                                    age_start = seq(0, 95, 5),
                                    value1 = 1, value2= 2)
gen_end(input_dt2, id_cols = setdiff(id_cols, "age_end"), col_stem = "age")
input_dt3 <- data.table::data.table(year = 2010,
                                    age_start = 0,
                                    value1 = 1, value2= 2)
gen_end(input_dt3, id_cols = setdiff(id_cols, "age_end"), col_stem = "age")
dt <- rbind(input_dt1, input_dt2, input_dt3)

# subset to age columns that need to be checked
check_ages <- dt[, list(age_start, age_end)]

expected <- rbind(input_dt2, input_dt3)
expected <- expected[, list(start = age_start, end = age_end)]
setkeyv(expected, c("start", "end"))

test_that("overlapping age intervals are correctly identified", {
  output <- identify_overlapping_intervals(check_ages)
  expect_identical(output, expected)
})

# Test that intervals are collapsed correctly to common set ---------------

id_cols <- c("year_start", "year_end", "sex", "age_start", "age_end")
value_cols <- c("value")

# set up test input data.table
input_dt_male <- CJ(year_start = 2005, year_end = 2010,
                    sex = "male",
                    age_start = seq(0, 95, 5),
                    value = 25)
input_dt_male[age_start == 95, value := 5]
input_dt_female <- CJ(year_start = 2005:2009,
                      sex = "female",
                      age_start = seq(0, 95, 1),
                      value = 1)
gen_end(input_dt_female, setdiff(id_cols, c("year_end", "age_end")),
        col_stem = "year", right_most_endpoint = 2010)
input_dt <- rbind(input_dt_male, input_dt_female)
gen_end(input_dt, setdiff(id_cols, "age_end"), col_stem = "age")
setkeyv(input_dt, id_cols)

expected_dt <- CJ(year_start = 2005, year_end = 2010,
                  sex = c("male", "female"),
                  age_start = seq(0, 95, 5),
                  value = 25)
expected_dt[age_start == 95, value := 5]
gen_end(expected_dt, setdiff(id_cols, "age_end"), col_stem = "age")
setkeyv(expected_dt, id_cols)

test_that("intervals are collapsed correctly to common set", {
  collapsed_dt <- collapse_common_intervals(
    dt = input_dt,
    id_cols = id_cols,
    value_cols = value_cols,
    col_stem = "year"
  )
  collapsed_dt <- collapse_common_intervals(
    dt = collapsed_dt,
    id_cols = id_cols,
    value_cols = value_cols,
    col_stem = "age"
  )
  expect_identical(collapsed_dt, expected_dt)
})
