testthat::context("aggregate_sex() tests")

# set up standard input data.table
input_dt <- data.table::CJ(location = "France", year = 2010:2020,
                           sex = c("female", "male"),
                           value1 = 1, value2 = 2)
id_cols <- c("location", "year", "sex")
value_cols <- c("value1", "value2")
setkeyv(input_dt, id_cols)

# set up expected output table
expected_dt <- data.table::CJ(location = "France", year = 2010:2020,
                              sex = c("both"),
                              value1 = 2, value2 = 4)
setkeyv(expected_dt, id_cols)

test_that("check `aggregate_sex()` basic functionality works", {
  output_dt <- aggregate_sex(input_dt, id_cols, value_cols)
  testthat::expect_identical(output_dt, expected_dt)
})

test_that("check `aggregate_sex()` errors are thrown for different cases", {
  # Check error thrown when wrong argument types are given
  testthat::expect_error(aggregate_sex(input_dt, 5, value_cols))
  testthat::expect_error(aggregate_sex(input_dt, id_cols, 5))
  testthat::expect_error(aggregate_sex(data.frame(input_dt), id_cols, value_cols))

  # Check error thrown when `id_cols` + `value_cols` are not equal to cols in `dt`
  testthat::expect_error(aggregate_sex(input_dt, id_cols = setdiff(id_cols, "sex"), value_cols))
  testthat::expect_error(aggregate_sex(input_dt, id_cols = setdiff(id_cols, "location"), value_cols))
  testthat::expect_error(aggregate_sex(input_dt, id_cols = id_cols, c(value_cols, "value3")))
  testthat::expect_error(aggregate_sex(input_dt, id_cols = c(id_cols, "age"), value_cols))

  # Check error thrown when input dt is missing some rows for 'female' or 'male'
  missing_female_dt <- input_dt[!(year == max(year) & sex == "female")]
  testthat::expect_error(suppressMessages(aggregate_sex(missing_female_dt, id_cols, value_cols)))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(input_dt, input_dt)
  testthat::expect_error(aggregate_sex(non_unique_input_dt, id_cols, value_cols))

  # Check that `value_cols` are numeric columns in `dt`
  non_numeric_dt <- copy(input_dt)
  non_numeric_dt[, value1 := NULL]
  non_numeric_dt[, value1 := "error"]
  testthat::expect_error(aggregate_sex(non_numeric_dt, id_cols, value_cols))

  # Check error thrown when both sexes combined already exists in dataset
  testthat::expect_error(capture.output(aggregate_sex(expected_dt, id_cols, value_cols)))
})
