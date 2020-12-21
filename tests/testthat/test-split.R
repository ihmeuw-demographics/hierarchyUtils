
library(data.table)

dt <- data.table(
  age_start = c(0, 1, 2, NA),
  age_end = c(1, 2, 3, NA),
  population = c(20, 30, 50, 10)
)

expected_output <- data.table(
  age_start = c(0, 1, 2),
  age_end = c(1, 2, 3),
  population = c(22, 33, 55)
)

id_cols <- c("age_start", "age_end")
value_cols <- "population"
col_stem <- "age"
col_type <- "interval"
mapping <- data.table(age_start = c(0), age_end = c(3))

test_that("splitting unknown intervals works", {
  output <- expect_silent(
    split_unknown(
      dt, id_cols, value_cols, col_stem, col_type, mapping
    )
  )
  expect_equal(output, expected_output)
})


dt <- data.table(
  sex = c("male", "female", NA),
  population = c(25, 75, 10)
)

expected_output <- data.table(
  sex = c("male", "female"),
  population = c(27.5, 82.5)
)

id_cols <- c("sex")
value_cols <- "population"
col_stem <- "sex"
col_type <- "categorical"
mapping <- data.table(parent = c("all", "all"), child = c("male", "female"))

test_that("splitting unknown categories works", {
  output <- expect_silent(
    split_unknown(
      dt, id_cols, value_cols, col_stem, col_type, mapping
    )
  )
  expect_equal(output, expected_output)
})
