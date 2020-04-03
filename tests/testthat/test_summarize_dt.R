library(data.table)
library(testthat)

# set up test input data.table
input_dt <- CJ(location = "USA",
               year = 1950:2010,
               draw = 1:101)
input_dt[, value := draw - 1]

# set up expected output table
expected_dt <- CJ(location = sort(unique(input_dt$location)),
                  year = sort(unique(input_dt$year)),
                  mean = 50, median = 50, sd = sd(0:100),
                  min = 0, max = 100,
                  q2.5 = 2.5, q10 = 10, q90 = 90, q97.5 = 97.5)
setkeyv(expected_dt, c("location", "year"))

test_that("calculating summary statistics works", {
  output_dt <- summarize_dt(dt = input_dt,
                            id_cols = c("location", "year", "draw"),
                            summarize_cols = c("draw"),
                            value_col = "value",
                            summary_fun = c("mean", "median", "sd", "min", "max"),
                            probs = c(0.025, 0.1, 0.9, 0.975))
  expect_identical(output_dt, expected_dt)
})
