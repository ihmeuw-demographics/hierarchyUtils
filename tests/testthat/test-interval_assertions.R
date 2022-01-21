
# Missing intervals -------------------------------------------------------

test_missing_intervals <- function(description,
                                   dt, id_cols, expected_ints_dt,
                                   drop_age_starts) {

  testthat::test_that(description, {

    missing_dt <- identify_missing_intervals_dt(
      dt = dt[!age_start %in% drop_age_starts],
      id_cols = id_cols,
      col_stem = "age",
      expected_ints_dt = expected_ints_dt
    )
    testthat::expect_equal(missing_dt, dt[age_start %in% drop_age_starts, id_cols, with = FALSE])

    testthat::expect_error(
      assert_no_missing_intervals_dt(
        dt = dt[!age_start %in% drop_age_starts],
        id_cols = id_cols,
        col_stem = "age",
        expected_ints_dt = expected_ints_dt
      ),
      regexp = "There are missing intervals"
    )

    testthat::expect_error(
      assert_no_missing_intervals_dt(
        dt = dt,
        id_cols = id_cols,
        col_stem = "age",
        expected_ints_dt = expected_ints_dt
      ),
      NA
    )
  })
}

# test multiple groupings
drop_age_starts <- c(0, 10, 95)
id_cols <- c("year", "age_start", "age_end")

input_dt <- data.table(
  year = c(rep(2010, 20), rep(2015, 96)),
  age_start = c(seq(0, 95, 5), seq(0, 95, 1)),
  age_end = c(seq(5, 95, 5), Inf, seq(1, 95, 1), Inf),
  value = 1
)
setkeyv(input_dt, id_cols)

expected_ints_dt <- data.table(
  age_start = 0,
  age_end = Inf
)

test_missing_intervals(
  description = "missing intervals are identified correctly",
  input_dt, id_cols, expected_ints_dt, drop_age_starts
)

# test single grouping with no additional id_cols
input_dt <- data.table(
  age_start = seq(0, 95, 5),
  age_end = c(seq(5, 95, 5), Inf),
  value = 1
)
id_cols <- c("age_start", "age_end")

test_missing_intervals(
  description = "missing intervals are identified correctly when no extra id_cols are included",
  input_dt, id_cols, expected_ints_dt, drop_age_starts
)

# Overlapping intervals ---------------------------------------------------

id_cols <- c("age_start", "age_end")

input_dt <- data.table(
  age_start = seq(0, 95, 5),
  age_end = c(seq(5, 95, 5), Inf)
)

testthat::test_that("overlapping intervals are identified correctly", {

  testthat::expect_error(
    assert_no_overlapping_intervals_dt(
      dt = input_dt,
      id_cols = id_cols,
      col_stem = "age",
      identify_all_possible = FALSE
    ),
    NA
  )
  testthat::expect_error(
    assert_no_overlapping_intervals_dt(
      dt = input_dt,
      id_cols = id_cols,
      col_stem = "age",
      identify_all_possible = TRUE
    ),
    NA
  )

  expected_overlapping_dt <- data.table(age_start = c(15), age_end = c(60))
  input_dt <- rbind(input_dt, expected_overlapping_dt)
  setkeyv(input_dt, id_cols)

  overlapping_dt <- identify_overlapping_intervals_dt(
    dt = input_dt,
    id_cols = id_cols,
    col_stem = "age",
    identify_all_possible = FALSE
  )
  setkeyv(expected_overlapping_dt, id_cols)
  testthat::expect_equal(overlapping_dt, expected_overlapping_dt)

  overlapping_dt <- identify_overlapping_intervals_dt(
    dt = input_dt,
    id_cols = id_cols,
    col_stem = "age",
    identify_all_possible = TRUE
  )
  expected_overlapping_dt <- input_dt[age_start >= 15 & age_end <= 60]
  setkeyv(expected_overlapping_dt, id_cols)
  testthat::expect_equal(overlapping_dt, expected_overlapping_dt)

  testthat::expect_error(
    assert_no_overlapping_intervals_dt(
      dt = input_dt,
      id_cols = id_cols,
      col_stem = "age",
      identify_all_possible = FALSE
    ),
    regexp = "There are overlapping intervals"
  )
  testthat::expect_error(
    assert_no_overlapping_intervals_dt(
      dt = input_dt,
      id_cols = id_cols,
      col_stem = "age",
      identify_all_possible = TRUE
    ),
    regexp = "There are overlapping intervals"
  )
})


# Going from name to start and end of interval works ----------------------

names <- c("[-20, 0)", "[-Inf, -20)", "[0, 10)", "[-Inf, Inf)")
expected_result <- data.table(
  start = c(-20, -Inf, 0, -Inf),
  end = c(0, -20, 10, Inf)
)

testthat::test_that("interval name correctly converted to 'start' and 'end'", {
  result <- as.data.table(name_to_start_end(names))
  testthat::expect_identical(result, expected_result)
})
