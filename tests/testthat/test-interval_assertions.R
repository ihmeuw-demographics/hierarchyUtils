
# Missing intervals -------------------------------------------------------

ints_dt <- data.table(
  start = seq(0, 95, 5),
  end = c(seq(5, 95, 5), Inf)
)

expected_ints_dt <- data.table(
  start = 0,
  end = Inf
)

testthat::test_that("missing intervals are identified correctly", {

  missing_dt <- identify_missing_intervals(
    ints_dt[!start %in% c(0, 10, 95)],
    expected_ints_dt
  )
  expected_missing_dt <- data.table(start = c(0, 10, 95), end = c(5, 15, Inf))
  setkeyv(expected_missing_dt, c("start", "end"))
  testthat::expect_equal(missing_dt, expected_missing_dt)

  testthat::expect_error(
    assert_no_missing_intervals(
      ints_dt[!start %in% c(0, 10, 95)],
      expected_ints_dt
    ),
    regexp = "There are missing intervals"
  )

  testthat::expect_silent(
    assert_no_missing_intervals(
      ints_dt,
      expected_ints_dt
    )
  )
})

# Overlapping intervals ---------------------------------------------------

ints_dt <- data.table(
  start = seq(0, 95, 5),
  end = c(seq(5, 95, 5), Inf)
)

testthat::test_that("missing intervals are identified correctly", {


  testthat::expect_silent(
    assert_no_overlapping_intervals(ints_dt)
  )

  expected_overlapping_dt <- data.table(
    start = c(15),
    end = c(60)
  )

  ints_dt <- rbind(ints_dt, expected_overlapping_dt)

  overlapping_dt <- identify_overlapping_intervals(ints_dt)
  setkeyv(expected_overlapping_dt, c("start", "end"))
  testthat::expect_equal(overlapping_dt, expected_overlapping_dt)

  testthat::expect_error(
    assert_no_overlapping_intervals(ints_dt),
    regexp = "There are overlapping intervals"
  )

})
