
testthat::test_that("Test that `logit` returns expected value", {
  testthat::expect_equal(logit(0.2), log(0.2/0.8))
})

testthat::test_that("Test that `logit` and `invlogit` inverse works", {

  # standard
  testthat::expect_equal(invlogit(logit(0.1)), 0.1)

  # scaled
  x <- c(10, 20, 30)
  logit_x <- logit(x, scale = 0.01)
  x_again <- invlogit(logit_x, scale = 0.01)
  testthat::expect_equal(x, x_again)

  # domain shift
  x <- runif(n = 100, min = 10, max = 20)
  logit_x <- logit(x, domain_lower = 10, domain_upper = 20)
  x_again <- invlogit(logit_x, domain_lower = 10, domain_upper = 20)
  testthat::expect_equal(x, x_again)

})
