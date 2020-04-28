
testthat::test_that("Test that `logit` returns expected value", {
  testthat::expect_equal(logit(0.2), log(0.2/0.8))
})

testthat::test_that("Test that `logit` and `invlogit` inverse works", {

  # standard
  testthat::expect_equal(invlogit(logit(0.1)), 0.1)

  # domain shift
  x <- runif(n = 5, min = 10, max = 20)
  logit_x <- logit(x, domain_lower = 10, domain_upper = 20)
  x_again <- invlogit(logit_x, domain_lower = 10, domain_upper = 20)
  testthat::expect_equal(x, x_again)

})

testthat::test_that("Test that `logit` works with vector input", {

  # standard
  testthat::expect_equal(invlogit(logit(x = c(0.1, 0.2))), c(0.1, 0.2))

  # domain shift
  x <- c(runif(n = 5, min = 10, max = 20), runif(n = 5, min = 20, max = 30))
  domain_lower = c(rep(10, 5), rep(20, 5))
  domain_upper = c(rep(20, 5), rep(30, 5))
  logit_x <- logit(x, domain_lower, domain_upper)
  x_again <- invlogit(logit_x, domain_lower, domain_upper)
  testthat::expect_equal(x, x_again)

})
