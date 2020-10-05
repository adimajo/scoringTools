context("test-consistency")

test_that("consistency errors", {
     expect_error(check_consistency("toto", matrix(0, c(1,1)), matrix(0, 1)))
     expect_error(check_consistency(matrix(0, c(1,1)), matrix(0, c(1,1)), matrix(2, 1)))
     expect_error(check_consistency(matrix(0, c(20,1)), matrix(0, c(20,2)), c(rep(0, 10), rep(0, 10))))
     expect_error(check_consistency(matrix(0, c(20,1)), matrix(0, c(20,1)), c(rep(0, 10), rep(0, 9))))
})
