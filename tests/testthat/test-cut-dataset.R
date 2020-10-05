context("test-cut-dataset")

test_that("cut_dataset works with only validation", {
  test_de_cut <- cut_dataset(20, c(0.1), test = F, validation = T)
  expect_type(test_de_cut[[1]], "integer")
  expect_type(test_de_cut[[2]], "integer")
  expect_null(test_de_cut[[3]])
})

test_that("cut_dataset works with only test", {
  test_de_cut <- cut_dataset(20, c(0.1), test = T, validation = F)
  expect_type(test_de_cut[[1]], "integer")
  expect_type(test_de_cut[[2]], "integer")
  expect_null(test_de_cut[[3]])
})

test_that("cut_dataset errors with test and validation and only one proportion", {
  expect_error(cut_dataset(20, c(0.1), test = T, validation = T))
})

test_that("cut_dataset errors with test or validation and more than one proportion", {
  expect_error(cut_dataset(20, proportions = c(), test = T, validation = F))
  expect_error(cut_dataset(20, proportions = c(), test = F, validation = T))
  expect_error(cut_dataset(20, c(-0.1), test = T, validation = F))
  expect_error(cut_dataset(20, c(-0.1), test = F, validation = T))
  expect_error(cut_dataset(20, c(1.1), test = T, validation = F))
  expect_error(cut_dataset(20, c(1.1), test = F, validation = T))
})

test_that("cut_dataset works with test and validation", {
  test_de_cut <- cut_dataset(20, c(0.1, 0.1), test = T, validation = T)
  expect_type(test_de_cut[[1]], "integer")
  expect_type(test_de_cut[[2]], "integer")
  expect_type(test_de_cut[[3]], "integer")
})

test_that("cut_dataset works with no test and no validation", {
  test_de_cut1 <- cut_dataset(20, c(0.1, 0.1), test = F, validation = F)
  test_de_cut2 <- cut_dataset(20, c(0.1), test = F, validation = F)
  test_de_cut3 <- cut_dataset(20, test = F, validation = F)
  expect_type(test_de_cut1[[1]], "integer")
  expect_type(test_de_cut2[[1]], "integer")
  expect_type(test_de_cut3[[1]], "integer")
  expect_null(test_de_cut1[[2]])
  expect_null(test_de_cut2[[2]])
  expect_null(test_de_cut3[[2]])
  expect_null(test_de_cut1[[3]])
  expect_null(test_de_cut2[[3]])
  expect_null(test_de_cut3[[3]])
})
