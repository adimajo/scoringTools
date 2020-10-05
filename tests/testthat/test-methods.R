context("test-methods")

test_that("show method for reject works", {
  xf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
  theta <- c(2, -2)
  log_odd <- apply(xf, 1, function(row) theta %*% row)
  yf <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
  xnf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
  aug <- augmentation(xf, xnf, yf)
  expect_invisible(show(aug))
})

test_that("show method for discretization works", {
  x <- matrix(runif(300), nrow = 100, ncol = 3)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
    sapply(
      seq_along(xd[row_id, ]),
      function(element) theta[xd[row_id, element], element]
    )
  })))
  y <- stats::rbinom(100, 1, 1 / (1 + exp(-log_odd)))
  chi2_modele <- chi2_iter(x, y)
  expect_invisible(show(chi2_modele))
  expect_invisible(print(chi2_modele))
  expect_visible(summary(chi2_modele))
})

test_that("predict method for discretization works", {
  x <- matrix(runif(300), nrow = 100, ncol = 3)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
    sapply(
      seq_along(xd[row_id, ]),
      function(element) theta[xd[row_id, element], element]
    )
  })))
  y <- stats::rbinom(100, 1, 1 / (1 + exp(-log_odd)))
  chi2_modele <- chi2_iter(x, y)
  prediction <- predict(object = chi2_modele, newdata = data.frame(x))
  expect_vector(prediction, ptype = double())
  expect_length(prediction, 100)
  expect_true(all(prediction >= 0))
  expect_true(all(prediction <= 1))
})

test_that("plot method for discretization works", {
  x <- matrix(runif(300), nrow = 100, ncol = 3)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
    sapply(
      seq_along(xd[row_id, ]),
      function(element) theta[xd[row_id, element], element]
    )
  })))
  y <- stats::rbinom(100, 1, 1 / (1 + exp(-log_odd)))
  chi2_modele <- chi2_iter(x, y)
  plotly_plot <- plot(x = chi2_modele, type = "ROC")
})

test_that("discretize method for discretization works", {
  x <- matrix(runif(300), nrow = 100, ncol = 3)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
    sapply(
      seq_along(xd[row_id, ]),
      function(element) theta[xd[row_id, element], element]
    )
  })))
  y <- stats::rbinom(100, 1, 1 / (1 + exp(-log_odd)))
  chi2_modele <- chi2_iter(x, y)
  discretized_data <- discretize(chi2_modele, x)
  expect_true(ncol(discretized_data) == ncol(x))
  expect_true(nrow(discretized_data) == nrow(x))
})
