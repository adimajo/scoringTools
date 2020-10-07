context("test-methods")

# test_that("runDemo", {
#   runDemo()
# })

test_that("normalizedGini errors", {
  expect_error(normalizedGini(c(0, 1, 0), c(0.1, 0.1)))
})

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

test_that("predict method for reject inference works", {
  xf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
  theta <- c(2, -2)
  log_odd <- apply(xf, 1, function(row) theta %*% row)
  yf <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
  # We simulate data from not financed clients (MCAR mechanism)
  xnf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
  augmented_model <- augmentation(xf, xnf, yf)
  prediction <- predict(object = augmented_model, newdata = xf, type = "response")
  expect_vector(prediction, ptype = double())
  expect_length(prediction, 100)
  expect_true(all(prediction >= 0))
  expect_true(all(prediction <= 1))

  augmented_model <- augmentation(data.frame(xf), data.frame(xnf), yf)
  prediction <- predict(object = augmented_model, newdata = data.frame(xf), type = "response")
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
  expect_warning(plotly_plot <- plot(x = chi2_modele, type = "ROC"))
  chi2_modele <- chi2_iter(x, y, validation = TRUE)
  expect_warning(plotly_plot <- plot(x = chi2_modele, type = "ROC"))
  expect_error(plotly_plot <- plot(x = chi2_modele, type = "lift"))
  expect_error(plotly_plot <- plot(x = chi2_modele, type = "discretization"))
  expect_error(plotly_plot <- plot(x = chi2_modele, type = "glm"))

  with_mock(
    "scoringTools:::is_pROC_installed" = function() FALSE,
    {
      expect_error(plotly_plot <- plot(x = chi2_modele, type = "ROC"))
    }
  )

  with_mock(
    "scoringTools:::is_plotly_installed" = function() FALSE,
    {
      with_mock(
        "scoringTools:::is_pROC_installed" = function() FALSE,
        {
          expect_error(base_plot <- plot(x = chi2_modele, type = "ROC"))
        }
      )
      expect_warning(base_plot <- plot(x = chi2_modele, type = "ROC"))
      chi2_modele <- chi2_iter(x, y)
      expect_warning(base_plot <- plot(x = chi2_modele, type = "ROC"))
      expect_error(base_plot <- plot(x = chi2_modele, type = "discretization"))
      expect_error(base_plot <- plot(x = chi2_modele, type = "lift"))
      with_mock(
        "scoringTools:::is_speedglm_installed" = function() FALSE,
        {
          chi2_modele <- chi2_iter(x, y)
          base_plot <- plot(x = chi2_modele, type = "glm")
        }
      )
    }
  )
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
  expect_true(isGeneric("discretize"))
  discretized_data <- discretize(chi2_modele, x)
  expect_true(ncol(discretized_data) == ncol(x))
  expect_true(nrow(discretized_data) == nrow(x))
})
