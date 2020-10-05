context("test-modchi2")

test_that("modchi2 method works with speedglm", {
  x <- matrix(runif(600), nrow = 200, ncol = 3)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
    sapply(
      seq_along(xd[row_id, ]),
      function(element) theta[xd[row_id, element], element]
    )
  })))
  y <- stats::rbinom(200, 1, 1 / (1 + exp(-log_odd)))
  for (test in c(TRUE, FALSE)) {
    for (validation in c(TRUE, FALSE)) {
      for (criterion in c("gini", "aic")) {
        modchi2_modele <- modchi2_iter(x, y, test = test, validation = validation, criterion = criterion)
        expect_s4_class(modchi2_modele, "discretization")
        expect_equal(modchi2_modele@method.name, "modchi2")
        expect_equal(
          modchi2_modele@parameters[-length(modchi2_modele@parameters)],
          list(
            x,
            test,
            validation,
            criterion,
            list(alp = 0.5)
          )
        )
        expect_s3_class(modchi2_modele@best.disc[[1]], "speedglm")
      }
    }
  }
})

test_that("modchi2 method works with glm", {
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
  for (test in c(TRUE, FALSE)) {
    for (validation in c(TRUE, FALSE)) {
      for (criterion in c("gini", "aic")) {
        with_mock(
          "scoringTools:::is_speedglm_installed" = function() FALSE,
          expect_warning(modchi2_modele <- modchi2_iter(x, y, test = test, validation = validation, criterion = criterion))
        )
        expect_s4_class(modchi2_modele, "discretization")
        expect_equal(modchi2_modele@method.name, "modchi2")
        expect_equal(
          modchi2_modele@parameters[-length(modchi2_modele@parameters)],
          list(
            x,
            test,
            validation,
            criterion,
            list(alp = 0.5)
          )
        )
        expect_s3_class(modchi2_modele@best.disc[[1]], "glm")
      }
    }
  }
})


test_that("modchi2 method throws errors", {
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
  expect_error(modchi2_iter(x, y, criterion = "toto"))
  expect_error(modchi2_iter(x, y[1:150], criterion = "gini"))
})
