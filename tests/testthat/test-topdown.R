context("test-topdown")

test_that("topdown method works with speedglm", {
  x <- matrix(runif(600), nrow = 300, ncol = 2)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 2, 2, -2, -2), ncol = 3, nrow = 2))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
    sapply(
      seq_along(xd[row_id, ]),
      function(element) theta[xd[row_id, element], element]
    )
  })))
  y <- stats::rbinom(300, 1, 1 / (1 + exp(-log_odd)))
  for (test in c(TRUE, FALSE)) {
    for (validation in c(TRUE, FALSE)) {
      for (criterion in c("gini", "aic")) {
        topdown_modele <- topdown_iter(x, y, test = test, validation = validation, criterion = criterion)
        expect_s4_class(topdown_modele, "discretization")
        expect_equal(topdown_modele@method.name, "topdown")
        expect_equal(
          topdown_modele@parameters[-length(topdown_modele@parameters)],
          list(
            x,
            test,
            validation,
            criterion,
            list(1, 2, 3)
          )
        )
        if (is_speedglm_installed() & is_speedglm_predict_installed()) {
          expect_s3_class(topdown_modele@best.disc[[1]], "speedglm")
        }
      }
    }
  }
})

# test_that("topdown method works with glm", {
#      x <- matrix(runif(600), nrow = 200, ncol = 3)
#      cuts <- seq(0, 1, length.out = 4)
#      xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
#      theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
#      log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
#           sapply(
#                seq_along(xd[row_id, ]),
#                function(element) theta[xd[row_id, element], element]
#           )
#      })))
#      y <- stats::rbinom(200, 1, 1 / (1 + exp(-log_odd)))
#      for (test in c(TRUE, FALSE)) {
#           for (validation in c(TRUE, FALSE)) {
#                for (criterion in c("gini", "aic")) {
#                        with_mock(
#                                "scoringTools:::is_speedglm_installed" = function() FALSE,
#                                expect_warning(topdown_modele <- topdown_iter(x, y, test = test, validation = validation, criterion = criterion))
#                        )
#                     expect_s4_class(topdown_modele, "discretization")
#                     expect_equal(topdown_modele@method.name, "topdown")
#                     expect_equal(topdown_modele@parameters[-length(topdown_modele@parameters)],
#                                  list(x,
#                                       test,
#                                       validation,
#                                       criterion,
#                                       list(1, 2, 3)))
#                     expect_s3_class(topdown_modele@best.disc[[1]], "glm")
#                }
#           }
#      }
# }
# )


test_that("topdown method throws errors", {
  x <- matrix(runif(50), nrow = 25, ncol = 2)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 2, 2, -2, -2), ncol = 3, nrow = 2))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
    sapply(
      seq_along(xd[row_id, ]),
      function(element) theta[xd[row_id, element], element]
    )
  })))
  y <- stats::rbinom(25, 1, 1 / (1 + exp(-log_odd)))
  expect_error(topdown_iter(x, y, criterion = "toto"))
  expect_error(topdown_iter(x, y[1:12], criterion = "gini"))
})
