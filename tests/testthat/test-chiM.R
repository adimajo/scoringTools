context("test-chiM")

test_that("chiM method works with speedglm and glm", {
  x <- matrix(runif(140), nrow = 70, ncol = 2)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 2, 2, -2, -2), ncol = 3, nrow = 2))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
    sapply(
      seq_along(xd[row_id, ]),
      function(element) theta[xd[row_id, element], element]
    )
  })))
  y <- stats::rbinom(70, 1, 1 / (1 + exp(-log_odd)))
  for (test in c(TRUE, FALSE)) {
    for (validation in c(TRUE, FALSE)) {
      for (criterion in c("gini", "aic")) {
        chiM_modele <- chiM_iter(x, y, test = test, validation = validation, criterion = criterion, param = list(alpha = 0.01))
        expect_s4_class(chiM_modele, "discretization")
        expect_equal(chiM_modele@method.name, "chiM")
        expect_equal(
          chiM_modele@parameters[-length(chiM_modele@parameters)],
          list(
            x,
            test,
            validation,
            criterion,
            list(alpha = 0.01)
          )
        )
        if (is_speedglm_installed() & is_speedglm_predict_installed()) {
          expect_s3_class(chiM_modele@best.disc[[1]], "speedglm")
        }

        with_mock(
          "scoringTools:::is_speedglm_installed" = function() FALSE,
          {
            expect_warning(chiM_modele <- chiM_iter(x, y, test = test, validation = validation, criterion = criterion))
            expect_s4_class(chiM_modele, "discretization")
            expect_equal(chiM_modele@method.name, "chiM")
            expect_equal(
              chiM_modele@parameters[-length(chiM_modele@parameters)],
              list(
                x,
                test,
                validation,
                criterion,
                list(alpha = 0.05)
              )
            )
            expect_s3_class(chiM_modele@best.disc[[1]], "glm")
          }
        )
      }
    }
  }
})

test_that("chiM method throws errors", {
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
  expect_error(chiM_iter(x, y, criterion = "toto"))
  expect_error(chiM_iter(x, y[1:12], criterion = "gini"))
})
