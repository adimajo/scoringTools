context("test-fuzzy-augmentation")

test_that("fuzzy-augmentation method works with speedglm and glm", {
  xf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
  theta <- c(2, -2)
  log_odd <- apply(xf, 1, function(row) theta %*% row)
  yf <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
  xnf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
  modele_rejected <- fuzzy_augmentation(xf, xnf, yf)
  expect_s4_class(modele_rejected, "reject_infered")
  expect_equal(modele_rejected@method_name, "fuzzy_augmentation")
  expect_s3_class(modele_rejected@financed_model, "glmORlogicalORspeedglm")
  expect_true(is.na(modele_rejected@acceptance_model))
  expect_s3_class(modele_rejected@infered_model, "glmORlogicalORspeedglm")
  with_mock(
    "scoringTools:::is_speedglm_installed" = function() FALSE,
    {
      expect_warning(modele_rejected <- fuzzy_augmentation(xf, xnf, yf))
      expect_s4_class(modele_rejected, "reject_infered")
      expect_equal(modele_rejected@method_name, "fuzzy_augmentation")
      expect_s3_class(modele_rejected@financed_model, "glmORlogicalORspeedglm")
      expect_true(is.na(modele_rejected@acceptance_model))
      expect_s3_class(modele_rejected@infered_model, "glmORlogicalORspeedglm")
    }
  )
})
