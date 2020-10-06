#' Reclassification
#'
#' This function performs Reject Inference using the Reclassification technique. Note that this technique has no theoretical foundation as it performs a one-step CEM algorithm.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @param thresh The threshold to use in the Classification step, i.e. the probability above which a not financed client is considered to have a label equal to 1.
#' @return List containing the model using financed clients only and the model produced using the Reclassification method.
#' @keywords reject inference réintégration scorecard credit scoring
#' @importFrom stats predict
#' @export
#' @author Adrien Ehrhardt
#' @seealso \code{glm}, \code{speedglm}
#' @details
#' This function performs the Reclassification method on the data. When provided with labeled observations \eqn{(x^\ell,y)}, it first fits the logistic regression model \eqn{p_\theta} of
#' \eqn{x^\ell} on \eqn{y}, then considers that unlabeled observations are of the expected class given by the model \eqn{p_\theta} (this is equivalent to a CEM algorithm).
#' It then refits a logistic regression model \eqn{p_\eta} on the whole sample.
#' @references
#' Enea, M. (2015), speedglm: Fitting Linear and Generalized Linear Models to Large Data Sets, \url{https://CRAN.R-project.org/package=speedglm}
#' Ehrhardt, A., Biernacki, C., Vandewalle, V., Heinrich, P. and Beben, S. (2018), Reject Inference Methods in Credit Scoring: a rational review,
#' @examples
#' # We simulate data from financed clients
#' xf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
#' theta <- c(2, -2)
#' log_odd <- apply(xf, 1, function(row) theta %*% row)
#' yf <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
#' # We simulate data from not financed clients (MCAR mechanism)
#' xnf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
#' reclassification(xf, xnf, yf)
reclassification <- function(xf, xnf, yf, thresh = 0.5) {
  check_consistency(xf, xnf, yf)
  df_f <- data.frame(labels = yf, x = xf)
  model_f <- model_finances(df_f)

  df <- rbind(df_f, data.frame(labels = rep(NA, nrow(xnf)), x = xnf))
  df$acc <- NA
  df$acc[1:nrow(df_f)] <- 1
  df$acc[(nrow(df_f) + 1):nrow(df)] <- 0

  df[df$acc == 0, "labels"] <- (predict(model_f, df[df$acc == 0, ], type = "response") > thresh) * 1

  if (!(is_speedglm_installed() & is_speedglm_predict_installed())) {
    model_reclassification <- stats::glm(labels ~ ., family = stats::binomial(link = "logit"), df[, -which(names(df) %in% c("acc"))])
  } else {
    model_reclassification <- speedglm::speedglm(labels ~ ., family = stats::binomial(link = "logit"), df[, -which(names(df) %in% c("acc"))])
  }
  class(model_reclassification) <- c("glmORlogicalORspeedglm", class(model_reclassification))

  return(methods::new(Class = "reject_infered", method_name = "reclassification", financed_model = model_f, acceptance_model = as.logical(NA), infered_model = model_reclassification))
}
