#' Fuzzy Augmentation
#'
#' This function performs Reject Inference using the Fuzzy Augmentation technique. Note that this technique has no theoretical foundation and should produce (under the identifiability assumption) the same parameters' estimates than the financed clients scorecard.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @return List containing the model using financed clients only and the model produced using the Fuzzy Augmentation method.
#' @keywords reject inference réintégration scorecard credit scoring
#' @importFrom stats predict
#' @export
#' @author Adrien Ehrhardt
#' @seealso \code{glm}, \code{speedglm}
#' @details
#' This function performs the Fuzzy Augmentation method on the data. When provided with labeled observations \eqn{(x^\ell,y)}, it first fits the logistic regression model \eqn{p_\theta} of
#' \eqn{x^\ell} on \eqn{y}, then labels the unlabelled samples \eqn{x^{u}} with the predicted probabilities of \eqn{p_\theta}, i.e. \eqn{\hat{y}^{u} = p_\theta(y|x^{u})}
#' then refits a logistic regression model \eqn{p_\eta} on the whole sample.
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
#' fuzzy_augmentation(xf, xnf, yf)
fuzzy_augmentation <- function(xf, xnf, yf) {
  check_consistency(xf, xnf, yf)
  df_f <- data.frame(labels = yf, x = xf)
  model_f <- model_finances(df_f)

  df <- rbind(df_f, data.frame(labels = rep(NA, nrow(xnf)), x = xnf))
  df$acc <- NA
  df$acc[1:nrow(df_f)] <- 1
  df$acc[(nrow(df_f) + 1):nrow(df)] <- 0

  df[df$acc == 0, "labels"] <- predict(model_f, df[df$acc == 0, ], type = "response")

  if (!(is_speedglm_installed() & is_speedglm_predict_installed())) {
    model_fuzzy <- stats::glm(labels ~ ., family = stats::binomial(link = "logit"), df[, -which(names(df) %in% c("acc"))])
  } else {
    model_fuzzy <- speedglm::speedglm(labels ~ ., family = stats::binomial(link = "logit"), df[, -which(names(df) %in% c("acc"))])
  }
  class(model_fuzzy) <- c("glmORlogicalORspeedglm", class(model_fuzzy))

  return(methods::new(Class = "reject_infered", method_name = "fuzzy_augmentation", financed_model = model_f, acceptance_model = as.logical(NA), infered_model = model_fuzzy))
}
