# #' Financed model
# #'
# #' This function learns a logistic regression on financed clients only
# #' @param df_f data frame of labels and financed clients' data
# #' @return Logistic regression model
# #' @author Adrien Ehrhardt
# #' @seealso \code{glm}, \code{speedglm}
# #' @references
# #' Enea, M. (2015), speedglm: Fitting Linear and Generalized Linear Models to Large Data Sets, \url{https://CRAN.R-project.org/package=speedglm}
# #'
# #' Ehrhardt, A., Biernacki, C., Vandewalle, V., Heinrich, P. and Beben, S. (2018), Reject Inference Methods in Credit Scoring: a rational review,
# #' @examples
# #' # We simulate data from financed clients
# #' xf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
# #' theta <- c(2, -2)
# #' log_odd <- apply(xf, 1, function(row) theta %*% row)
# #' yf <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
# #' df_f <- data.frame(labels = yf, x = xf)
# #' model_finances(df_f)
model_finances <- function(df_f) {
  if (!(is_speedglm_installed() & is_speedglm_predict_installed())) {
    warning("Speedglm not installed, using glm instead (slower).", call. = FALSE)
    model_f <- stats::glm(labels ~ ., family = stats::binomial(link = "logit"), data = df_f)
  } else {
    model_f <- speedglm::speedglm(labels ~ ., family = stats::binomial(link = "logit"), data = df_f)
  }
  class(model_f) <- c("glmORlogicalORspeedglm", class(model_f))
  return(model_f)
}
