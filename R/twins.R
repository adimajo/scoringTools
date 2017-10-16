#' Twins
#'
#' This function performs Reject Inference using the Twins technique. Note that this technique has no theoretical foundation.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @return List containing the model using financed clients only, the model of acceptance and the model produced using the Twins method.
#' @keywords reject, inference, r?int?gration, scorecard, credit, scoring
#' @importFrom stats predict
#' @export
#' @author Adrien Ehrhardt
#' @seealso \code{glm}, \code{speedglm}
#' @details
#' This function performs the Twins method on the data. When provided with labeled observations \eqn{(x^\ell,y)}, it first fits the logistic regression model \eqn{p_\theta} of
#' \eqn{x^\ell} on \eqn{y}, then fits the logistic regression model \eqn{p_\omega} of \eqn{X} on the binomial random variable denoting the observation of the data \eqn{Z}.
#' We use predictions of both models on the labeled observations to construct a "meta"-score based on logistic regression which predicted probabilities are used to reweight samples and construct the final score \eqn{p_\eta}.
#' @references
#' Enea, M. (2015), speedglm: Fitting Linear and Generalized Linear Models to Large Data Sets, \url{https://CRAN.R-project.org/package=speedglm}
#' Ehrhardt, A., Biernacki, C., Vandewalle, V., Heinrich, P. and Beben, S. (2018), Reject Inference Methods in Credit Scoring: a rational review,
#' @examples
#' # We simulate data from financed clients
#' set.seed(1)
#' xf = matrix(runif(100*2), nrow = 100, ncol = 2)
#' theta = c(2,-2)
#' log_odd = apply(xf, 1, function(row) theta%*%row)
#' yf = rbinom(100,1,1/(1+exp(-log_odd)))
#' # We simulate data from not financed clients (MCAR mechanism)
#' xnf = matrix(runif(100*2), nrow = 100, ncol = 2)
#' list_models <- twins(xf,xnf,yf)
#' # This is the model constructed using the financed clients (xf,yf):
#' list_models[1]
#' # This is the model on the acceptance process:
#' list_models[2]
#' # This is the model constructed using the twins method:
#' list_models[3]

twins <- function(xf,xnf,yf) {
     df_f <- data.frame(labels = yf, x = xf)
     if (!requireNamespace("speedglm", quietly = TRUE)) {
          warning("Speedglm not installed, using glm instead (slower).",call. = FALSE)
          model_f <- stats::glm(labels ~ ., family=stats::binomial(link="logit"), data = df_f)
     } else {
          model_f <- speedglm::speedglm(labels ~ ., family=stats::binomial(link="logit"), data = df_f)
     }
     df <- rbind(df_f, data.frame(labels = rep(NA,nrow(xnf)), x = xnf))
     df$acc[1:nrow(df_f)] <- 1
     df$acc[(nrow(df_f)+1):nrow(df)] <- 0

     if (!requireNamespace("speedglm", quietly = TRUE)) {
          model_acc <- stats::glm(acc ~ ., family = stats::binomial(link='logit'), df[,-which(names(df) %in% c("labels"))])
     } else {
          model_acc <- speedglm::speedglm(acc ~ ., family = stats::binomial(link='logit'), df[,-which(names(df) %in% c("labels"))])
     }
     df$score_acc <- predict(model_acc,df)
     df$score_def <- predict(model_f,df)
     if (!requireNamespace("speedglm", quietly = TRUE)) {
          model_twins = stats::glm(labels ~ score_acc + score_def, family = stats::binomial(link='logit'), df[df$acc==1,-which(names(df) %in% c("acc"))])
     } else {
          model_twins = speedglm::speedglm(labels ~ score_acc + score_def, family = stats::binomial(link='logit'), df[df$acc==1,-which(names(df) %in% c("acc"))])
     }
     # return(list(financed.model = model_f, acceptation.model = model_acc, twins.model = model_twins))
     return(methods::new(Class = "reject_infered", method_name = "twins", financed_model = model_f, acceptance_model = model_acc, infered_model = model_twins))

}
