#' Reclassification
#'
#' This function performs Reject Inference using the Reclassification technique. Note that this technique has no theoretical foundation as it performs a one-step CEM algorithm.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @param thresh The threshold to use in the Classification step, i.e. the probability above which a not financed client is considered to have a label equal to 1.
#' @return List containing the model using financed clients only and the model produced using the Reclassification method.
#' @keywords reject, inference, r?int?gration, scorecard, credit, scoring
#' @importFrom stats predict
#' @export
#' @examples
#' # We simulate data from financed clients
#' set.seed(1)
#' xf = matrix(runif(100*2), nrow = 100, ncol = 2)
#' theta = c(2,-2)
#' log_odd = apply(xf, 1, function(row) theta%*%row)
#' yf = rbinom(100,1,1/(1+exp(-log_odd)))
#' # We simulate data from not financed clients (MCAR mechanism)
#' xnf = matrix(runif(100*2), nrow = 100, ncol = 2)
#' list_models <- reclassification(xf,xnf,yf)
#' # This is the model constructed using the financed clients (xf,yf):
#' list_models[1]
#' # This is the model constructed using all the clients (xf,yf,xnf)
#' # and the reclassification technique:
#' list_models[2]

reclassification <- function(xf,xnf,yf,thresh=0.5) {
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

     df[df$acc==0,"labels"] <- (predict(model_f,df[df$acc==0,],type="response") > thresh)*1

     if (!requireNamespace("speedglm", quietly = TRUE)) {
          model_reclassification = stats::glm(labels ~ ., family = stats::binomial(link='logit'), df[,-which(names(df) %in% c("acc"))])
     } else {
          model_reclassification = speedglm::speedglm(labels ~ ., family = stats::binomial(link='logit'), df[,-which(names(df) %in% c("acc"))])
     }
     return(list(financed.model = model_f, reclassification.model = model_reclassification))
}
