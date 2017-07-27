#' Fuzzy Augmentation
#'
#' This function performs Reject Inference using the Fuzzy Augmentation technique. Note that this technique has no theoretical foundation and should produce (under the identifiability assumption) the same parameters' estimates than the financed clients scorecard.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @return List containing the model using financed clients only and the model produced using the Fuzzy Augmentation method.
#' @keywords reject, inference, r?int?gration, scorecard, credit, scoring
#' @import speedglm
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
#' list_models <- fuzzy_augmentation(xf,xnf,yf)
#' # This is the model constructed using the financed clients (xf,yf):
#' list_models[1]
#' # This is the model constructed using all the clients (xf,yf,xnf)
#' # and the fuzzy augmentation technique:
#' list_models[2]

fuzzy_augmentation <- function(xf,xnf,yf) {
     df_f <- data.frame(labels = yf, x = xf)
     model_f <- speedglm::speedglm(labels ~ ., family=stats::binomial(link="logit"), data = df_f)

     df <- rbind(df_f, data.frame(labels = rep(NA,nrow(xnf)), x = xnf))
     df$acc[1:nrow(df_f)] <- 1
     df$acc[(nrow(df_f)+1):nrow(df)] <- 0

     df[df$acc==0,"labels"] <- speedglm:::predict.speedglm(model_f,df[df$acc==0,],type="response")

     model_fuzzy = speedglm::speedglm(labels ~ ., family = stats::binomial(link='logit'), df[,-which(names(df) %in% c("acc"))])

     return(list(financed.model = model_f, fuzzy_augmentation.model = model_fuzzy))
}
