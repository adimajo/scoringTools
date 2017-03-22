#' Reclassification
#'
#' This function performs Reject Inference using the Reclassification technique. Note that this technique has no theoretical foundation as it performs a one-step CEM algorithm.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @keywords reject, inference, r?int?gration, scorecard, credit, scoring
#' @import speedglm
#' @export
#' @examples
#' reclassification()

reclassification <- function(xf,xnf,yf,thresh=0.5) {
     df_f <- data.frame(labels = yf, x = xf)
     model_f <- speedglm(labels ~ ., family=binomial(link="logit"), data = df_f)

     df <- rbind(df_f, data.frame(labels = rep(NA,nrow(xnf)), x = xnf))
     df$acc[1:nrow(df_f)] <- 1
     df$acc[(nrow(df_f)+1):nrow(df)] <- 0

     df[df$acc==0,"labels"] <- (predict(model_f,df[df$acc==0,],type="response") > thresh)*1

     model_reclassification = speedglm(labels ~ ., family = binomial(link='logit'), df[,-which(names(df) %in% c("acc"))])

     return(list(financed.model = model_f, reclassification.model = model_reclassification))
}
