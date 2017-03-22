#' Twins
#'
#' This function performs Reject Inference using the Twins technique. Note that this technique has no theoretical foundation.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @keywords reject, inference, r?int?gration, scorecard, credit, scoring
#' @import speedglm
#' @export
#' @examples
#' twins()

twins <- function(xf,xnf,yf) {
     df_f <- data.frame(labels = yf, x = xf)
     model_f <- speedglm(labels ~ ., family=binomial(link="logit"), data = df_f)

     df <- rbind(df_f, data.frame(labels = rep(NA,nrow(xnf)), x = xnf))
     df$acc[1:nrow(df_f)] <- 1
     df$acc[(nrow(df_f)+1):nrow(df)] <- 0

     model_acc <- speedglm(acc ~ ., family = binomial(link='logit'), df[,-which(names(df) %in% c("labels"))])

     df$score_acc <- predict(model_acc,df)
     df$score_def <- predict(model_f,df)

     model_twins = speedglm(labels ~ score_acc + score_def, family = binomial(link='logit'), df[df$acc==1,-which(names(df) %in% c("acc"))])

     return(list(financed.model = model_f, acceptation.model = model_acc, twins.model = model_twins))
}
